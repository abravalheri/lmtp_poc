%%%-------------------------------------------------------------------
%% @doc A simple LMTP server that writes the received emails in the file system
%% based on gen_smtp's smtp_server_example.
%%
%% A `MAIL_DIR' environment variable should be provided.
%% Emails will be saved inside the directory `MAIL_DIR/<email address>'.
%% If the directory does not exist, then the LMTP server will not accept the
%% incoming email.
%%
%% A list of senders can be blocked by writing their email addresses in the
%% `MAIL_DIR/blocked.txt' file (one per line)
%% @end
%%%-------------------------------------------------------------------
-module(lmtp_server).

-define(BLOCKED_LIST, "blocked.txt").
-define(EMAIL_EXT, ".eml").

-behaviour(gen_smtp_server_session).

-include_lib("hut/include/hut.hrl").  % Used for logging

-ifdef(TEST).
% useful for debugging
-include_lib("eunit/include/eunit.hrl").
-endif.

% Types
-record(state, {options = [] :: list(), blocked = undefined :: undefined | string()}).

-type delivery_status() :: {ok | error, iodata()}.
%% @doc Unique name used by `ranch' to refer to a socket listener
%% (can be a `any()').
-type ref() :: ranch:ref().

-export_type([delivery_status/0]).

% -- Public API
% Standalone mode
-export([start/0, start/1, start/2]).
-export([stop/0, stop/1]).
% Embedded mode
-export([child_spec/0, child_spec/1, child_spec/2]).
% gen_smtp_server_session callbacks
-export([init/4]).
-export([handle_HELO/2, handle_EHLO/3, handle_LHLO/3]).
-export([handle_MAIL/2, handle_MAIL_extension/2]).
-export([handle_RCPT/2, handle_RCPT_extension/2]).
-export([handle_DATA/4]).
-export([handle_RSET/1]).
-export([handle_VRFY/2]).
-export([handle_EXPN/2]).
-export([handle_STARTTLS/1]).
-export([handle_other/3]).
-export([code_change/3]).
-export([terminate/2]).

%-------------------------------------------------------------------
% Public API
%-------------------------------------------------------------------

%% @doc Start a supervision tree with listeners for the LMTP protocol
-spec start() -> {ok, pid()} | {error, any()}.
start() ->
    start(?MODULE, []).

-spec start(gen_smtp_server:options()) -> {ok, pid()} | {error, any()}.
start(Options) ->
    start(?MODULE, Options).

-spec start(ref(), gen_smtp_server:options()) -> {ok, pid()} | {error, any()}.
start(ServerName, Options) ->
    gen_smtp_server:start(ServerName, ?MODULE, lmtp_options(Options)).

%% @doc Stop listening to the LMTP protocol
-spec stop() -> ok | {error, any()}.
stop() ->
    stop(?MODULE).

-spec stop(ref()) -> ok | {error, any()}.
stop(ServerName) ->
    gen_smtp_server:stop(ServerName).

%% @doc Create a `child_spec' derived from `gen_smtp_server' that can be used
%% with a supervisor
-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    child_spec(?MODULE, []).

-spec child_spec(gen_smtp_server:options()) -> supervisor:child_spec().
child_spec(Options) when is_list(Options) ->
    child_spec(?MODULE, Options).

-spec child_spec(ref(), gen_smtp_server:options()) -> supervisor:child_spec().
child_spec(ServerName, Options) ->
    gen_smtp_server:child_spec(ServerName, ?MODULE, lmtp_options(Options)).

%-------------------------------------------------------------------
% Implementation of `gen_smtp_server_session' callbacks
%-------------------------------------------------------------------

%% @doc the `init/4' callback can be used for a few different purposes, to
%% mention a few:
%%
%% 1. Allow a custom "banner" to be specified. The banner is used as a opening
%%    message sent when the server accepts a connection.
%% 2. Monitor the maximum number of process handling incoming connections
%%    (via the second argument - `SessionCount').
%%    For example, an implementation of `init/4' might choose to return
%%    `{stop, normal, "421 Server too busy"}' when the number of process
%%    spawned is higher than a certain number.
%%    Please notice that the "hard limit" of process being spawned is
%%    controlled by passing `ranch_opts' to `gen_smtp_server'. Returning
%%    `{stop, ...}' from `init/4' means that a new process will be spawned
%%    (if the maximum number defined in `ranch_opts' was still not reached)
%%    just to send an error message to the connecting client.
%%    This gives the opportunity to the client to try later and avoids
%%    overwhelming the server.
%% 3. Storing the session options given in the third argument (which allows
%%    parameterisation of the implemented behaviour).
%% 4. Awareness about the network address of the client establishing the
%%    connection (third argument) and the server's own address/hostname (first
%%    argument).
-spec init(Hostname :: inet:hostname(),
           SessionCount :: non_neg_integer(),
           Address :: inet:ip_address(),
           Options :: list()) ->
              {ok, iodata(), #state{}} | {stop, any(), iodata()}.
init(Hostname, SessionCount, Address, Options) ->
    ?log(info, "peer: ~p~n", [Address]),
    case SessionCount > 20 of
        false ->
            Banner = [Hostname, " LMTP proof of concept server"],
            State = #state{options = Options},
            {ok, Banner, State};
        true ->
            ?log(warning, "Connection limit exceeded~n"),
            {stop, normal, ["421 ", Hostname, " is too busy to accept mail right now"]}
    end.

%% @hidden This implementation is required by the `gen_smtp_server_session'
%% behaviour, ideally `gen_smtp' should provide an alternative behaviour for
%% LMTP that does not require this callback.
handle_HELO(_, State) ->
    {error, "500 LTMP server not SMTP", State}.

%% @hidden Currently it is used for LMTP, but the semantics are distorted to
%% reply to `LHLO' commands. We explicitly implement `handle_LHLO' for clarity.
handle_EHLO(Hostname, Extensions, State) ->
    handle_LHLO(Hostname, Extensions, State).

%% @doc This callback can be used to signal to the connecting client which
%% extensions to the LMTP protocol are supported by the server.
%% The second argument (`Extensions') is a list of extensions supported by
%% default by `gen_smtp', so the implementation can decide to just use them by
%% returning `{ok, Extension, State}', or manipulate the list to remove or add
%% extensions. Not that added extensions require custom implementation.
handle_LHLO(_Hostname, Extensions, State) ->
    {ok, Extensions, State}.

%% @doc This callback can be used to accept or reject specific senders.
handle_MAIL(From, #state{blocked = undefined} = State) ->
    BlockedFile = filename:join(mail_dir(), ?BLOCKED_LIST),
    Blocked =
        case file:read_file(BlockedFile) of
            {ok, Contents} ->
                unicode:characters_to_list(Contents);
            {error, Reason} ->
                ?log(info, "File ~p not found, no sender will be blocked (~p)~n", [BlockedFile, Reason]),
                ""
        end,
    handle_MAIL(From, State#state{blocked = Blocked});
handle_MAIL(From, #state{blocked = Blocked} = State) ->
    case string:find(Blocked, From) of
        nomatch ->
            {ok, State};
        _ ->
            {error, "552 Go Away", State}
    end.

%% @hidden No extension implemented
handle_MAIL_extension(Extension, _State) ->
    ?log(warning, "Unknown MAIL FROM extension ~s~n", [Extension]),
    error.

%% @doc This callback can be used to signal to the client which recipients the
%% server can deliver emails to.
handle_RCPT(To, State) ->
    case filelib:is_dir(mail_dir(To)) of
        true ->
            {ok, State};
        false ->
            ?log(info, "No RCPT ~n", [To]),
            {error, "550 No such recipient", State}
    end.

%% @hidden No extension implemented
handle_RCPT_extension(Extension, _State) ->
    ?log(warning, "Unknown RCPT TO extension ~s~n", [Extension]),
    error.

%% @doc This callback is responsible for actually handling the email (the email
%% body is given as the third argument, `Data'), by storing it somewhere (e.g.
%% database or file system) and notifying the user or automatically processing
%% the email. The sky is the limit.
%% Note that the client will expect a reply with a delivery status for each
%% recipient accepted (the ones the server returned `ok' on `handle_RCPT').
%% If the returned delivery status is `ok' then it is the responsibility of the
%% LMTP server to make sure the user receives the email (or it is automatically
%% processed) - the client will stop retrying to send the same email again and
%% will assume it was delivered.
-spec handle_DATA(From :: binary(),
                  To :: [binary(), ...],
                  Data :: binary(),
                  State :: #state{}) ->
                     {multiple, [delivery_status()], #state{}}.
handle_DATA(From, To, Data, State) ->
    Length = byte_size(Data),
    ?log(info, "message from ~s delivered to ~p, body length ~p~n", [From, To, Length]),
    Multiple = [store_mail(reference(), From, Recipient, Data) || Recipient <- To],
    {multiple, Multiple, State}.

%% @doc Reset the state of the process.
handle_RSET(State) ->
    State#state{blocked = undefined}.

%% @hidden Both VRFY and EXPN can be exploited to find out all the existing
%% users
handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled", State}.

%% @hidden Similarly to VRFY, EXPN can be exploited by malicious clients
handle_EXPN(_Address, State) ->
    {error, "252 EXPN disabled", State}.

%% @hidden The docs for smtp_server_example clearly state that this callback is
%% optional, but the callback itself is not marked as optional in the behaviour
%% definition.
handle_STARTTLS(State) ->
    ?log(debug, "LMTP TLS started"),
    State.

%% @hidden Can be used to handle other LMTP/SMTP commands
-spec handle_other(binary(), binary(), #state{}) -> {string(), #state{}}.
handle_other(Verb, _Args, State) ->
    % You can implement other SMTP verbs here, if you need to
    {["500 Error: command not recognized : '", Verb, "'"], State}.

%% @hidden No old versions so we don't need to worry about that.
-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @hidden Let's just terminate if asked to do so
-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(Reason, State) ->
    {ok, Reason, State}.

%-------------------------------------------------------------------
% Auxiliary functions
%-------------------------------------------------------------------

%% @hidden
%% Stores the email in the directory given by `mail_dir/1'
-spec store_mail(string(), binary(), binary(), iodata()) -> delivery_status().
store_mail(Reference, From, To, Data) ->
    File = filename:join(mail_dir(To), Reference ++ ?EMAIL_EXT),
    case file:write_file(File, Data) of
        ok ->
            ?log(info, "Message from ~s delivered to ~p (~s)~n", [From, To, File]),
            {ok, ["delivered to ", To]};
        {error, Reason} ->
            ?log(error, "Problems in writing the file ~s (~p)~n", [File, Reason]),
            {error, ["554 Error when delivering to ", To]}
    end.

%% @hidden
%% Creates a unique reference string that can be used as id to store the
%% email
-spec reference() -> string().
reference() ->
    UniqueId = erlang:unique_integer(),
    lists:flatten([io_lib:format("~2.16.0b", [X])
                   || <<X>> <= erlang:md5(term_to_binary(UniqueId))]).

%% @hidden
%% Returns the path to the directory used by the mail server to store
%% email and configuration.
%% If the environment variable `MAIL_DIR' is given, this is the value returned,
%% otherwise the function falls back to the current working directory.
mail_dir() ->
    {ok, Cwd} = file:get_cwd(),
    os:getenv("MAIL_DIR", Cwd).

%% @hidden
%% Like `mail_dir/0' but relative to an specific `Recipient'.
mail_dir(Recipient) ->
    filename:join(mail_dir(), Recipient).

%% @hidden
%% Ensure 'protocol' inside 'sessionoptions' is 'lmtp'
lmtp_options(Options) ->
    SessionOpts = proplists:get_value(sessionoptions, Options, []),
    SessionOpts1 = proplists:delete(protocol, SessionOpts),
    SessionOpts2 = [{protocol, lmtp} | SessionOpts1],
    Opts = proplists:delete(sessionoptions, Options),
    [{sessionoptions, SessionOpts2} | Opts].
