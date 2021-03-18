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
-module(lmtp_server).

%% @doc Simple text file with email addresses to block (one per line)
-define(BLOCKED_LIST, "blocked.txt").

% -behaviour(gen_smtp_server_session).

% -export([start_link/0]).
% -export([init/1]).

-include_lib("hut/include/hut.hrl").

-record(state, {options = [] :: list(), blocked = nil :: none() | string()}).

-type delivery_status() :: {ok | error, string()}.

init([]) ->
    SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

% -define(SERVER, ?MODULE).

% start_link() ->
%     supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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

handle_HELO(_, State) ->
    {error, "500 LTMP server not SMTP", State}.

handle_LHLO(Hostname, Extensions, State) ->
    {ok, Extensions, State}.

handle_EHLO(Hostname, Extensions, State) ->
    handle_LHLO(Hostname, Extensions, State).

handle_MAIL(From, #state{blocked = nil} = State) ->
    BlockedFile = filename:join(mail_dir(), ?BLOCKED_LIST),
    Blocked =
        case file:read(BlockedFile) of
            {ok, Contents} ->
                Contents;
            {error, Reason} ->
                ?log(info, "File ~p not found, no sender will be blocked~n", [BlockedFile]),
                <<"">>
        end,
    handle_MAIL(From, State#state{blocked = Blocked});
handle_MAIL(From, #state{blocked = Blocked} = State) ->
    case string:find(Blocked, From) of
        nomatch ->
            {ok, State};
        _ ->
            {error, "552 Go Away", State}
    end.

handle_MAIL_extension(Extension, _State) ->
    ?log(warning, "Unknown MAIL FROM extension ~s~n", [Extension]),
    error.

handle_RCPT(To, State) ->
    case file:is_dir(mail_dir(To)) of
        true ->
            {ok, State};
        false ->
            ?log(info, "No RCPT ~n", [To]),
            {error, "550 No such recipient", State}
    end.

-spec handle_DATA(From :: string(),
                  To :: [string(), ...],
                  Data :: string(),
                  State :: #state{}) ->
                     {ok | error, string(), #state{}} | {multiple, [delivery_status()], #state{}}.
handle_DATA(From, To, Data, State) ->
    Length = byte_size(Data),
    ?log(info, "message from ~s delivered to ~p, body length ~p~n", [From, To, Length]),
    Multiple = [store_mail(reference(), From, Recipient, Data) || Recipient <- To],
    {multiple, Multiple, State}.

-spec store_mail(string(), string(), string(), string()) -> ok.
store_mail(Reference, From, To, Data) ->
    File = filename:join(mail_dir(To), Reference),
    case file:write_file(File, Data) of
        ok ->
            ?log(info, "Message from ~s delivered to ~p (~s)~n", [From, To, File]),
            {ok, ["delivered to ", To]};
        {error, Reason} ->
            ?log(error, "Problems in writing the file ~s~n", [File]),
            {error, ["550 Mailbox ", To, " not available"]}
    end.

-spec reference() -> string().
reference() ->
    UniqueId = erlang:unique_integer(),
    lists:flatten([io_lib:format("~2.16.0b", [X])
                   || <<X>> <= erlang:md5(term_to_binary(UniqueId))]).

mail_dir() ->
    {ok, Cwd} = file:get_cwd(),
    {ok, Dir} = os:getenv("MAIL_DIR", Cwd),
    Dir.

mail_dir(Recipient) ->
    filename:join(mail_dir(), Recipient).
