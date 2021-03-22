%%%-------------------------------------------------------------------
%% @doc lmtp_poc public API
%% @end
%%%-------------------------------------------------------------------

-module(lmtp_poc_app).

-behaviour(application).

-include_lib("hut/include/hut.hrl").  % Used for logging

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ?log(info, "*** ~p started ***~n", [?MODULE]),
    lmtp_poc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
