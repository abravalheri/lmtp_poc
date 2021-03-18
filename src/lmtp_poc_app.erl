%%%-------------------------------------------------------------------
%% @doc lmtp_poc public API
%% @end
%%%-------------------------------------------------------------------

-module(lmtp_poc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lmtp_poc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
