%%%-------------------------------------------------------------------
%% @doc erlmemfs public API
%% @end
%%%-------------------------------------------------------------------

-module(erlmemfs_app).
-include_lib("eunit/include/eunit.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    erlmemfs_sup:start_link(),
    erlmemfs_file_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    gen_server:stop(erlmemfs_sup),
    gen_server:stop(erlmemfs_file_sup),
    ok.

%%====================================================================
%% Tests
%%====================================================================

start_stop_test() ->
    start(ok, ok),
    ok =:= stop(ok).
