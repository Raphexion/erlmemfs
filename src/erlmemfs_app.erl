%%%-------------------------------------------------------------------
%% @doc erlmemfs public API
%% @end
%%%-------------------------------------------------------------------

-module(erlmemfs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    erlmemfs_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
