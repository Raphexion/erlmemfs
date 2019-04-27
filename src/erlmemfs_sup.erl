%%%-------------------------------------------------------------------
%% @doc erlmemfs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlmemfs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 create_erlmemfs/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_erlmemfs() ->
    supervisor:start_child(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},

    ErlMemFs = #{id => erlmemfs,
		 restart => transient,
		 start => {erlmemfs, start_link, []}},

    Children = [ErlMemFs],

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
