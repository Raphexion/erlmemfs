-module(erlmemfs_file_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
	 create_erlmemfs_file/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_erlmemfs_file(Data) ->
    supervisor:start_child(?MODULE, [Data]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 1},

    ErlMemFsFile = #{id => erlmemfs_file,
		     start => {erlmemfs_file, start_link, []},
		     restart => transient},

    Children = [ErlMemFsFile],

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
