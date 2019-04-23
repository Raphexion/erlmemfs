-module(prop_tree).
-include_lib("proper/include/proper.hrl").
-import(prop_generators, [folder/0,
			  folders/0,
			  files/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_minimal_tree_test() ->
    ?FORALL(Folder, folder(),
	    begin
		application:ensure_all_started(erlmemfs),
		{ok, Fs} = erlmemfs_sup:create_erlmemfs(),
		{ok, Folder} = erlmemfs:make_directory(Fs, Folder),

		Counter = start_counter(self()),
		Binder = binder(Counter),
		erlmemfs:tree(Fs, Binder),

		Counter ! total,
		{Dirs, Files} = receive
				    Res ->
					Res
				after 100 ->
					{-1, -1}
				end,

		%% 2 because Root dir + Folder
		Dirs =:= 2 andalso Files =:= 0
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
start_counter(Pid) ->
    spawn_link(fun() -> counter(Pid, 0, 0) end).

counter(Pid, Dirs, Files) ->
    receive
	total ->
	    Pid ! {Dirs, Files};
	file ->
	    counter(Pid, Dirs, Files + 1);
	dir ->
	    counter(Pid, Dirs + 1, Files)
    end.

binder(Counter) ->
    fun (dir, _, _) ->
	    Counter ! dir;
        (file, _, _) ->
	    Counter ! file
    end.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
