-module(prop_tree).
-include_lib("proper/include/proper.hrl").
-import(prop_generators, [unique_files_and_folders/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_non_nested_tree_test() ->
    ?FORALL({Files, Folders}, unique_files_and_folders(),
	    begin
		erlmemfs_sup:start_link(),
		{ok, Fs} = erlmemfs_sup:create_erlmemfs(),

		create_folders(Fs, Folders),
		create_files(Fs, Files),

		Counter = start_counter(self()),
		Binder = binder(Counter),
		erlmemfs:tree(Fs, Binder),

		Counter ! total,
		{NbFolders, NbFiles} =
		    receive
			Res ->
			    Res
		    after 100 ->
			    {-1, -1}
		    end,

		%% +1 because of root folder /
		NbFolders =:= (length(Folders) + 1)
		    andalso NbFiles =:= length(Files)
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

create_folders(_Fs, []) ->
    ok;
create_folders(Fs, [Folder|Folders]) ->
    erlmemfs:make_directory(Fs, Folder),
    create_folders(Fs, Folders).

create_files(_Fs, []) ->
    ok;
create_files(Fs, [File|Files]) ->
    erlmemfs:put_file(Fs, File, <<>>),
    create_files(Fs, Files).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
