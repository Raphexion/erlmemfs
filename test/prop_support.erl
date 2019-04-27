-module(prop_support).
-include("erlmemfs.hrl").
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_move_ups_test() ->
    ?FORALL(Folders, prop_generators:folders(),
	    begin
		{ok, Leaf} = erlmemfs_support:mkdirs(erlmemfs_support:root(), Folders),
		NbSteps = length(Folders),
		#dir{name=Name, parent=Parent} = erlmemfs_support:move_ups(Leaf, NbSteps),
		Name =:= "/" andalso Parent =:= none
	    end).

prop_move_ups_past_test() ->
    ?FORALL(Folders, prop_generators:folders(),
	    begin
		{ok, Leaf} = erlmemfs_support:mkdirs(erlmemfs_support:root(), Folders),
		NbSteps = length(Folders),
		#dir{name=Name, parent=Parent} = erlmemfs_support:move_ups(Leaf, NbSteps + 1),
		Name =:= "/" andalso Parent =:= none
	    end).

prop_move_down_test() ->
    ?FORALL(Folders, prop_generators:folders(),
	    begin
		{ok, Leaf1} = erlmemfs_support:mkdirs(erlmemfs_support:root(), Folders),
		Base = erlmemfs_support:move_ups(Leaf1, length(Folders)),
		{ok, Leaf2} = erlmemfs_support:move_down(Base, Folders),
		#dir{name=Name1} = Leaf1,
		#dir{name=Name2} = Leaf2,
		Name = lists:last(Folders),
		Name1 =:= Name andalso Name2 =:= Name
	    end).

prop_move_down_errors_test() ->
    ?FORALL({Folders, Folder}, {prop_generators:folders(), prop_generators:folder()},
	    begin
		{ok, Leaf1} = erlmemfs_support:mkdirs(erlmemfs_support:root(), Folders),
		Base = erlmemfs_support:move_ups(Leaf1, length(Folders)),
		{ok, Leaf2} = erlmemfs_support:move_down(Base, Folders),
		{error, missing_folder} =:= erlmemfs_support:move_down(Leaf2, Folder)
	    end).

prop_collision_test() ->
    ?FORALL(Name, prop_generators:folder(),
	    begin
		erlmemfs_sup:start_link(),
		erlmemfs_file_sup:start_link(),
		{ok, Fs} = erlmemfs:start_link(),
		{ok, Name} = erlmemfs:put_file(Fs, Name, <<>>),
		{error, file_collision} =:= erlmemfs:make_directory(Fs, Name)
		    andalso
		    {error, target_is_file} =:= erlmemfs:change_directory(Fs, Name)
	    end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
