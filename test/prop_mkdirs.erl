-module(prop_mkdirs).
-include("erlmemfs.hrl").
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_support_root_test() ->
    ?FORALL(Folders, prop_generators:folders(),
	    begin
		{ok, Leaf} = erlmemfs_support:mkdirs(erlmemfs_support:root(), Folders),
		Tree = erlmemfs_support:find_root(Leaf),
		#{dir := N, file := 0} = stats:count(Tree),
		N =:= length(Folders)
	    end).

prop_support_non_root_test() ->
    ?FORALL(Folders, prop_generators:folders(),
	    begin
		Root = erlmemfs_support:root(),
		Base = #dir{name="base", parent=Root},
		{ok, Leaf} = erlmemfs_support:mkdirs(Base, Folders),
		Tree = erlmemfs_support:find_root(Leaf),
		#{dir := N, file := 0} = stats:count(Tree),
		N =:= length(Folders) + 1
	    end).

prop_root_test() ->
    ?FORALL(Folders, prop_generators:folders(),
	    begin
		{ok, Fs} = erlmemfs:start_link(),
		Path = "/" ++ string:join(Folders, "/"),
		erlmemfs:make_directory(Fs, Path),
		{ok, #{dir := N, file := 0}} = erlmemfs:count(Fs),
		N =:= length(Folders)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
