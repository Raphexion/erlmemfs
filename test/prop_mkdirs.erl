-module(prop_mkdirs).
-include("erlmemfs.hrl").
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_support_root_test() ->
    ?FORALL(Folders, folders(),
	    begin
		{ok, Leaf} = support:mkdirs(support:root(), Folders),
		Tree = support:find_root(Leaf),
		#{dir := N, file := 0} = stats:count(Tree),
		N =:= length(Folders)
	    end).

prop_support_non_root_test() ->
    ?FORALL(Folders, folders(),
	    begin
		Root = support:root(),
		Base = #dir{name="base", parent=Root},
		{ok, Leaf} = support:mkdirs(Base, Folders),
		Tree = support:find_root(Leaf),
		#{dir := N, file := 0} = stats:count(Tree),
		N =:= length(Folders) + 1
	    end).

prop_root_test() ->
    ?FORALL(Folders, folders(),
	    begin
		{ok, Fs} = erlmemfs:start_link(),
		Root = support:root(),
		Path = "/" ++ string:join(Folders, "/"),
		true
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

folder() ->
    ?SUCHTHAT(Folder, non_empty(string()), not invalid(Folder)).

folders() ->
    non_empty(list(folder())).

invalid(Folder) ->
    lists:member($/, Folder) or (Folder =:= ".") or (Folder =:= "..").
