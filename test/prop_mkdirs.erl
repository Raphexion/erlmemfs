-module(prop_mkdirs).
-include("erlmemfs.hrl").
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Folders, folders(),
	    begin
		Leaf = support:mkdirs(support:root(), Folders),
		Tree = support:find_root(Leaf),
		Res = #{dir := N, file := 0} = stats:count(Tree),
		io:fwrite("RES: ~p <- ~p~n", [Res, Folders]),
		N =:= length(Folders)
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

