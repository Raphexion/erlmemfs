-module(prop_cd).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Folders, folders(),
	    begin
		{ok, Fs} = erlmemfs:start_link(),
		Path = "/" ++ string:join(Folders, "/"),
		erlmemfs:make_directory(Fs, Path),
		erlmemfs:change_directory(Fs, Path),
		TestablePaths = testable_paths(Folders),
		length(TestablePaths) =:= length(Folders)
		    andalso test_paths(Fs, TestablePaths)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

testable_paths(Folders) ->
    testable_paths(Folders, "", []).

testable_paths([], _Acc, Paths) ->
    Paths;
testable_paths([Name|Rest], Acc, Paths) ->
    New = Acc ++ "/" ++ Name,
    testable_paths(Rest, New, [New|Paths]).

test_paths(_Fs, []) ->
    true;
test_paths(Fs, [Path|Rest]) ->
    Name = lists:last(string:tokens(Path, "/")),
    {ok, Name} =:= erlmemfs:change_directory(Fs, Path)
	andalso test_paths(Fs, Rest).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
folder() ->
    ?SUCHTHAT(Folder, non_empty(string()), not invalid(Folder)).

folders() ->
    non_empty(list(folder())).

invalid(Folder) ->
    lists:member($/, Folder) or (Folder =:= ".") or (Folder =:= "..").
