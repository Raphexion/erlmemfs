-module(prop_generators).
-include_lib("proper/include/proper.hrl").

-export([folder/0,
	 folders/0]).

folder() ->
    ?SUCHTHAT(Folder, non_empty(string()), not invalid(Folder)).

folders() ->
    non_empty(list(folder())).

invalid(Folder) ->
    lists:member($/, Folder) or (Folder =:= ".") or (Folder =:= "..").

