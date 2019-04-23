-module(prop_generators).
-include_lib("proper/include/proper.hrl").

-export([file/0,
	 files/0,
	 content/0,
	 folder/0,
	 folders/0]).

file() ->
    non_empty(string()).

files() ->
    non_empty(list(file())).

content() ->
    binary().

folder() ->
    ?SUCHTHAT(Folder, non_empty(string()), not invalid(Folder)).

folders() ->
    non_empty(list(folder())).

invalid(Folder) ->
    lists:member($/, Folder) or (Folder =:= ".") or (Folder =:= "..").
