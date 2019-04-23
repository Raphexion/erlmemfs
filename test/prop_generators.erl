-module(prop_generators).
-include_lib("proper/include/proper.hrl").

-export([file/0,
	 files/0,
	 content/0,
	 folder/0,
	 folders/0,
	 unique_files_and_folders/0]).

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

files_and_folders() ->
    {files(), folders()}.

unique_files_and_folders() ->
    ?SUCHTHAT({Files, Folders}, files_and_folders(), unique_list(Files ++ Folders)).

unique_list(List) ->
    length(List) =:= sets:size(sets:from_list(List)).
