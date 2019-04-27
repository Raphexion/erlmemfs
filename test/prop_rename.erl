-module(prop_rename).
-include_lib("proper/include/proper.hrl").
-import(prop_generators, [file/0, folder/0, content/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_rename_test() ->
    ?FORALL({OrgName, NewName, Content}, {file(), file(), file()},
	    begin
		erlmemfs_sup:start_link(),
		{ok, F} = erlmemfs_sup:create_erlmemfs(),
		{ok, OrgName} = erlmemfs:put_file(F, OrgName, Content),
		{ok, Fp} = erlmemfs:get_file(F, OrgName),
		{ok, NewName} = erlmemfs:rename_file(F, OrgName, NewName),
		{ok, Fp} =:= erlmemfs:get_file(F, NewName)
	    end).

prop_rename_missing_file_test() ->
    ?FORALL({OrgName, NewName}, unique_files(),
	    begin
		erlmemfs_sup:start_link(),
		{ok, Fs} = erlmemfs_sup:create_erlmemfs(),
		{error, file_missing} =:= erlmemfs:rename_file(Fs, OrgName, NewName)
	    end).

prop_rename_folder_collision_test() ->
    ?FORALL({Folder, File}, unique_folder_file(),
	    begin
		erlmemfs_sup:start_link(),
		{ok, F} = erlmemfs_sup:create_erlmemfs(),
		{ok, Folder} = erlmemfs:make_directory(F, Folder),
		{error, not_a_file} =:= erlmemfs:rename_file(F, Folder, File)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

unique_files() ->
    ?SUCHTHAT({OrgName, NewName}, {file(), file()}, OrgName /= NewName).

unique_folder_file() ->
    ?SUCHTHAT({OrgName, NewName}, {folder(), file()}, OrgName /= NewName).
