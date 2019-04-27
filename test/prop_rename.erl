-module(prop_rename).
-include_lib("proper/include/proper.hrl").
-import(prop_generators, [file/0, content/0]).

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

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
