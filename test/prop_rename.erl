-module(prop_rename).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_rename_test() ->
    ?FORALL({OrgName, NewName, Content}, test_data(),
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

test_data() ->
    {prop_generators:file(), prop_generators:file(), prop_generators:content()}.
