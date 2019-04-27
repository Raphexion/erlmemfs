-module(prop_file_content).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Content, prop_generators:content(),
	    begin
		erlmemfs_file_sup:start_link(),
		{ok, F} = erlmemfs_file_sup:create_erlmemfs_file(Content),
		{ok, Content} =:= erlmemfs_file:read(F)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
