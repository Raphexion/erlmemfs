-module(prop_file_content).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Content, prop_generators:content(),
	    begin
		application:ensure_all_started(erlmemfs),
		{ok, F} = erlmemfs_file_sup:create_erlmemfs_file(Content),
		{ok, Content} =:= erlmemfs_file:read(F)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
