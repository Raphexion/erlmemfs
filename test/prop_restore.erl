-module(prop_restore).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_path_to_parts() ->
    ?FORALL(Parts, prop_generators:folders(),
	    begin
		{ok, Leaf} = erlmemfs_support:mkdirs(erlmemfs_support:root(), Parts),
		Parts =:= erlmemfs_support:path_to_parts(Leaf)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
