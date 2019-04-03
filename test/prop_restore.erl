-module(prop_restore).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_path_to_parts() ->
    ?FORALL(Parts, prop_generators:folders(),
	    begin
		{ok, Leaf} = support:mkdirs(support:root(), Parts),
		Parts =:= support:path_to_parts(Leaf)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
