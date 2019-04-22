-module(prop_support).
-include("erlmemfs.hrl").
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Folders, prop_generators:folders(),
	    begin
		{ok, Leaf} = erlmemfs_support:mkdirs(erlmemfs_support:root(), Folders),
		NbSteps = length(Folders),
		#dir{name=Name, parent=Parent} = erlmemfs_support:move_ups(Leaf, NbSteps),
		Name =:= "/" andalso Parent =:= none
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
