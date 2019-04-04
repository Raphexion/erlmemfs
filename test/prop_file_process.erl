-module(prop_file_process).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_basic_read() ->
    ?FORALL(Data, prop_generators:content(),
	    begin
		{ok, F} = erlmemfs_file:start_link(Data),
		{ok, Ref} = erlmemfs_file:open(F),
		{ok, Data} =:= erlmemfs_file:read_block(F, Ref, byte_size(Data))
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
