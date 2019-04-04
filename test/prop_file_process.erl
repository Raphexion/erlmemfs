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
		{ok, Data} =:= erlmemfs_file:read_block(F, Ref, byte_size(Data)) andalso
		    {ok, eof} =:= erlmemfs_file:read_block(F, Ref, 1)
	    end).

prop_twice_read() ->
    ?FORALL(BaseData, non_empty(prop_generators:content()),
	    begin
		Data = double_data(BaseData),
		{ok, F} = erlmemfs_file:start_link(Data),
		{ok, Ref} = erlmemfs_file:open(F),
		Size = byte_size(BaseData),
		{ok, BaseData} =:= erlmemfs_file:read_block(F, Ref, Size) andalso
		    {ok, BaseData} =:= erlmemfs_file:read_block(F, Ref, Size) andalso
		    {ok, eof} =:= erlmemfs_file:read_block(F, Ref, 1)
	    end).

prop_reverse_twice_read() ->
    ?FORALL(DataOrg, non_empty(prop_generators:content()),
	    begin
		DataRev = reverse_data(DataOrg),
		{ok, F} = erlmemfs_file:start_link(<<DataOrg/binary, DataRev/binary>>),
		{ok, Ref} = erlmemfs_file:open(F),
		Size = byte_size(DataOrg),
		{ok, DataOrg} =:= erlmemfs_file:read_block(F, Ref, Size) andalso
		    {ok, DataRev} =:= erlmemfs_file:read_block(F, Ref, Size) andalso
		    {ok, eof} =:= erlmemfs_file:read_block(F, Ref, 1)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

reverse_data(Data) ->
    binary:list_to_bin(lists:reverse(binary:bin_to_list(Data))).

double_data(Data) ->
    <<Data/binary, Data/binary>>.
