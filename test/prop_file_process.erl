-module(prop_file_process).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_basic_file() ->
    ?FORALL(Data, prop_generators:content(),
	    begin
		erlmemfs_file_sup:start_link(),
		{ok, F} = erlmemfs_file_sup:create_erlmemfs_file(Data),
		{ok, Data} =:= erlmemfs_file:read(F)
	    end).

prop_basic_read() ->
    ?FORALL(Data, non_empty(prop_generators:content()),
	    begin
		erlmemfs_file_sup:start_link(),
		{ok, F} = erlmemfs_file_sup:create_erlmemfs_file(Data),
		{ok, Ref} = erlmemfs_file:open(F),
		{ok, Data} =:= read_block(F, Ref, byte_size(Data)) andalso
		    {ok, eof} =:= read_block(F, Ref, 1) andalso
		    {ok, closed} =:= erlmemfs_file:close(F, Ref) andalso
		    {error, alread_closed} =:= erlmemfs_file:close(F, Ref)
	    end).

prop_bad_read() ->
    ?FORALL(Data, non_empty(prop_generators:content()),
	    begin
		erlmemfs_file_sup:start_link(),
		{ok, F} = erlmemfs_file_sup:create_erlmemfs_file(Data),
		{ok, _Ref} = erlmemfs_file:open(F),
		Ref = nonsense_reference,
		{error, bad_reference} =:= read_block(F, Ref, byte_size(Data))
	    end).

prop_twice_read() ->
    ?FORALL(BaseData, non_empty(prop_generators:content()),
	    begin
		Data = double_data(BaseData),
		erlmemfs_file_sup:start_link(),
		{ok, F} = erlmemfs_file_sup:create_erlmemfs_file(Data),
		{ok, Ref} = erlmemfs_file:open(F),
		Size = byte_size(BaseData),
		{ok, BaseData} =:= read_block(F, Ref, Size) andalso
		    {ok, BaseData} =:= read_block(F, Ref, Size) andalso
		    {ok, eof} =:= read_block(F, Ref, 1)
	    end).

prop_reverse_twice_read() ->
    ?FORALL(DataOrg, non_empty(prop_generators:content()),
	    begin
		DataRev = reverse_data(DataOrg),
		erlmemfs_file_sup:start_link(),
		{ok, F} = erlmemfs_file_sup:create_erlmemfs_file(<<DataOrg/binary, DataRev/binary>>),
		{ok, Ref} = erlmemfs_file:open(F),
		Size = byte_size(DataOrg),
		{ok, DataOrg} =:= read_block(F, Ref, Size) andalso
		    {ok, DataRev} =:= read_block(F, Ref, Size) andalso
		    {ok, eof} =:= read_block(F, Ref, 1)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

read_block(F, Ref, Size) ->
    erlmemfs_file:read_block(F, Ref, Size).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

reverse_data(Data) ->
    binary:list_to_bin(lists:reverse(binary:bin_to_list(Data))).

double_data(Data) ->
    <<Data/binary, Data/binary>>.
