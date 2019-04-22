-module(prop_put_file).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_basic_write() ->
    ?FORALL(Data, non_empty(prop_generators:content()),
	    begin
		application:ensure_all_started(erlmemfs),
		{ok, F} = erlmemfs_file_sup:create_erlmemfs_file(<<>>),
		{ok, Ref} = erlmemfs_file:open(F),
		done = erlmemfs_file:write_block(F, Ref, create_writer(Data)),
		{ok, closed} = erlmemfs_file:close(F, Ref),
		{ok, Fd2} = erlmemfs_file:open(F),
		{ok, Data} =:= erlmemfs_file:read_block(F, Fd2, 4096)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

create_writer(Data) ->
    {ok, Fp} = erlmemfs_file_sup:create_erlmemfs_file(Data),
    {ok, Fd} = erlmemfs_file:open(Fp),
    fun(BlockSize) ->
	    case erlmemfs_file:read_block(Fp, Fd, BlockSize) of
		{ok, eof} ->
		    done;
		{ok, Bytes} ->
		    {ok, Bytes, byte_size(Bytes)}
	    end
    end.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
