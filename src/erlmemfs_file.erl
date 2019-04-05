-module(erlmemfs_file).
-behaviour(gen_server).

%% API

-export([start_link/1,
	 stop/1,
	 open/1,
	 read_block/3]).

%% Behaviour callbacks

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-----------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Data) ->
    gen_server:start_link(?MODULE, Data, []).

stop(Pid) ->
    gen_server:stop(Pid, normal, 5000).

open(Pid) ->
    gen_server:call(Pid, open).

read_block(Pid, Ref, NbBytes) ->
    gen_server:call(Pid, {read, Ref, NbBytes}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {
	  data = <<>> :: binary(),
	  refs = #{} :: map()
	 }).

init(Data) ->
    {ok, #state{data=Data}}.

handle_call(open, _From, State=#state{refs=Refs}) ->
    Ref = make_ref(),
    {reply, {ok, Ref}, State#state{refs=Refs#{Ref => 0}}};

handle_call({read, Ref, NbBytes}, _From, State=#state{data=Data, refs=Refs}) ->
    case priv_read(Data, NbBytes, maps:get(Ref, Refs, badkey)) of
	badkey ->
	    {reply, {error, bad_reference}, State};
	eof ->
	    {reply, {ok, eof}, State};
	{ok, Bytes, RefState} ->
	    {reply, {ok, Bytes}, State#state{refs=Refs#{Ref => RefState}}}
    end;

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

priv_read(_Data, _NbBytes, badkey) ->
    badkey;

priv_read(Data, _NbBytes, AlreadyRead) when AlreadyRead >= byte_size(Data) ->
    eof;

priv_read(Data, NbBytes, AlreadyRead) ->
    Rem = byte_size(Data) - AlreadyRead,
    case NbBytes < Rem of
	true ->
	    Block = binary:part(Data, AlreadyRead, NbBytes),
	    {ok, Block, AlreadyRead + NbBytes};
	false ->
	    Block = binary:part(Data, AlreadyRead, Rem),
	    {ok, Block, byte_size(Data)}
    end.
