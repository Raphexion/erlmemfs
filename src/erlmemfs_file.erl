-module(erlmemfs_file).
-behaviour(gen_server).

%% API

-export([start_link/1,
	 stop/1,
	 open/1,
	 close/2,
	 write_block/3,
	 read_block/3]).
-export([debug/2]).

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

close(Pid, Ref) ->
    gen_server:call(Pid, {close, Ref}).

write_block(Pid, Ref, Fun) ->
    gen_server:call(Pid, {write_block, Ref, Fun}).

read_block(Pid, Ref, NbBytes) ->
    gen_server:call(Pid, {read, Ref, NbBytes}).

debug(Pid, DebugString) ->
    gen_server:call(Pid, {debug, DebugString}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

-record(state, {
		data = <<>> :: binary(),
		refs = #{} :: map(),
		locked = false :: boolean()
	       }).

init(Data) ->
    {ok, #state{data=Data}}.

handle_call({debug, DebugString}, _From, State=#state{data=Data}) ->
    io:fwrite(DebugString, [State]),
    {reply, ok, State};

handle_call({close, Ref}, _From, State=#state{refs=Refs}) ->
    case maps:get(Ref, Refs, badkey) of
	badkey ->
	    {reply, {error, missing}, State};
	closed ->
	    {reply, {error, alread_closed}, State};
	_Size ->
	    {reply, {ok, closed}, State#state{refs=Refs#{Ref => closed}}}
    end;

handle_call(_What, _From, State=#state{locked=true}) ->
    %% Please not that both debug and close must be matched
    %% before this (locked). Otherwise we can neither
    %% debug or close our files
    {reply, {error, locked}, State};

handle_call(open, _From, State=#state{refs=Refs}) ->
    Ref = make_ref(),
    {reply, {ok, Ref}, State#state{refs=Refs#{Ref => 0}}};

handle_call({write_block, _Ref, Fun}, From, State=#state{refs=Refs}) ->
    Values = maps:values(Refs),
    Filter = fun(V) -> V /= closed end,
    Status = lists:filter(Filter, Values),
    case Status of
	[0] ->
	    erlang:send_after(0, self(), {write, Fun, From}),
	    Empty = <<>>,
	    {noreply, State#state{locked=true, data=Empty}};
	_ ->
	    {reply, {error, busy}, State}
    end;

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

handle_info({write, Fun, From}, State) ->
    {noreply, priv_write_blocks(Fun, From, State)};

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

priv_write_blocks(Fun, From, State) ->
    case Fun(1024) of
        {ok, Bytes, _ReadCount} ->
	    erlang:send_after(0, self(), {write, Fun, From}),
	    #state{data=Data} = State,
	    NewData = <<Data/binary, Bytes/binary>>,
	    State#state{data=NewData};
        done ->
	    gen_server:reply(From, done),
	    State#state{locked=false}
    end.

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
