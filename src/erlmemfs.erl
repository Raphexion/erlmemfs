-module(erlmemfs).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([current_directory/1,
         make_directory/2,
         change_directory/2,
         list_files/1,
         list_files/2,
         remove_directory/2,
         remove_file/2,
         put_file/4,
         get_file/2,
         file_info/2,
         rename_file/3]).

%% Behaviour Callbacks

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

current_directory(Fs) ->
    gen_server:call(Fs, current_directory).

make_directory(Fs, Name) ->
    gen_server:call(Fs, {make_directory, Name}).

change_directory(Fs, Name) ->
    gen_server:call(Fs, {change_directory, Name}).

list_files(Fs) ->
    gen_server:call(Fs, list_files).

list_files(Fs, Name) ->
    gen_server:call(Fs, {list_files, Name}).

remove_directory(Fs, Name) ->
    gen_server:call(Fs, {remove_directory, Name}).

remove_file(Fs, Name) ->
    gen_server:call(Fs, {remove_file, Name}).

put_file(Fs, Name, Mode, Data) ->
    gen_server:call(Fs, {put_file, Name, Mode, Data}).

get_file(Fs, Name) ->
    gen_serve:call(Fs, {get_file, Name}).

file_info(Fs, Name) ->
    gen_server:call(Fs, {file_info, Name}).

rename_file(Fs, From, To) ->
    gen_server:call(Fs, {rename_file, From, To}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init(_) ->
    {ok, #{}}.

%% @hidden
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
