-module(erlmemfs).
-behaviour(gen_server).
-include("erlmemfs.hrl").
-include_lib("eunit/include/eunit.hrl").


%% API
-export([start_link/0]).
-export([current_directory/1,
         make_directory/2,
         change_directory/2,
         list_files/1,
         list_files/2,
         remove_directory/2,
         remove_file/2,
         put_file/3,
         get_file/2,
         file_info/2,
         rename_file/3,
	 tree/1,
	 tree/2]).
-export([count/1]).
-export([debug/1]).

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
    gen_server:call(Fs, {list_files, "."}).

list_files(Fs, Name) ->
    gen_server:call(Fs, {list_files, Name}).

remove_directory(Fs, Name) ->
    gen_server:call(Fs, {remove_directory, Name}).

remove_file(Fs, Name) ->
    gen_server:call(Fs, {remove_file, Name}).

put_file(Fs, Name, Data) ->
    gen_server:call(Fs, {put_file, Name, Data}).

get_file(Fs, Name) ->
    gen_server:call(Fs, {get_file, Name}).

file_info(Fs, Name) ->
    gen_server:call(Fs, {file_info, Name}).

rename_file(_Fs, Name, Name) ->
    {ok, Name};

rename_file(Fs, From, To) ->
    gen_server:call(Fs, {rename_file, From, To}).

tree_printer(dir, Name, N) ->
    Gap = ["  " || _ <- lists:seq(1, N)],
    io:fwrite("~s|---~s~n", [Gap, Name]);
tree_printer(file, Name, N) ->
    Gap = [" " || _ <- lists:seq(1, N+1)],
    io:fwrite("~s|---~s~n", [Gap, Name]).

tree(Fs) ->
    tree(Fs, fun tree_printer/3).

tree(Fs, Fun) ->
    gen_server:call(Fs, {tree, Fun}).

count(Fs) ->
    gen_server:call(Fs, count).

debug(Fs) ->
    gen_server:call(Fs, debug).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init(_) ->
    {ok, #dir{name="/", parent=none}}.

%% @hidden
handle_call(current_directory, _From, CWD=#dir{name = Name}) ->
    {reply, {ok, Name}, CWD};

handle_call({make_directory, Path=[$/|_]}, _From, CWD) ->
    Restore = erlmemfs_support:backup_dir(CWD),
    Root = erlmemfs_support:find_root(CWD),
    Parts = string:tokens(Path, "/"),
    case erlmemfs_support:mkdirs(Root, Parts) of
	{error, Reason} ->
	    {reply, {error, Reason}, CWD};
	{ok, NewDir} ->
	    {reply, {ok, Path}, Restore(NewDir)}
    end;

handle_call({make_directory, Path}, _From, CWD) ->
    Restore = erlmemfs_support:backup_dir(CWD),
    Parts = string:tokens(Path, "/"),
    case erlmemfs_support:mkdirs(CWD, Parts) of
	{error, Reason} ->
	    {reply, {error, Reason}, CWD};
	{ok, NewDir} ->
	    {reply, {ok, Path}, Restore(NewDir)}
    end;

handle_call({change_directory, "."}, _From, CWD=#dir{name=Name}) ->
    {reply, {ok, Name}, CWD};

handle_call({change_directory, "/"}, _From, CWD) ->
    {reply, {ok, "/"}, erlmemfs_support:find_root(CWD)};

handle_call({change_directory, ".."}, _From, CWD=#dir{parent=none}) ->
    {reply, {error, root_dir}, CWD};

handle_call({change_directory, ".."}, _From,  CWD) ->
    Parent=#dir{name=Name}=erlmemfs_support:move_up(CWD),
    {reply, {ok, Name}, Parent};

handle_call({change_directory, Abspath=[$/|_]}, _From, CWD) ->
    Root = erlmemfs_support:find_root(CWD),
    Path = string:tokens(Abspath, "/"),
    case erlmemfs_support:move_down(Root, Path) of
	{ok, Folder=#dir{name=Name}} ->
	    {reply, {ok, Name}, Folder};
	Error ->
	    {reply, Error, CWD}
    end;

handle_call({change_directory, Name}, _From, CWD=#dir{content=Content}) ->
    case maps:get(Name, Content, badkey) of
	badkey ->
	    {reply, {error, missing_folder}, CWD};
	#file{} ->
	    {reply, {error, target_is_file}, CWD};
	Target=#dir{} ->
	    %% It is really important that we set
	    %% the parent here. Because we have
	    %% immutable data structures, this is
	    %% the only up-to-date CWD
	    {reply, {ok, Name}, Target#dir{parent=CWD}}
    end;

handle_call({tree, Fun}, _From, CWD) ->
    priv_tree(CWD, Fun),
    {reply, ok, CWD};

handle_call({put_file, Name, Data}, _From, CWD=#dir{content=Content}) ->
    case maps:get(Name, Content, badkey) of
	#dir{} ->
	    {reply, {error, dir_collision}, CWD};
	_ ->
	    %% badkey or #file{}
	    %% design decision: we will override an existing file
	    {ok, Fp} = erlmemfs_file_sup:create_erlmemfs_file(Data),
	    File = #file{name=Name, fp=Fp},
	    {reply, {ok, Name}, CWD#dir{content=Content#{Name => File}}}
    end;

handle_call({get_file, Name}, _From, CWD=#dir{content=Content}) ->
    case maps:get(Name, Content, badkey) of
	badkey ->
	    {reply, {error, missing_file}, CWD};
	#dir{} ->
	    {reply, {error, target_is_dir}, CWD};
	#file{fp=Fp} ->
	    {reply, {ok, Fp}, CWD}
    end;

handle_call({remove_file, Name}, _From, CWD=#dir{content=Content}) ->
    case maps:get(Name, Content, badkey) of
	badkey ->
	    {reply, {error, not_found}, CWD};
	#dir{} ->
	    {reply, {error, not_a_file}, CWD};
	#file{fp=Fp} ->
	    erlmemfs_file:stop(Fp),
	    {reply, {ok, Name}, CWD#dir{content=maps:remove(Name, Content)}}
    end;

%% TODO, handle unempty directory
handle_call({remove_directory, Name}, _From, CWD=#dir{content=Content}) ->
    case maps:get(Name, Content, badkey) of
	badkey ->
	    {reply, {error, directory_missing}, CWD};
	#file{} ->
	    {reply, {error, not_a_directory}, CWD};
	#dir{} ->
	    {reply, {ok, Name}, CWD#dir{content=maps:remove(Name, Content)}}
    end;

handle_call({rename_file, From, To}, _From, CWD=#dir{content=Content}) ->
    FromStatus = maps:get(From, Content, badkey),
    ToStatus = maps:get(To, Content, badkey),

    case priv_rename_file(Content, From, To, FromStatus, ToStatus) of
	{ok, Name, NewContent} ->
	    {reply, {ok, Name}, CWD#dir{content=NewContent}};
	{error, Why, Content} ->
	    {reply, {error, Why}, CWD}
    end;

handle_call({list_files, "."}, _From, CWD) ->
    Listing = priv_ls(CWD),
    {reply, {ok, Listing}, CWD};

handle_call({list_files, Name}, _From, CWD=#dir{content=Content}) ->
    case maps:get(Name, Content, badkey) of
	badkey ->
	    {reply, {error, missing}, CWD};
	#file{name=Name} ->
	    {reply, {ok, [Name]}, CWD};
	Dir=#dir{} ->
	    Listing = priv_ls(Dir),
	    {reply, {ok, Listing}, CWD}
    end;

handle_call({file_info, _Name}, _From, State) ->
    {reply, {error, not_implemented}, State};

handle_call(count, _From, State) ->
    {reply, {ok, stats:count(erlmemfs_support:find_root(State))}, State};

handle_call(debug, _From, State) ->
    {reply, {ok, State}, State};

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

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

priv_tree(Tree, Fun) ->
    priv_tree(Tree, Fun, 0).

priv_tree(#dir{name = Name, content = Tree}, Fun, N) ->
    priv_print_dir(Fun, Name, N),
    [priv_tree(Node, Fun, N+1) || Node <- maps:values(Tree)];

priv_tree(#file{name = Name, fp=Fp}, Fun, N) ->
    priv_print_file(Fun, Name, Fp, N).

priv_print_dir(Fun, Name, N) ->
    Fun(dir, Name, N).

priv_print_file(Fun, Name, _Fp, N) ->
    Fun(file, Name, N).

priv_rename_file(Content, _From, _To, badkey, _ToStatus) ->
    {error, file_missing, Content};

priv_rename_file(Content, _From, _To, #dir{}, _ToStatus) ->
    {error, not_a_file, Content};

priv_rename_file(Content, From, To, File0=#file{}, badkey) ->
    File1 = File0#file{name = To},
    {ok, To, maps:put(To, File1, maps:remove(From, Content))};

priv_rename_file(Content, _From, _To, _FromStatus, _ToStatus) ->
    {error, collision, Content}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

%%
priv_ls(#dir{content=Content}) ->
    lists:map(fun node_name/1, maps:values(Content)).

%% @doc Extract name of both files and dirs
node_name(#file{name=Name}) ->
    {file, Name};
node_name(#dir{name=Name}) ->
    {dir, Name}.

%%-----------------------------------------------------------------------------
%% Test
%%------------------------------------------------------------------------------

missing_folder_test() ->
    {ok, Fs} = start_link(),
    ?assert({error, missing_folder} =:= change_directory(Fs, "abc")).

missing_folder_abs_test() ->
    {ok, Fs} = start_link(),
    ?assert({error, missing_folder} =:= change_directory(Fs, "/abc")).

get_missing_file_test() ->
    {ok, Fs} = start_link(),
    ?assert({error, missing_file} =:= get_file(Fs, "abc.txt")).

get_file_that_is_a_folder_test() ->
    Name = "abc",
    {ok, Fs} = start_link(),
    {ok, Name} = make_directory(Fs, Name),
    ?assert({error, target_is_dir} =:= get_file(Fs, Name)).

remove_missing_file_test() ->
    {ok, Fs} = start_link(),
    ?assert({error, not_found} =:= remove_file(Fs, "abc.txt")).

remove_missing_directory_test() ->
    {ok, Fs} = start_link(),
    ?assert({error, directory_missing} =:= remove_directory(Fs, "abc")).

remove_file_that_is_folder_test() ->
    Name = "abc",
    {ok, Fs} = start_link(),
    {ok, Name} = make_directory(Fs, Name),
    ?assert({error, not_a_file} =:= remove_file(Fs, Name)).

remove_directory_that_is_a_file_test() ->
    erlmemfs_file_sup:start_link(),
    Name = "abc",
    {ok, Fs} = start_link(),
    {ok, Name} = put_file(Fs, Name, <<>>),
    ?assert({error, not_a_directory} =:= remove_directory(Fs, Name)).

rename_file_to_file_that_already_exists_test() ->
    erlmemfs_file_sup:start_link(),
    OrgName = "abc.txt",
    NewName = "def.txt",
    {ok, Fs} = start_link(),
    {ok, OrgName} = put_file(Fs, OrgName, <<1,2,3,4>>),
    {ok, NewName} = put_file(Fs, NewName, <<1,2,3,4>>),

    ?assert({error, collision} =:= rename_file(Fs, OrgName, NewName)).

cd_file_collision_test() ->
    Name = "abc",
    {ok, Fs} = start_link(),
    {ok, Name} = put_file(Fs, Name, <<>>),
    ?assert({error, target_is_file} =:= change_directory(Fs, Name)).

cd_file_collision_abs_test() ->
    Name = "abc",
    {ok, Fs} = start_link(),
    {ok, Name} = put_file(Fs, Name, <<>>),
    ?assert({error, target_is_file} =:= change_directory(Fs, "/" ++ Name)).

list_files_minimal_test() ->
    {ok, Fs} = start_link(),

    {ok, "a"} = put_file(Fs, "a", <<>>),
    {ok, "b"} = put_file(Fs, "b", <<>>),
    {ok, "c"} = put_file(Fs, "c", <<>>),

    {ok, "d"} = make_directory(Fs, "d"),
    {ok, "e"} = make_directory(Fs, "e"),
    {ok, "f"} = make_directory(Fs, "f"),

    Res = [{file, "a"}, {file, "b"}, {file, "c"},
	   {dir, "d"}, {dir, "e"}, {dir, "f"}],
    ?assert({ok, Res} =:= list_files(Fs)).

list_files_in_folder_minimal_test() ->
    {ok, Fs} = start_link(),

    Folder = "sandbox",
    {ok, Folder} = make_directory(Fs, Folder),
    {ok, Folder} = change_directory(Fs, Folder),

    {ok, "a"} = put_file(Fs, "a", <<>>),
    {ok, "b"} = put_file(Fs, "b", <<>>),
    {ok, "c"} = put_file(Fs, "c", <<>>),

    {ok, "d"} = make_directory(Fs, "d"),
    {ok, "e"} = make_directory(Fs, "e"),
    {ok, "f"} = make_directory(Fs, "f"),

    {ok, "/"} = change_directory(Fs, ".."),

    Res = [{file, "a"}, {file, "b"}, {file, "c"},
	   {dir, "d"}, {dir, "e"}, {dir, "f"}],

    ?assert({ok, Res} =:= list_files(Fs, Folder)).
