-module(support).
-include("erlmemfs.hrl").

-export([move_up/1,
	 move_ups/2,
	 find_root/1,
	 move_down/2,
	 path_to_folders/1]).

%% @doc move_up
%% Whenever we move back in the tree,
%% we must update the parent's copy of the tree.
%% Otherwise all changes are lost.
%% Thus, only do it at this place in the code.

move_up(Root=#dir{name="/", parent=none}) ->
    Root;
move_up(Tree=#dir{name=CWD, parent=Parent}) ->
    #dir{content=Content} = Parent,
    Parent#dir{content=Content#{CWD => Tree}}.

%% @doc move_ups
%% Move up N times in the tree.

move_ups(CWD, 0) ->
    CWD;
move_ups(CWD, N) ->
    move_ups(move_up(CWD), N-1).

%% @doc find_root
%% Recursively move up until we hit the root

find_root(Node=#dir{name="/", parent=none}) ->
    Node;
find_root(Node) ->
    find_root(move_up(Node)).

%% @doc move_down
%% Move down in the tree according to
%% a list of folder names

move_down(Tree=#dir{}, []) ->
    {ok, Tree};
move_down(#dir{content=Content}, [Head|Tail]) ->
    case maps:get(Head, Content, badkey) of
	badkey ->
	    {error, missing_folder};
	#file{} ->
	    {error, missing_folder};
	Folder=#dir{} ->
	    move_down(Folder, Tail)
    end.

%% @doc path_to_folders
%% Turn a path into a list of folders

path_to_folders(CWD) ->
    path_to_folders(CWD, []).

path_to_folders(#dir{name="/", parent=none}, Folders) ->
    lists:reverse(Folders);
path_to_folders(CWD=#dir{name=Name}, Folders) ->
    path_to_folders(move_up(CWD), [Name|Folders]).

