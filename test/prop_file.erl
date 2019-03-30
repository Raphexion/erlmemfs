-module(prop_file).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL({Name, Content}, {file(), content()},
	    begin
		{ok, Fs} = erlmemfs:start_link(),
		combine(Fs, Name, Content, [fun put_file/3,
					    fun get_file/3,
					    fun del_file/3,
					    fun gone/3])
	    end).

prop_nested() ->
    ?FORALL({Folder, Name, Content}, {folder(), file(), content()},
	    begin
		{ok, Fs} = erlmemfs:start_link(),
		make_path(Fs, Folder),
		put_file(Fs, Name, Content)
		    andalso get_file(Fs, Name, Content)
		    andalso correct_count(Fs, Folder, 1)
		    andalso del_file(Fs, Name, Content)
		    andalso correct_count(Fs, Folder, 0)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

make_path(_Fs, []) ->
    ok;
make_path(Fs, [H|T]) ->
    erlmemfs:make_directory(Fs, H),
    erlmemfs:change_directory(Fs, H),
    make_path(Fs, T).

combine(_Fs, _Name, _Content, []) ->
    true;
combine(Fs, Name, Content, [Test|Rest]) ->
    Test(Fs, Name, Content) andalso
	combine(Fs, Name, Content, Rest).

put_file(Fs, Name, Content) ->
    {ok, Name} =:= erlmemfs:put_file(Fs, Name, Content).

get_file(Fs, Name, Content) ->
    {ok, Content} =:= erlmemfs:get_file(Fs, Name).

del_file(Fs, Name, _) ->
    {ok, Name} =:= erlmemfs:remove_file(Fs, Name).

gone(Fs, Name, _) ->
    {error, missing_file} =:= erlmemfs:get_file(Fs, Name).

correct_count(Fs, Folder, NbFiles) ->
    N = length(Folder),
    {ok, #{file => NbFiles, dir => N}} =:= erlmemfs:count(Fs).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

folder() ->
    non_empty(list(non_empty(string()))).

file() ->
    non_empty(string()).

content() ->
    non_empty(binary()).
