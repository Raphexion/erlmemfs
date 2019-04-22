-module(prop_file).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_file_test() ->
    ?FORALL({Name, Content}, name_and_content(),
	    begin
		application:ensure_all_started(erlmemfs),
		{ok, Fs} = erlmemfs:start_link(),
		combine(Fs, Name, Content, [fun put_file/3,
					    fun get_file/3,
					    fun del_file/3,
					    fun gone/3])
	    end).

prop_file_nested_test() ->
    ?FORALL({Folders, Name, Content}, folders_file_and_content(),
	    begin
		application:ensure_all_started(erlmemfs),
		{ok, Fs} = erlmemfs:start_link(),
		Path = "/" ++ string:join(Folders, "/"),
		erlmemfs:make_directory(Fs, Path),
		erlmemfs:change_directory(Fs, Path),
		put_file(Fs, Name, Content)
		    andalso get_file(Fs, Name, Content)
		    andalso correct_count(Fs, Folders, 1)
		    andalso del_file(Fs, Name, Content)
		    andalso correct_count(Fs, Folders, 0)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

combine(_Fs, _Name, _Content, []) ->
    true;
combine(Fs, Name, Content, [Test|Rest]) ->
    Test(Fs, Name, Content) andalso
	combine(Fs, Name, Content, Rest).

put_file(Fs, Name, Content) ->
    {ok, Name} =:= erlmemfs:put_file(Fs, Name, Content).

get_file(Fs, Name, Content) ->
    {ok, Fp} = erlmemfs:get_file(Fs, Name),
    Content =:= content(Fp).

del_file(Fs, Name, _) ->
    {ok, Name} =:= erlmemfs:remove_file(Fs, Name).

gone(Fs, Name, _) ->
    {error, missing_file} =:= erlmemfs:get_file(Fs, Name).

correct_count(Fs, Folders, NbFiles) ->
    N = length(Folders),
    {ok, #{file => NbFiles, dir => N}} =:= erlmemfs:count(Fs).

content(Fp) ->
    {ok, Fd} = erlmemfs_file:open(Fp),
    {ok, Content} = erlmemfs_file:read_block(Fp, Fd, 4096),
    {ok, eof} = erlmemfs_file:read_block(Fp, Fd, 4096),
    Content.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

name_and_content() ->
    {prop_generators:file(),
     non_empty(prop_generators:content())}.

folders_file_and_content() ->
    {prop_generators:folders(),
     prop_generators:file(),
     non_empty(prop_generators:content())}.
