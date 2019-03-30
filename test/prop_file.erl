-module(prop_file).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    {ok, Fs} = erlmemfs:start_link(),
    ?FORALL({Name, Content}, {non_empty(string()), binary()},
	    begin
		combine(Fs, Name, Content, [fun put_file/3,
					    fun get_file/3,
					    fun del_file/3,
					    fun gone/3])
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
    {ok, Content} =:= erlmemfs:get_file(Fs, Name).

del_file(Fs, Name, _) ->
    {ok, Name} =:= erlmemfs:remove_file(Fs, Name).

gone(Fs, Name, _) ->
    {error, missing_file} =:= erlmemfs:get_file(Fs, Name).
