-module(prop_dirs).
-include_lib("proper/include/proper.hrl").

 %%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    {ok, Fs} = erlmemfs:start_link(),
    ?FORALL(Name, string(),
	    begin
		make(Fs, Name)
		    andalso 1 =:= count_dirs(Fs)
		    andalso remove(Fs, Name)
		    andalso 0 =:= count_dirs(Fs)
		    andalso gone(Fs, Name)
	    end).

prop_tree() ->
    ?FORALL(Names, folders(),
	    begin
		{ok, Fs} = erlmemfs:start_link(),
		lists:map(fun(Name) ->
				  erlmemfs:make_directory(Fs, Name),
				  erlmemfs:change_directory(Fs, Name)
			  end,
			  Names),
		length(Names) =:= count_dirs(Fs)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

make(Fs, Name) ->
    {ok, Name} =:= erlmemfs:make_directory(Fs, Name).

remove(Fs, Name) ->
    {ok, Name} =:= erlmemfs:remove_directory(Fs, Name).

gone(Fs, Name) ->
    {error, directory_missing} =:= erlmemfs:remove_directory(Fs, Name).

count_dirs(Fs) ->
    {ok, #{dir := N}} = erlmemfs:count(Fs),
    N.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
folder() ->
    ?SUCHTHAT(Folder, non_empty(string()), not invalid(Folder)).

folders() ->
    non_empty(list(folder())).

invalid(Folder) ->
    lists:member($/, Folder) or (Folder =:= ".") or (Folder =:= "..").
