-module(prop_ls).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_ls_test() ->
    ?FORALL(Items, items(),
	    begin
		{ok, Fs} = erlmemfs:start_link(),
		make_items(Fs, Items),
		{ok, Ls} = erlmemfs:list_files(Fs),
		length(Items) =:= length(Ls) andalso same(Items, Ls)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

make_items(_Fs, []) ->
    ok;

make_items(Fs, [{file, Name}|T]) ->
    erlmemfs:put_file(Fs, Name, <<"not important">>),
    make_items(Fs, T);

make_items(Fs, [{dir, Name}|T]) ->
    erlmemfs:make_directory(Fs, Name),
    make_items(Fs, T).

names(Items) ->
    names(Items, []).

names([], Acc) ->
    Acc;
names([{_, Name}|T], Acc) ->
    names(T, [Name|Acc]).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

unique(Items) ->
    N = length(Items),
    M = sets:size(sets:from_list(Items)),
    N =:= M.

item() ->
    {oneof([file, dir]), non_empty(name())}.

items() ->
    ?SUCHTHAT(Items, non_empty(list(item())), unique(names(Items))).

same(A, B) ->
    lists:sort(A) =:= lists:sort(B).

name() ->
    ?SUCHTHAT(Name, non_empty(string()), not invalid(Name)).

invalid(Name) ->
    lists:member($/, Name) or (Name =:= ".") or (Name =:= "..").
