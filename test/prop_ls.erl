-module(prop_ls).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Items, items(),
	    begin
		{ok, Fs} = erlmemfs:start_link(),
		make_items(Fs, Items),
		{ok, Ls} = erlmemfs:list_files(Fs),
		length(Items) =:= length(Ls) andalso same(names(Items), Ls)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

make_items(_Fs, []) ->
    ok;

make_items(Fs, [{Name, f}|T]) ->
    erlmemfs:put_file(Fs, Name, <<"not important">>),
    make_items(Fs, T);

make_items(Fs, [{Name, d}|T]) ->
    erlmemfs:make_directory(Fs, Name),
    make_items(Fs, T).

names(Items) ->
    names(Items, []).

names([], Acc) ->
    Acc;
names([{Name, _}|T], Acc) ->
    names(T, [Name|Acc]).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

unique(Items) ->
    N = length(Items),
    M = sets:size(sets:from_list(Items)),
    N =:= M.

item() ->
    {non_empty(string()), oneof([f, d])}.

items() ->
    ?SUCHTHAT(Items, non_empty(list(item())), unique(names(Items))).

same(A, B) ->
    lists:sort(A) =:= lists:sort(B).
