%%
%%
%%

-module(eiconv).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([open/2, conv/2, close/1]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif(code:priv_dir(eiconv) ++ "/eiconv_nif", 0).

open(_ToCode, _FromCode) ->
    exit(nif_library_not_loaded).

conv(_Cd, _Input) ->
    exit(nif_library_not_loaded).

close(_Cd) ->
    exit(nif_library_not_loaded).


