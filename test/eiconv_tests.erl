%%
%% Test suite for eiconv...
%%

-module(eiconv_tests).

-include_lib("eunit/include/eunit.hrl").

conv(C, I) ->
    {ok, R} = eiconv:conv(C, I),
    R.

%% Some simple tests...
ascii_to_utf8_test() ->
    {ok, C} = eiconv:open("utf8", "ascii"),
    ?assertEqual(<<"123">>, conv(C, "123")),
    ?assertEqual(<<"123">>, conv(C, ["123"])),
    ?assertEqual(<<"123">>, conv(C, <<"123">>)).

iso8859_15_to_utf8_test() ->
    {ok, C} = eiconv:open("utf8", "iso8859-15"),
    ?assertEqual([194, 163,  226, 130, 172,  194, 165], binary_to_list(conv(C, [163, 164, 165]))).
    
utf8_to_iso8859_15_test() ->
    {ok, C} = eiconv:open("iso8859-15", "utf8"),
    ?assertEqual([163,  164, 165], binary_to_list(conv(C, [194, 163, 226, 130, 172, 194, 165]))).

non_existing_codecs_test() ->
    %% ?assertEqual({error, einval}, eiconv:open("", "")),
    ?assertEqual({error, einval}, eiconv:open("utf-9", "utf-15")),
    ?assertEqual({error, einval}, eiconv:open("utf-8", "iso8859-23")),
    ?assertEqual({error, einval}, eiconv:open("iso8859-23", "utf-8")).

convert_encoding_test() ->
    ?assertEqual({error, einval}, eiconv:convert("utf-9", "Test, 123")),
    ?assertEqual({error, einval}, eiconv:convert("utf-8", "non-existent", "Test, 123")),
    ?assertEqual({ok, <<"123">>}, eiconv:convert("utf-8", "ascii", "123")),
    ?assertEqual({ok, <<"123">>}, eiconv:convert("ascii", "123")).

%%garbage_collection_test() ->
%%    true = erlang:garbage_collect(),
%%    Before = erlang:memory(),
%%    ?assertEqual(ok, stress_open_conv(100000)),
%%    true = erlang:garbage_collect(),
%%    AfterCollect = erlang:memory(),
%%    ?assertEqual(Before, AfterCollect).

%%stress_open_conv(0) ->
%%    ok;
%%stress_open_conv(N) ->
%%    {ok, C} = eiconv:open("iso8859-15", "utf8"),
%%    [163,  164, 165] = binary_to_list(conv(C, [194, 163, 226, 130, 172, 194, 165])),
%%    stress_open_conv(N-1).


    
    
    

