%%
%% Test suite for eiconv...
%%

-module(iconv_test).

-include_lib("eunit/include/eunit.hrl").

conv(C, I) ->
    {ok, R} = iconv:conv(C, I),
    R.

%% Some simple tests...
ascii_to_utf8_test() ->
    %% The encodings could be binaries and lists. Input also
    {ok, C} = iconv:open("utf8", <<"ascii">>),
    ?assertEqual(<<"123">>, conv(C, "123")),
    ?assertEqual(<<"123">>, conv(C, ["123"])),
    ?assertEqual(<<"123">>, conv(C, <<"123">>)).

iso8859_15_to_utf8_test() ->
    {ok, C} = iconv:open(<<"utf8">>, "iso8859-15"),
    ?assertEqual(<<194, 163,  226, 130, 172,  194, 165>>, conv(C, [163, 164, 165])).
    
utf8_to_iso8859_15_test() ->
    {ok, C} = iconv:open("iso8859-15", "utf8"),
    ?assertEqual(<<163,  164, 165>>, conv(C, [194, 163, 226, 130, 172, 194, 165])),
    ok.

iolist_input_test() ->
    {ok, C} = iconv:open("iso8859-15", "utf8"),
    ?assertEqual(<<163,  164, 165>>, conv(C, <<194, 163, 226, 130, 172, 194, 165>>)),
    ?assertEqual(<<163,  164, 165>>, conv(C, [<<194, 163, 226>>, 130, [172, 194, 165]])),
    ok.

non_existing_codecs_test() ->
    ?assertEqual({error, einval}, iconv:open("utf-9", "utf-15")),
    ?assertEqual({error, einval}, iconv:open("utf-8", "iso8859-23")),
    ?assertEqual({error, einval}, iconv:open("iso8859-23", "utf-8")).




    
    
    

