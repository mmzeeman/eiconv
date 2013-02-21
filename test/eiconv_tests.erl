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
    ?assertEqual(<<194, 163,  226, 130, 172,  194, 165>>, 
        conv(C, [163, 164, 165])).

iso8859_15_to_utf8_chunks_test() ->
    % convert byte for byte
    Chunk = fun(C, I) ->
        {ok, R} = eiconv:conv(C, I, 1),
        R
    end,

    {ok, C1} = eiconv:open("utf8", "iso8859-15"),
    ?assertEqual(<<194, 163,  226, 130, 172,  194, 165>>, Chunk(C1, [163, 164, 165])),

    {ok, C2} = eiconv:open("iso8859-15", "utf8"),
    ?assertEqual(<<163,  164, 165>>, Chunk(C2, [194, 163, 226, 130, 172, 194, 165])),
    ok.

utf8_to_iso8859_15_test() ->
    {ok, C} = eiconv:open("iso8859-15", "utf8"),
    ?assertEqual(<<163,  164, 165>>, conv(C, [194, 163, 226, 130, 172, 194, 165])).

non_existing_codecs_test() ->
    ?assertEqual({error, einval}, eiconv:open("utf-9", "utf-15")),
    ?assertEqual({error, einval}, eiconv:open("utf-8", "iso8859-23")),
    ?assertEqual({error, einval}, eiconv:open("iso8859-23", "utf-8")).

convert_encoding_test() ->
    ?assertEqual({error, einval}, eiconv:convert("utf-9", "Test, 123")),
    ?assertEqual({error, einval}, eiconv:convert("utf-8", "non-existent", "Test, 123")),
    ?assertEqual({ok, <<"123">>}, eiconv:convert("utf-8", "ascii", "123")),
    ?assertEqual({ok, <<"123">>}, eiconv:convert("ascii", "123")).

iso8859_15_to_utf8_chunk_test() ->
    {ok, C} = eiconv:open("utf8", "iso8859-15"),
    ?assertEqual({done,<<194,163,226,130,172,194,165>>}, 
        eiconv:chunk(C, [163, 164, 165])),
    ?assertEqual(ok, eiconv:finalize(C)),

    %% Converting in chunks
    ?assertEqual({done,<<194,163>>}, eiconv:chunk(C, [163])),
    ?assertEqual({done,<<226,130,172>>}, eiconv:chunk(C, [164])),
    ?assertEqual({done,<<194,165>>}, eiconv:chunk(C, [165])),
    ?assertEqual(ok, eiconv:finalize(C)),
    ok.

incomplete_chunk_test() ->
    {ok, C} = eiconv:open("iso8859-15", "utf8"),
    ?assertEqual({more, <<"AB">>}, eiconv:chunk(C, <<65, 66, 194>>)),
    ?assertEqual({done, <<163>>}, eiconv:chunk(C, <<163>>)),
    ?assertEqual({more, <<>>}, eiconv:chunk(C, <<226, 130>>)),
    ?assertEqual({done, <<164>>}, eiconv:chunk(C, <<172>>)),
    ?assertEqual({done, <<165>>}, eiconv:chunk(C, [194, 165])),
    ?assertEqual(ok, eiconv:finalize(C)),
    ok.
