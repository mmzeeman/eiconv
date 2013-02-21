% Copyright 2011, 2012, 2013 Maas-Maarten Zeeman
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
%     http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(eiconv).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

% easy api one shot convert api
-export([convert/2, convert/3]).

% old iconv compatible api
-export([open/2, conv/2, conv/3, close/1]).

% low level api.
-export([chunk/2, finalize/1]).

-define(DEFAULT_CHUNK_SIZE, 1024 * 128).

-on_load(init/0).

% @doc Load the nif
%
init() ->
    NifName = "eiconv_nif",
    NifFileName = case code:priv_dir(eiconv) of
        {error, bad_name} -> filename:join(["priv", NifName]);
        Dir -> filename:join([Dir, NifName])
    end,

    ok = erlang:load_nif(NifFileName, 0).

% @doc Open a new encoder which can be used to convert text from FromCode into ToCode.
%
open(_ToCode, _FromCode) ->
    exit(nif_library_not_loaded).

% @doc Convert a chunk, returns {done, ConvertedBytes} } | {more, Converted}
%
chunk(_Cd, _Input) ->
    exit(nif_library_not_loaded).

% @doc Reset the cd structure, returns ok | {rest, LeftOverBytes}
%
finalize(_Cd) ->
    exit(nif_library_not_loaded).

% @doc Convert Input into the requested encoding.
%
conv(Cd, Input) ->
    conv(Cd, Input, ?DEFAULT_CHUNK_SIZE).

% @doc Convert input. The input will first be split into 
% chunks of ChunkSize before being converted by the nif.
%
conv(Cd, Input, ChunkSize) ->
    Chunks = split_input(ChunkSize, Input),
    case conv1(Cd, Chunks, []) of
        {ok, Converted} -> 
            {ok, erlang:iolist_to_binary(Converted)};
        {error,_}=Error -> 
            Error
    end.
    
conv1(Cd, [], Acc) ->
    case finalize(Cd) of
        ok ->
            {ok, lists:reverse(Acc)};
        {rest, _} ->
            {error, einval}
    end;
conv1(Cd, [H|T], Acc) ->
    case chunk(Cd, H) of
        {error, _}=Error ->
            Error;
        {What, <<>>} when What =:= done orelse What =:= more ->
            conv1(Cd, T, Acc);
        {What, Result} when What =:= done orelse What =:= more ->
            conv1(Cd, T, [Result | Acc])
    end.

% @doc Close the encoder - dummy function, close will be done by the garbage collector.
%
close(_Cd) ->
    ok.

% @doc Convert input FromEncoding to utf-8
%
convert(FromEncoding, Input) ->
    convert(FromEncoding, "utf-8", Input).

% @doc Convert input which is in FromEncoding to ToEncoding.
%
convert(FromEncoding, ToEncoding, Input) ->
    case open(ToEncoding, FromEncoding) of
        {ok, Cd} ->
            conv(Cd, Input);
        {error, _}=Error ->
            Error
    end.

%% Helpers
split_input(MaxSize, Input) when MaxSize > 0 ->
    case iolist_to_binary(Input) of
        <<Chunk:MaxSize/binary, Rest/binary>> ->
            [Chunk | split_input(MaxSize, Rest)];
        Chunk -> [Chunk]
    end.

