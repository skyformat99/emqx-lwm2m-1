%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 EMQ Enterprise, Inc. (http://emqtt.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_lwm2m_tlv).

-author("Feng Lee <feng@emqtt.io>").

-export([parse/1]).

-include("emq_lwm2m.hrl").

%-define(LOG(Level, Format, Args), lager:Level("LWM2M-TLV: " ++ Format, Args)).
-define(LOG(Level, Format, Args), io:format("LWM2M-TLV: " ++ Format, Args)).

-define(TLV_TYPE_OBJECT_INSTANCE,     0).
-define(TLV_TYPE_RESOURCE_INSTANCE,   1).
-define(TLV_TYPE_MULTIPLE_RESOURCE,   2).
-define(TLV_TYPE_RESOURCE_WITH_VALUE, 3).

-define(TLV_NO_LENGTH_FIELD, 0).
-define(TLV_LEGNTH_8_BIT,    1).
-define(TLV_LEGNTH_16_BIT,   2).
-define(TLV_LEGNTH_24_BIT,   3).


parse(Data) ->
    parse_loop(Data, []).

parse_loop(<<>>, Acc)->
    ?LOG(debug, "parse_loop() Acc=~p", [Acc]),
    lists:reverse(Acc);
parse_loop(Data, Acc) ->
    ?LOG(debug, "parse_loop() Data=~p, Acc=~p", [binary_to_hex_string(Data), Acc]),
    {New, Rest} = parse_step1(Data),
    parse_loop(Rest, [New|Acc]).

parse_step1(<<?TLV_TYPE_OBJECT_INSTANCE:2, IdLength:1, LengthType:2, InlineLength:3, Rest/binary>>) ->
    ?LOG(debug, "parse_step1 1 input IdLength=~p, LengthType=~p, InlineLength=~p, Rest=~p", [IdLength, LengthType, InlineLength, binary_to_hex_string(Rest)]),
    {Id, Value, Rest2} = parse_step2(id_length_bit_width(IdLength), LengthType, InlineLength, Rest),
    ?LOG(debug, "parse_step1 1 return Id=~p, Rest=~p", [Id, binary_to_hex_string(Rest2)]),
    {#{<<"tlv_object_instance">> => Id,  <<"value">> => parse_loop(Value, [])}, Rest2};
parse_step1(<<?TLV_TYPE_RESOURCE_INSTANCE:2, IdLength:1, LengthType:2, InlineLength:3, Rest/binary>>) ->
    ?LOG(debug, "parse_step1 2 IdLength=~p, LengthType=~p, InlineLength=~p, Rest=~p", [IdLength, LengthType, InlineLength, binary_to_hex_string(Rest)]),
    {Id, Value, Rest2} = parse_step2(id_length_bit_width(IdLength), LengthType, InlineLength, Rest),
    ?LOG(debug, "parse_step1 2 return Id=~p, Value=~p, Rest=~p", [Id, Value, binary_to_hex_string(Rest2)]),
    {#{<<"tlv_resource_instance">> => Id, <<"value">> => Value}, Rest2};
parse_step1(<<?TLV_TYPE_MULTIPLE_RESOURCE:2, IdLength:1, LengthType:2, InlineLength:3, Rest/binary>>) ->
    ?LOG(debug, "parse_step1 3 IdLength=~p, LengthType=~p, InlineLength=~p, Rest=~p", [IdLength, LengthType, InlineLength, binary_to_hex_string(Rest)]),
    {Id, Value, Rest2} = parse_step2(id_length_bit_width(IdLength), LengthType, InlineLength, Rest),
    ?LOG(debug, "parse_step1 3 return Id=~p, Rest=~p", [Id, binary_to_hex_string(Rest2)]),
    {#{<<"tlv_multiple_resource">> => Id, <<"value">> => parse_loop(Value, [])}, Rest2};
parse_step1(<<?TLV_TYPE_RESOURCE_WITH_VALUE:2, IdLength:1, LengthType:2, InlineLength:3, Rest/binary>>) ->
    ?LOG(debug, "parse_step1 4 IdLength=~p, LengthType=~p, InlineLength=~p, Rest=~p", [IdLength, LengthType, InlineLength, binary_to_hex_string(Rest)]),
    {Id, Value, Rest2} = parse_step2(id_length_bit_width(IdLength), LengthType, InlineLength, Rest),
    ?LOG(debug, "parse_step1 2 return Id=~p, Value=~p Rest=~p", [Id, Value, binary_to_hex_string(Rest2)]),
    {#{<<"tlv_resource_with_value">> => Id,  <<"value">> => Value}, Rest2}.

parse_step2(IdLength, ?TLV_NO_LENGTH_FIELD, Length, Data) ->
    ?LOG(debug, "parse_step2 IdLength=~p, Length=~p, Data=~p", [IdLength, Length, binary_to_hex_string(Data)]),
    <<Id:IdLength, Value:Length/binary, Rest/binary>> = Data,
    {Id, Value, Rest};
parse_step2(IdLength, ?TLV_LEGNTH_8_BIT, _, Data) ->
    ?LOG(debug, "parse_step2 8bit IdLength=~p, Data=~p", [IdLength, binary_to_hex_string(Data)]),
    <<Id:IdLength, Length:8, Rest/binary>> = Data,
    parse_step3(Id, Length, Rest);
parse_step2(IdLength, ?TLV_LEGNTH_16_BIT, _, Data) ->
    ?LOG(debug, "parse_step2 16bit IdLength=~p, Data=~p", [IdLength, binary_to_hex_string(Data)]),
    <<Id:IdLength, Length:16, Rest/binary>> = Data,
    parse_step3(Id, Length, Rest);
parse_step2(IdLength, ?TLV_LEGNTH_24_BIT, _, Data) ->
    ?LOG(debug, "parse_step2 24bit IdLength=~p, Data=~p", [IdLength, binary_to_hex_string(Data)]),
    <<Id:IdLength, Length:24, Rest/binary>> = Data,
    parse_step3(Id, Length, Rest).

parse_step3(Id, Length, Data) ->
    <<Value:Length/binary, Rest/binary>> = Data,
    ?LOG(debug, "parse_step3 Id=~p, Length=~p, Data=~p, Value=~p, Rest=~p", [Id, Length, binary_to_hex_string(Data), binary_to_hex_string(Value), binary_to_hex_string(Rest)]),
    {Id, Value, Rest}.

id_length_bit_width(0) -> 8;
id_length_bit_width(1) -> 16.




binary_to_hex_string(Data) ->
    lists:flatten([io_lib:format("~2.16.0B ",[X]) || <<X:8>> <= Data ]).


