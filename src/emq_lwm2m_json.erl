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

-module(emq_lwm2m_json).

-author("Feng Lee <feng@emqtt.io>").

-export([tlv_to_json/2]).

-include("emq_lwm2m.hrl").

-define(LOG(Level, Format, Args), lager:Level("LWM2M-JSON: " ++ Format, Args)).






tlv_to_json(BaseName, TlvData) ->
    ObjectId = object_id(BaseName),
    ObjDefinition = emq_lwm2m_xml_object:get_obj_def(ObjectId, true),
    List = tlv_loop(<<>>, TlvData, ObjDefinition, []),
    jsx:encode(#{<<"bn">>=>BaseName, <<"e">>=>List}).

tlv_loop(_RelativePath, [], _ObjDefinition, Acc) ->
    lists:reverse(Acc);
tlv_loop(RelativePath, [#{tlv_object_instance:=Id, value:=Value}|T], ObjDefinition, Acc) ->
    New = tlv_loop(<<(integer_to_binary(Id))/binary, $/>>, Value, ObjDefinition, []),
    tlv_loop(RelativePath, T, ObjDefinition, New++Acc);
tlv_loop(RelativePath, [#{tlv_resource_with_value:=Id, value:=Value}|T], ObjDefinition, Acc) ->
    {K, V} = value(Value, Id, ObjDefinition),
    New = #{<<"n">> => <<RelativePath/binary, $/, (integer_to_binary(Id))/binary>>, K => V},
    tlv_loop(RelativePath, T, ObjDefinition, [New|Acc]);
tlv_loop(RelativePath, [#{tlv_multiple_resource:=Id, value:=Value}|T], ObjDefinition, Acc) ->
    SubList = tlv_loop(<<RelativePath/binary, $/, (integer_to_binary(Id))/binary>>, Value, ObjDefinition, []),
    tlv_loop(RelativePath, T, ObjDefinition, SubList++Acc);
tlv_loop(RelativePath, [#{tlv_resource_instance:=Id, value:=Value}|T], ObjDefinition, Acc) ->
    {K, V} = value(Value, Id, ObjDefinition),
    New = #{<<"n">> => <<RelativePath/binary, $/, (integer_to_binary(Id))/binary>>, K => V},
    tlv_loop(RelativePath, T, ObjDefinition, [New|Acc]).

object_id(BaseName) ->
    case binary:split(BaseName, [<<$/>>], [global]) of
        [<<>>, ObjIdBin1]       -> binary_to_integer(ObjIdBin1);
        [<<>>, ObjIdBin2, _]    -> binary_to_integer(ObjIdBin2);
        [<<>>, ObjIdBin3, _, _] -> binary_to_integer(ObjIdBin3)
    end.


value(Value, ResourceId, ObjDefinition) ->
    case emq_lwm2m_xml_object:get_resource_type(ResourceId, ObjDefinition) of
        "String" ->
            {<<"sv">>, Value};  % keep binary type since it is same as a string for jsx
        "Integer" ->
            Size = byte_size(Value)*8,
            <<IntResult:Size>> = Value,
            {<<"v">>, IntResult};
        "Float" ->
            <<FloatResult:32/float>> = Value,
            {<<"v">>, FloatResult};
        "Boolean" ->
            B = case Value of
                    <<0>> -> false;
                    <<1>> -> true
                end,
            {<<"bv">>, B};
        "Opaque" ->
            {<<"sv">>, base64:encode(Value)};
        "Time" ->
            Size = byte_size(Value)*8,
            <<IntResult:Size>> = Value,
            {<<"v">>, IntResult};
        "Objlnk" ->
            <<ObjId:16, ObjInsId:16>> = Value,
            {<<"ov">>, io_lib:format("~b:~b", [ObjId, ObjInsId])}
    end.

