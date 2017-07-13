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

%-define(LOG(Level, Format, Args), lager:Level("LWM2M-JSON: " ++ Format, Args)).
-define(LOG(Level, Format, Args), io:format("LWM2M-JSON: " ++ Format, Args)).





tlv_to_json(BaseName, TlvData) ->
    ObjectId = object_id(BaseName),
    ObjDefinition = emq_lwm2m_xml_object:get_obj_def(ObjectId, true),
    case TlvData of
        [#{tlv_resource_with_value:=Id, value:=Value}] ->
            TrueBaseName = basename(BaseName, undefined, undefined, Id, 3),
            encode_json(TrueBaseName, tlv_single_resource(Id, Value, ObjDefinition));
        List1 = [#{tlv_resource_with_value:=_Id}, _|_] ->
            TrueBaseName = basename(BaseName, undefined, undefined, undefined, 2),
            encode_json(TrueBaseName, tlv_level2(<<>>, List1, ObjDefinition, []));
        List2 = [#{tlv_multiple_resource:=_Id}|_] ->
            TrueBaseName = basename(BaseName, undefined, undefined, undefined, 2),
            encode_json(TrueBaseName, tlv_level2(<<>>, List2, ObjDefinition, []));
        [#{tlv_object_instance:=Id, value:=Value}] ->
            TrueBaseName = basename(BaseName, undefined, Id, undefined, 2),
            encode_json(TrueBaseName, tlv_level1(TrueBaseName, Value, ObjDefinition, []));
        List3=[#{tlv_object_instance:=Id, value:=Value}, _|_] ->
            TrueBaseName = basename(BaseName, Id, undefined, undefined, 1),
            encode_json(TrueBaseName, tlv_level1(TrueBaseName, List3, ObjDefinition, []))
    end.


tlv_level1(_RelativePath, [], _ObjDefinition, Acc) ->
    Acc;
tlv_level1(RelativePath, [#{tlv_object_instance:=_Id, value:=Value}|T], ObjDefinition, Acc) ->
    New = tlv_level2(RelativePath, Value, ObjDefinition, []),
    tlv_level1(RelativePath, T, ObjDefinition, Acc++New).

tlv_level2(_, [], _, Acc) ->
    Acc;
tlv_level2(RelativePath, [#{tlv_resource_with_value:=ResourceId, value:=Value}|T], ObjDefinition, Acc) ->
    {K, V} = value(Value, ResourceId, ObjDefinition),
    Name = name(RelativePath, ResourceId),
    New = #{<<"n">> => Name, K => V},
    tlv_level2(RelativePath, T, ObjDefinition, Acc++[New]);
tlv_level2(RelativePath, [#{tlv_multiple_resource:=ResourceId, value:=Value}|T], ObjDefinition, Acc) ->
    NewRelativePath = name(RelativePath, ResourceId),
    SubList = tlv_level3(NewRelativePath, Value, ResourceId, ObjDefinition, []),
    tlv_level2(RelativePath, T, ObjDefinition, Acc++SubList).

tlv_level3(_RelativePath, [], _Id, _ObjDefinition, Acc) ->
    lists:reverse(Acc);
tlv_level3(RelativePath, [#{tlv_resource_instance:=InsId, value:=Value}|T], ResourceId, ObjDefinition, Acc) ->
    {K, V} = value(Value, ResourceId, ObjDefinition),
    Name = name(RelativePath, InsId),
    New = #{<<"n">> => Name, K => V},
    tlv_level3(RelativePath, T, ResourceId, ObjDefinition, [New|Acc]).

tlv_single_resource(Id, Value, ObjDefinition) ->
    {K, V} = value(Value, Id, ObjDefinition),
    [#{K=>V}].



basename(OldBaseName, ObjectId, ObjectInstanceId, ResourceId, 3) ->
    ?LOG(debug, "basename3 OldBaseName=~p, ObjectId=~p, ObjectInstanceId=~p, ResourceId=~p", [OldBaseName, ObjectId, ObjectInstanceId, ResourceId]),
    case binary:split(OldBaseName, [<<$/>>], [global]) of
        [<<>>, ObjId, ObjInsId, ResId, <<>>] -> <<$/, ObjId/binary, $/, ObjInsId/binary, $/, ResId/binary>>;
        [<<>>, ObjId, ObjInsId, <<>>]        -> <<$/, ObjId/binary, $/, ObjInsId/binary, $/, (integer_to_binary(ResourceId))/binary>>;
        [<<>>, ObjId, ObjInsId, ResId]       -> <<$/, ObjId/binary, $/, ObjInsId/binary, $/, ResId/binary>>;
        [<<>>, ObjId, <<>>]                  -> <<$/, ObjId/binary, $/, (integer_to_binary(ObjectInstanceId))/binary, $/, (integer_to_binary(ResourceId))/binary>>;
        [<<>>, ObjId, ObjInsId]              -> <<$/, ObjId/binary, $/, ObjInsId/binary, $/, (integer_to_binary(ResourceId))/binary>>;
        [<<>>, ObjId]                        -> <<$/, ObjId/binary, $/, (integer_to_binary(ObjectInstanceId))/binary, $/, (integer_to_binary(ResourceId))/binary>>
    end;
basename(OldBaseName, ObjectId, ObjectInstanceId, ResourceId, 2) ->
    ?LOG(debug, "basename2 OldBaseName=~p, ObjectId=~p, ObjectInstanceId=~p, ResourceId=~p", [OldBaseName, ObjectId, ObjectInstanceId, ResourceId]),
    case binary:split(OldBaseName, [<<$/>>], [global]) of
        [<<>>, ObjId, ObjInsId, _ResId, <<>>] -> <<$/, ObjId/binary, $/, ObjInsId/binary>>;
        [<<>>, ObjId, ObjInsId, _ResId]       -> <<$/, ObjId/binary, $/, ObjInsId/binary>>;
        [<<>>, ObjId, <<>>]                   -> <<$/, ObjId/binary, $/, (integer_to_binary(ObjectInstanceId))/binary>>;
        [<<>>, ObjId, ObjInsId]               -> <<$/, ObjId/binary, $/, ObjInsId/binary>>;
        [<<>>, ObjId]                         -> <<$/, ObjId/binary, $/, (integer_to_binary(ObjectInstanceId))/binary>>
    end;
basename(OldBaseName, ObjectId, ObjectInstanceId, ResourceId, 1) ->
    ?LOG(debug, "basename1 OldBaseName=~p, ObjectId=~p, ObjectInstanceId=~p, ResourceId=~p", [OldBaseName, ObjectId, ObjectInstanceId, ResourceId]),
    case binary:split(OldBaseName, [<<$/>>], [global]) of
        [<<>>, ObjId, _ObjInsId, _ResId, <<>>] -> <<$/, ObjId/binary>>;
        [<<>>, ObjId, _ObjInsId, _ResId]       -> <<$/, ObjId/binary>>;
        [<<>>, ObjId, _ObjInsId]               -> <<$/, ObjId/binary>>;
        [<<>>, ObjId]                          -> <<$/, ObjId/binary>>
    end.


name(RelativePath, Id) ->
    case RelativePath of
        <<>> -> integer_to_binary(Id);
        _    -> <<RelativePath/binary, $/, (integer_to_binary(Id))/binary>>
    end.


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
            {<<"v">>, integer_to_binary(IntResult)};
        "Float" ->
            <<FloatResult:32/float>> = Value,
            {<<"v">>, float_to_binary(FloatResult)};
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
            {<<"v">>, integer_to_binary(IntResult)};
        "Objlnk" ->
            <<ObjId:16, ObjInsId:16>> = Value,
            {<<"ov">>, io_lib:format("~b:~b", [ObjId, ObjInsId])}
    end.


encode_json(BaseName, E) ->
    jsx:encode(#{bn=>BaseName, e=>E}).
