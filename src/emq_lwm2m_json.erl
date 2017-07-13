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

-export([tlv_to_json/2, json_to_tlv/1]).

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
            encode_json(TrueBaseName, tlv_level2(<<>>, Value, ObjDefinition, []));
        List3=[#{tlv_object_instance:=Id, value:=Value}, _|_] ->
            TrueBaseName = basename(BaseName, Id, undefined, undefined, 1),
            encode_json(TrueBaseName, tlv_level1(List3, ObjDefinition, []))
    end.


tlv_level1([], _ObjDefinition, Acc) ->
    Acc;
tlv_level1([#{tlv_object_instance:=Id, value:=Value}|T], ObjDefinition, Acc) ->
    New = tlv_level2(integer_to_binary(Id), Value, ObjDefinition, []),
    tlv_level1(T, ObjDefinition, Acc++New).

tlv_level2(_, [], _, Acc) ->
    Acc;
tlv_level2(RelativePath, [#{tlv_resource_with_value:=ResourceId, value:=Value}|T], ObjDefinition, Acc) ->
    {K, V} = value(Value, ResourceId, ObjDefinition),
    Name = name(RelativePath, ResourceId),
    New = #{n => Name, K => V},
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
    New = #{n => Name, K => V},
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
            Size = byte_size(Value),
            <<FloatResult:Size/float>> = Value,
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
    ?LOG(debug, "encode_json BaseName=~p, E=~p", [BaseName, E]),
    jsx:encode(#{bn=>BaseName, e=>E}).



json_to_tlv(JsonText) ->
    Json = jsx:decode(JsonText, [return_maps]),
    BaseName = maps:get(<<"bn">>, Json),
    E = maps:get(<<"e">>, Json),
    case split_path(BaseName) of
        [_ObjectId, _ObjectInstanceId, ResourceId] ->
            case length(E) of
                1 -> element_single_resource(ResourceId, E);
                _ -> element_loop_level4(E, [#{tlv_multiple_resource=>ResourceId, value=>[]}])
            end;
        [_ObjectId, _ObjectInstanceId] ->
            element_loop_level3(E, []);
        [_ObjectId] ->
            element_loop_level2(E, [])
    end.

element_single_resource(ResourceId, [H=#{}]) ->
    BinaryValue = value_ex(H),
    [#{tlv_resource_with_value=>ResourceId, value=>BinaryValue}].

element_loop_level2([], Acc) ->
    Acc;
element_loop_level2([H=#{<<"n">>:=Name}|T], Acc) ->
    BinaryValue = value_ex(H),
    Path = split_path(Name),
    ?LOG(debug, "element_loop_level2 T=~p, Acc=~p", [T, Acc]),
    NewAcc = insert_resource_into_object(Path, BinaryValue, Acc),
    ?LOG(debug, "element_loop_level2 NewAcc=~p", [NewAcc]),
    element_loop_level2(T, NewAcc).

element_loop_level3([], Acc) ->
    Acc;
element_loop_level3([H=#{<<"n">>:=Name}|T], Acc) ->
    ?LOG(debug, "element_loop_level3 H=~p, T=~p, Acc=~p", [H, T, Acc]),
    BinaryValue = value_ex(H),
    Path = split_path(Name),
    NewAcc = insert_resource_into_object_instance(Path, BinaryValue, Acc),
    element_loop_level3(T, NewAcc).

element_loop_level4([], Acc) ->
    Acc;
element_loop_level4([H=#{<<"n">>:=Name}|T], Acc) ->
    BinaryValue = value_ex(H),
    Path = split_path(Name),
    NewAcc = insert_resource_instance_into_resource(Path, BinaryValue, Acc),
    element_loop_level4(T, NewAcc).


value_ex(#{<<"v">>:=Value}) ->
    case catch binary_to_integer(Value) of
        {'EXIT',{badarg,_}} -> <<(binary_to_float(Value))/float>>;
        Int                 -> encode_int(Int)
    end;
value_ex(#{<<"sv">>:=Value}) ->
    Value;
value_ex(#{<<"t">>:=Value}) ->
    encode_int(binary_to_integer(Value));
value_ex(#{<<"bv">>:=Value}) ->
    case Value of
        <<"true">>  -> <<1>>;
        <<"false">> -> <<0>>
    end;
value_ex(#{<<"ov">>:=Value}) ->
    [P1, P2] = binary:split(Value, [<<$:>>], [global]),
    <<(binary_to_integer(P1)):16, (binary_to_integer(P2)):16>>.


insert_resource_into_object([ObjectInstanceId, ResourceId, ResourceInstanceId], Value, Acc) ->
    ?LOG(debug, "insert_resource_into_object1 ObjectInstanceId=~p, ResourceId=~p, ResourceInstanceId=~p, Value=~p, Acc=~p", [ObjectInstanceId, ResourceId, ResourceInstanceId, Value, Acc]),
    case find_obj_instance(ObjectInstanceId, Acc) of
        undefined ->
            NewList = insert_resource_into_object_instance([ResourceId, ResourceInstanceId], Value, []),
            Acc ++ [#{tlv_object_instance=>ObjectInstanceId, value=>NewList}];
        ObjectInstance = #{value:=List} ->
            NewList = insert_resource_into_object_instance([ResourceId, ResourceInstanceId], Value, List),
            Acc2 = lists:delete(ObjectInstance, Acc),
            Acc2 ++ [ObjectInstance#{value=>NewList}]
    end;
insert_resource_into_object([ObjectInstanceId, ResourceId], Value, Acc) ->
    ?LOG(debug, "insert_resource_into_object1 ObjectInstanceId=~p, ResourceId=~p, Value=~p, Acc=~p", [ObjectInstanceId, ResourceId, Value, Acc]),
    case find_obj_instance(ObjectInstanceId, Acc) of
        undefined ->
            NewList = insert_resource_into_object_instance([ResourceId], Value, []),
            Acc ++ [#{tlv_object_instance=>ObjectInstanceId, value=>NewList}];
        ObjectInstance = #{value:=List} ->
            NewList = insert_resource_into_object_instance([ResourceId], Value, List),
            Acc2 = lists:delete(ObjectInstance, Acc),
            Acc2 ++ [ObjectInstance#{value=>NewList}]
    end.

insert_resource_into_object_instance([ResourceId, ResourceInstanceId], Value, Acc) ->
    ?LOG(debug, "insert_resource_into_object_instance1() ResourceId=~p, ResourceInstanceId=~p, Value=~p, Acc=~p", [ResourceId, ResourceInstanceId, Value, Acc]),
    case find_resource(ResourceId, Acc) of
        undefined ->
            NewList = insert_resource_instance_into_resource(ResourceInstanceId, Value, []),
            Acc++[#{tlv_multiple_resource=>ResourceId, value=>NewList}];
        Resource = #{value:=List}->
            NewList = insert_resource_instance_into_resource(ResourceInstanceId, Value, List),
            Acc2 = lists:delete(Resource, Acc),
            Acc2 ++ [Resource#{value=>NewList}]
    end;
insert_resource_into_object_instance([ResourceId], Value, Acc) ->
    ?LOG(debug, "insert_resource_into_object_instance2() ResourceId=~p, Value=~p, Acc=~p", [ResourceId, Value, Acc]),
    NewMap = #{tlv_resource_with_value=>ResourceId, value=>Value},
    case find_resource(ResourceId, Acc) of
        undeinfed ->
            Acc ++ [NewMap];
        Resource ->
            Acc2 = lists:delete(Resource, Acc),
            Acc2 ++ [NewMap]
    end.

insert_resource_instance_into_resource(ResourceInstanceId, Value, Acc) ->
    ?LOG(debug, "insert_resource_instance_into_resource() ResourceInstanceId=~p, Value=~p, Acc=~p", [ResourceInstanceId, Value, Acc]),
    NewMap = #{tlv_resource_instance=>ResourceInstanceId, value=>Value},
    case find_resource_instance(ResourceInstanceId, Acc) of
        undeinfed ->
            Acc ++ [NewMap];
        Resource ->
            Acc2 = lists:delete(Resource, Acc),
            Acc2 ++ [NewMap]
    end.


find_obj_instance(_ObjectInstanceId, []) ->
    undefined;
find_obj_instance(ObjectInstanceId, [H=#{tlv_object_instance:=ObjectInstanceId}|_T]) ->
    H;
find_obj_instance(ObjectInstanceId, [#{tlv_object_instance:=_OtherId}|T]) ->
    find_obj_instance(ObjectInstanceId, T).

find_resource(_ResourceId, []) ->
    undefined;
find_resource(ResourceId, [H=#{tlv_resource_with_value:=ResourceId}|_T]) ->
    H;
find_resource(ResourceId, [#{tlv_resource_with_value:=_OtherId}|T]) ->
    find_resource(ResourceId, T);
find_resource(ResourceId, [H=#{tlv_multiple_resource:=ResourceId}|_T]) ->
    H;
find_resource(ResourceId, [#{tlv_multiple_resource:=_OtherId}|T]) ->
    find_resource(ResourceId, T).

find_resource_instance(_ResourceInstanceId, []) ->
    undefined;
find_resource_instance(ResourceInstanceId, [H=#{tlv_resource_instance:=ResourceInstanceId}|_T]) ->
    H;
find_resource_instance(ResourceInstanceId, [#{tlv_resource_instance:=_OtherId}|T]) ->
    find_resource_instance(ResourceInstanceId, T).

split_path(Path) ->
    List = binary:split(Path, [<<$/>>], [global]),
    path(List, []).

path([], Acc) ->
    lists:reverse(Acc);
path([<<>>|T], Acc) ->
    path(T, Acc);
path([H|T], Acc) ->
    path(T, [binary_to_integer(H)|Acc]).


encode_int(Int) ->
    if
        Int >= 65536 -> <<Int:24>>;
        Int >= 256   -> <<Int:16>>;
        true         -> <<Int:8>>
    end.


