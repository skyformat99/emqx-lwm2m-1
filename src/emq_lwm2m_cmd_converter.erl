%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 Feng Lee <feng@emqtt.io>. All Rights Reserved.
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

-module(emq_lwm2m_cmd_converter).

-author("Feng Lee <feng@emqtt.io>").

-include("emq_lwm2m.hrl").
-include_lib("gen_coap/include/coap.hrl").

-export([mqtt_payload_to_coap_request/1, coap_response_to_mqtt_payload/3]).


-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-CNVT: " ++ Format, Args)).


mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Read">>}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    {coap_message:request(con, get, <<>>, [{uri_path, Path}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Write">>, ?MQ_VALUE := Value}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    Method = case ResourceId of
                 undefined -> post;
                 _Other    -> put
             end,
    Payload =   if
                    is_integer(Value) -> list_to_binary(io_lib:format("~b", [Value]));
                    is_float(Value)   -> list_to_binary(io_lib:format("~f", [Value]));
                    is_list(Value)    -> list_to_binary(Value);
                    is_binary(Value)  -> Value
                end,
    {coap_message:request(con, Method, Payload, [{uri_path, Path}]), InputCmd}.

coap_response_to_mqtt_payload(CoapPayload, <<"text/plain">>, #{?MQ_COMMAND_ID := CmdId, ?MQ_OBJECT_ID := ObjId, ?MQ_OBJECT_INSTANCE_ID := ObjInsId, ?MQ_RESOURCE_ID := ResId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    jsx:encode(#{?MQ_COMMAND_ID => CmdId,
                 ?MQ_OBJECT_ID => ObjId,
                 ?MQ_OBJECT_INSTANCE_ID => ObjInsId,
                 ?MQ_RESULT => #{?MQ_RESOURCE_ID => ResId, ?MQ_VALUE_TYPE => <<"text">>,  ?MQ_VALUE => CoapPayload}});
coap_response_to_mqtt_payload(CoapPayload, <<"application/octet-stream">>, #{?MQ_COMMAND_ID := CmdId, ?MQ_OBJECT_ID := ObjId, ?MQ_OBJECT_INSTANCE_ID := ObjInsId, ?MQ_RESOURCE_ID := ResId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    Data = base64:encode(CoapPayload),
    jsx:encode(#{?MQ_COMMAND_ID => CmdId,
                 ?MQ_OBJECT_ID => ObjId,
                 ?MQ_OBJECT_INSTANCE_ID => ObjInsId,
                 ?MQ_RESULT => #{?MQ_RESOURCE_ID => ResId, ?MQ_VALUE_TYPE => <<"binary">>,  ?MQ_VALUE => Data}});
coap_response_to_mqtt_payload(CoapPayload, <<"application/vnd.oma.lwm2m+tlv">>, #{?MQ_COMMAND_ID := CmdId, ?MQ_OBJECT_ID := ObjId, ?MQ_OBJECT_INSTANCE_ID := ObjInsId, ?MQ_RESOURCE_ID := ResId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    % todo: support tls
    error("not support tls"),
    jsx:encode(#{?MQ_COMMAND_ID => CmdId,
        ?MQ_OBJECT_ID => ObjId,
        ?MQ_OBJECT_INSTANCE_ID => ObjInsId,
        ?MQ_RESULT => #{?MQ_RESOURCE_ID => ResId, ?MQ_VALUE_TYPE => <<"text">>,  ?MQ_VALUE => CoapPayload}});
coap_response_to_mqtt_payload(CoapPayload, <<"application/vnd.oma.lwm2m+json">>, #{?MQ_COMMAND_ID := CmdId, ?MQ_OBJECT_ID := ObjId, ?MQ_OBJECT_INSTANCE_ID := ObjInsId, ?MQ_RESOURCE_ID := ResId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    % todo: support json
    error("not support json"),
    jsx:encode(#{?MQ_COMMAND_ID => CmdId,
        ?MQ_OBJECT_ID => ObjId,
        ?MQ_OBJECT_INSTANCE_ID => ObjInsId,
        ?MQ_RESULT => #{?MQ_RESOURCE_ID => ResId, ?MQ_VALUE_TYPE => <<"text">>,  ?MQ_VALUE => CoapPayload}});
coap_response_to_mqtt_payload(CoapPayload, DataFormat, #{?MQ_COMMAND_ID := CmdId, ?MQ_OBJECT_ID := ObjId, ?MQ_OBJECT_INSTANCE_ID := ObjInsId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    % TODO: CoapPayload is a tlv or json
    error("not support @7830682240"),
    jsx:encode(#{?MQ_COMMAND_ID => CmdId,
                 ?MQ_OBJECT_ID => ObjId,
                 ?MQ_OBJECT_INSTANCE_ID => ObjInsId,
                 ?MQ_RESULT => #{?MQ_RESOURCE_ID => CmdId, ?MQ_VALUE => CoapPayload}}).




build_path({ObjectName, undefined, undefined}) ->
    ObjDef = emq_lwm2m_xml_object:get_obj_def(ObjectName, false),
    Oid = emq_lwm2m_xml_object:get_object_id(ObjDef),
    make_path("/~s", [Oid]);
build_path({ObjectName, ObjectInstanceId, undefined}) ->
    ObjDef = emq_lwm2m_xml_object:get_obj_def(ObjectName, false),
    Oid = emq_lwm2m_xml_object:get_object_id(ObjDef),
    make_path("/~s/~b", [Oid, ObjectInstanceId]);
build_path({ObjectName, ObjectInstanceId, ResourceId}) ->
    ObjDef = emq_lwm2m_xml_object:get_obj_def(ObjectName, false),
    {Oid, Rid} = emq_lwm2m_xml_object:get_object_and_resource_id(ResourceId, ObjDef),
    make_path("/~s/~b/~s", [Oid, ObjectInstanceId, Rid]).


make_path(Format, Args) ->
    [list_to_binary(lists:flatten(io_lib:format(Format, Args)))].


get_oid_rid(MqttPayload) ->
    ?LOG(debug, "get_oid_rid() MqttPayload=~p", [MqttPayload]),
    ObjectId         = maps:get(?MQ_OBJECT_ID, MqttPayload, undefined),
    ObjectInstanceId = maps:get(?MQ_OBJECT_INSTANCE_ID, MqttPayload, undefined),
    ResourceId       = maps:get(?MQ_RESOURCE_ID, MqttPayload, undefined),
    % they are all binary type
    {ObjectId, ObjectInstanceId, ResourceId}.




