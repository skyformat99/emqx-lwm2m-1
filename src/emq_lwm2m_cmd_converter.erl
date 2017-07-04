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

coap_response_to_mqtt_payload(CoapPayload, <<"text/plain">>, Ref=#{?MQ_COMMAND_ID := CmdId, ?MQ_RESOURCE_ID := _ResId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    make_resource_json(Ref, <<"text">>, CoapPayload);
coap_response_to_mqtt_payload(CoapPayload, <<"text/plain">>, #{?MQ_COMMAND_ID := CmdId}) ->
    ?LOG(error, "coap_response_to_mqtt_payload discard CoapPayload=~p, CmdId=~p due to missing resource id", [CoapPayload, CmdId]),
    error("plain text needs a resource id");
coap_response_to_mqtt_payload(CoapPayload, <<"application/octet-stream">>, Ref=#{?MQ_COMMAND_ID := CmdId, ?MQ_RESOURCE_ID := _ResId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    Data = base64:encode(CoapPayload),
    make_resource_json(Ref, <<"binary">>, Data);
coap_response_to_mqtt_payload(CoapPayload, <<"application/octet-stream">>, #{?MQ_COMMAND_ID := CmdId}) ->
    ?LOG(error, "coap_response_to_mqtt_payload discard CoapPayload=~p, CmdId=~p due to missing resource id", [CoapPayload, CmdId]),
    error("opaque needs a resource id");
coap_response_to_mqtt_payload(CoapPayload, <<"application/vnd.oma.lwm2m+tlv">>, Ref=#{?MQ_COMMAND_ID := CmdId, ?MQ_RESOURCE_ID := _ResId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    % todo: support tls
    error("not support tlv"),
    make_resource_json(Ref, <<"json">>, CoapPayload);
coap_response_to_mqtt_payload(CoapPayload, <<"application/vnd.oma.lwm2m+json">>, Ref=#{?MQ_COMMAND_ID := CmdId, ?MQ_RESOURCE_ID := _ResId}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload CoapPayload=~p, CmdId=~p", [CoapPayload, CmdId]),
    % todo: support tls
    error("not support tlv"),
    make_resource_json(Ref, <<"json">>, CoapPayload).



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

make_resource_json(Ref=#{}, Type, Value) ->
    jsx:encode(#{?MQ_COMMAND_ID => maps:get(?MQ_COMMAND_ID, Ref),
                 ?MQ_OBJECT_ID => maps:get(?MQ_OBJECT_ID, Ref),
                 ?MQ_OBJECT_INSTANCE_ID => maps:get(?MQ_OBJECT_INSTANCE_ID, Ref),
                 ?MQ_RESULT => #{?MQ_RESOURCE_ID => maps:get(?MQ_RESOURCE_ID, Ref),
                                 ?MQ_VALUE_TYPE => Type,
                                 ?MQ_VALUE => Value}}).

make_object_json(Ref=#{}, Type, Value) ->
    jsx:encode(#{?MQ_COMMAND_ID => maps:get(?MQ_COMMAND_ID, Ref),
                 ?MQ_OBJECT_ID => maps:get(?MQ_OBJECT_ID, Ref),
                 ?MQ_OBJECT_INSTANCE_ID => maps:get(?MQ_OBJECT_INSTANCE_ID, Ref),
                 ?MQ_RESULT => #{
                    % TODO: fill in mutilple resources
                 }}).

