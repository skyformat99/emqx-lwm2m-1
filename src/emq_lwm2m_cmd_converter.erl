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

-module(emq_lwm2m_cmd_converter).

-author("Feng Lee <feng@emqtt.io>").

-include("emq_lwm2m.hrl").
-include_lib("lwm2m_coap/include/coap.hrl").

-export([mqtt_payload_to_coap_request/1, coap_response_to_mqtt_payload/4]).


-define(LOG(Level, Format, Args), lager:Level("LWM2M-CNVT: " ++ Format, Args)).



mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Read">>, ?MQ_BASENAME := Path}) ->
    {lwm2m_coap_message:request(con, get, <<>>, [{uri_path, [Path]}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Write">>}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    Method = case ResourceId of
                 undefined -> post;
                 _         -> put
             end,
    case ResourceId of
        undefined -> process_write_object_command(Method, Path, InputCmd);
        _Other    -> process_write_resource_command(Method, Path, InputCmd)
    end;
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Execute">>}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    {lwm2m_coap_message:request(con, post, <<>>, [{uri_path, Path}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Discover">>}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    {lwm2m_coap_message:request(con, get, <<>>, [{uri_path, Path}, {'accept', ?LWM2M_FORMAT_LINK}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Write-Attributes">>}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    Query = maps:get(?MQ_VALUE, InputCmd),
    {lwm2m_coap_message:request(con, put, <<>>, [{uri_path, Path}, {uri_query, [Query]}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Observe">>}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    {lwm2m_coap_message:request(con, get, <<>>, [{uri_path, Path}, {observe, 0}]), InputCmd}.


coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Read">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload read Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_read_response_to_mqtt_payload(Method, CoapPayload, Format, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Write">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload write Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_write_response_to_mqtt_payload(Method, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Execute">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload execute Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_execute_response_to_mqtt_payload(Method, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Discover">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload discover Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_discover_response_to_mqtt_payload(CoapPayload, Method, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Write-Attributes">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload write-attribute Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_writeattr_response_to_mqtt_payload(CoapPayload, Method, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Observe">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload observe Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_observe_response_to_mqtt_payload(Method, CoapPayload, Format, Ref).





coap_read_response_to_mqtt_payload({error, Error}, _CoapPayload, _Format, Ref) ->
    make_read_error(Ref, error_code(Error));
coap_read_response_to_mqtt_payload({ok, content}, CoapPayload, Format, Ref) ->
    ?LOG(debug, "coap_read_response_to_mqtt_payload read CoapPayload=~p, Format=~p, Ref=~p", [CoapPayload, Format, Ref]),
    coap_read_response_to_mqtt_payload2(CoapPayload, Format, Ref).

coap_read_response_to_mqtt_payload2(CoapPayload, <<"text/plain">>, Ref=#{?MQ_BASENAME:=BaseName}) ->
    Result = emq_lwm2m_json:text_to_json(BaseName, CoapPayload),
    make_read_response(Ref, Result);
coap_read_response_to_mqtt_payload2(CoapPayload, <<"application/octet-stream">>, Ref=#{?MQ_BASENAME:=BaseName}) ->
    Result = emq_lwm2m_json:opaque_to_json(BaseName, CoapPayload),
    make_read_response(Ref, Result);
coap_read_response_to_mqtt_payload2(CoapPayload, <<"application/vnd.oma.lwm2m+tlv">>, Ref=#{?MQ_BASENAME:=BaseName}) ->
    Decode = emq_lwm2m_tlv:parse(CoapPayload),
    Result = emq_lwm2m_json:tlv_to_json(BaseName, Decode),
    ?LOG(debug, "coap_read_resource_response_to_mqtt_payload tlv ~p", [Result]),
    make_read_response(Ref, Result);
coap_read_response_to_mqtt_payload2(CoapPayload, <<"application/vnd.oma.lwm2m+json">>, Ref) ->
    Result = jsx:decode(CoapPayload),
    make_read_response(Ref, Result).



coap_write_response_to_mqtt_payload({ok, changed}, Ref) ->
    make_write_resource_response(Ref, <<"Changed">>);
coap_write_response_to_mqtt_payload({error, Error}, Ref) ->
    make_write_resource_response(Ref, error_code(Error)).

coap_execute_response_to_mqtt_payload({ok, changed}, Ref) ->
    make_write_resource_response(Ref, <<"Changed">>);
coap_execute_response_to_mqtt_payload({error, Error}, Ref) ->
    make_write_resource_error(Ref, error_code(Error)).

coap_discover_response_to_mqtt_payload(CoapPayload, {ok, content}, Ref) ->
    make_read_response(Ref, CoapPayload);
coap_discover_response_to_mqtt_payload(_CoapPayload, {error, Error}, Ref) ->
    make_read_error(Ref, error_code(Error)).

coap_writeattr_response_to_mqtt_payload(_CoapPayload, {ok, changed}, Ref) ->
    make_write_resource_response(Ref, <<"Changed">>);
coap_writeattr_response_to_mqtt_payload(_CoapPayload, {error, Error}, Ref) ->
    make_write_resource_response(Ref, error_code(Error)).


coap_observe_response_to_mqtt_payload({error, Error}, _CoapPayload, _Format, Ref) ->
    make_read_error(Ref, error_code(Error));
coap_observe_response_to_mqtt_payload({ok, content}, CoapPayload, Format, Ref) ->
    coap_read_response_to_mqtt_payload2(CoapPayload, Format, Ref).


build_path({undefined, undefined, undefined}) ->
    error("objectid is missing");
build_path({ObjectId, undefined, undefined}) ->
    %ObjDef = emq_lwm2m_xml_object:get_obj_def(ObjectName, false),
    %Oid = emq_lwm2m_xml_object:get_object_id(ObjDef),
    make_path("/~b", [ObjectId]);
build_path({ObjectId, ObjectInstanceId, undefined}) ->
    %ObjDef = emq_lwm2m_xml_object:get_obj_def(ObjectName, false),
    %Oid = emq_lwm2m_xml_object:get_object_id(ObjDef),
    make_path("/~b/~b", [ObjectId, ObjectInstanceId]);
build_path({ObjectId, ObjectInstanceId, ResourceId}) ->
    %ObjDef = emq_lwm2m_xml_object:get_obj_def(ObjectName, false),
    %{Oid, Rid} = emq_lwm2m_xml_object:get_object_and_resource_id(ResourceId, ObjDef),
    make_path("/~b/~b/~b", [ObjectId, ObjectInstanceId, ResourceId]).


make_path(Format, Args) ->
    [list_to_binary(lists:flatten(io_lib:format(Format, Args)))].


get_oid_rid(MqttPayload) ->
    ?LOG(debug, "get_oid_rid() MqttPayload=~p", [MqttPayload]),
    ObjectId         = maps:get(?MQ_OBJECT_ID, MqttPayload, undefined),
    ObjectInstanceId = maps:get(?MQ_OBJECT_INSTANCE_ID, MqttPayload, undefined),
    ResourceId       = maps:get(?MQ_RESOURCE_ID, MqttPayload, undefined),
    % they are all binary type
    {ObjectId, ObjectInstanceId, ResourceId}.



make_read_response(Ref=#{}, Value) ->
    jsx:encode(#{
                    ?MQ_COMMAND_ID          => maps:get(?MQ_COMMAND_ID, Ref),
                    ?MQ_COMMAND             => maps:get(?MQ_COMMAND, Ref),
                    ?MQ_RESULT              => Value
                }).


make_read_error(Ref=#{}, Error) ->
    jsx:encode(#{
                    ?MQ_COMMAND_ID          => maps:get(?MQ_COMMAND_ID, Ref),
                    ?MQ_OBJECT_ID           => maps:get(?MQ_OBJECT_ID, Ref),
                    ?MQ_OBJECT_INSTANCE_ID  => maps:get(?MQ_OBJECT_INSTANCE_ID, Ref),
                    ?MQ_RESOURCE_ID         => maps:get(?MQ_RESOURCE_ID, Ref),
                    ?MQ_VALUE               => Error
                }).




make_write_resource_response(Ref=#{}, Result) ->
    ?LOG(debug, "make_write_resource_response Ref=~p, Result=~p", [Ref, Result]),
    jsx:encode(#{
                    ?MQ_COMMAND_ID          => maps:get(?MQ_COMMAND_ID, Ref),
                    ?MQ_OBJECT_ID           => maps:get(?MQ_OBJECT_ID, Ref),
                    ?MQ_OBJECT_INSTANCE_ID  => maps:get(?MQ_OBJECT_INSTANCE_ID, Ref),
                    ?MQ_RESOURCE_ID         => maps:get(?MQ_RESOURCE_ID, Ref),
                    ?MQ_RESULT              => Result
                }).

make_write_resource_error(Ref=#{}, Error) ->
    ?LOG(debug, "make_write_resource_error Ref=~p, Error=~p", [Ref, Error]),
    jsx:encode(#{
                    ?MQ_COMMAND_ID          => maps:get(?MQ_COMMAND_ID, Ref),
                    ?MQ_OBJECT_ID           => maps:get(?MQ_OBJECT_ID, Ref),
                    ?MQ_OBJECT_INSTANCE_ID  => maps:get(?MQ_OBJECT_INSTANCE_ID, Ref),
                    ?MQ_RESOURCE_ID         => maps:get(?MQ_RESOURCE_ID, Ref),
                    ?MQ_ERROR               => Error
                }).



process_write_object_command(Method, Path, InputCmd) ->
    ?LOG(debug, "process_write_object_command Method=~p, Path=~p, InputCmd=~p", [Method, Path, InputCmd]),
    error("not support now").


process_write_resource_command(Method, Path, InputCmd=#{?MQ_VALUE_TYPE := Type, ?MQ_VALUE := Value}) ->
    {Format, Payload} = case Type of
                            <<"text">>   -> {<<"text/plain">>, to_binary(Value)};
                            <<"binary">> -> {<<"application/octet-stream">>, to_binary(Value)};
                            <<"json">>   -> error("not support now")
                        end,
    {lwm2m_coap_message:request(con, Method, Payload, [{uri_path, Path}, {content_format, Format}]), InputCmd}.


error_code(not_acceptable) ->
    <<"Not Acceptable">>;
error_code(method_not_allowed) ->
    <<"Method Not Allowed">>;
error_code(not_found) ->
    <<"Not Found">>;
error_code(uauthorized) ->
    <<"Unauthorized">>;
error_code(bad_request) ->
    <<"Bad Request">>.


to_binary(true) ->
    <<"1">>;
to_binary(false) ->
    <<"0">>;
to_binary(Value) when is_integer(Value) ->
    list_to_binary(io_lib:format("~b", [Value]));
to_binary(Value) when is_float(Value) ->
    list_to_binary(io_lib:format("~f", [Value]));
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
    Value.

