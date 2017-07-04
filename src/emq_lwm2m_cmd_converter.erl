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

-export([mqtt_payload_to_coap_request/1, coap_response_to_mqtt_payload/4]).


-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-CNVT: " ++ Format, Args)).


mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Read">>}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    {coap_message:request(con, get, <<>>, [{uri_path, Path}, {'accept', ?LWM2M_FORMAT_PLAIN_TEXT}]), InputCmd};
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
    {coap_message:request(con, post, <<>>, [{uri_path, Path}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Discover">>}) ->
    {ObjectId, ObjectInstanceId, ResourceId} = get_oid_rid(InputCmd),
    Path = build_path({ObjectId, ObjectInstanceId, ResourceId}),
    {coap_message:request(con, get, <<>>, [{uri_path, Path}, {'accept', ?LWM2M_FORMAT_LINK}]), InputCmd}.


coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Read">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload read Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_read_response_to_mqtt_payload(Method, CoapPayload, Format, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Write">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload write Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_write_response_to_mqtt_payload(Ref, Method);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Execute">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload execute Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_execute_response_to_mqtt_payload(Ref, Method);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Discover">>}) ->
    ?LOG(debug, "coap_response_to_mqtt_payload discover Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_discover_response_to_mqtt_payload(Ref, CoapPayload, Method).



coap_read_response_to_mqtt_payload({error, Error}, _CoapPayload, _Format, Ref) ->
    make_read_resource_error(Ref, error_code(Error));
coap_read_response_to_mqtt_payload({ok, content}, CoapPayload, Format, Ref) ->
    coap_read_response_to_mqtt_payload2(CoapPayload, Format, Ref).


coap_read_response_to_mqtt_payload2(CoapPayload, Format, Ref=#{?MQ_RESOURCE_ID := _ResId}) ->
    coap_read_resource_response_to_mqtt_payload(CoapPayload, Format, Ref);
coap_read_response_to_mqtt_payload2(CoapPayload, Format, Ref=#{}) ->
    coap_read_object_response_to_mqtt_payload(CoapPayload, Format, Ref).


coap_read_resource_response_to_mqtt_payload(CoapPayload, <<"text/plain">>, Ref) ->
    make_read_resource_response(Ref, <<"text">>, CoapPayload);
coap_read_resource_response_to_mqtt_payload(CoapPayload, <<"application/octet-stream">>, Ref) ->
    % CoapPayload is base64-encoded, no transformation is needed
    make_read_resource_response(Ref, <<"binary">>, CoapPayload);
coap_read_resource_response_to_mqtt_payload(CoapPayload, <<"application/vnd.oma.lwm2m+tlv">>, Ref) ->
    % todo: support tls
    error("not support tlv"),
    make_read_resource_response(Ref, <<"json">>, CoapPayload);
coap_read_resource_response_to_mqtt_payload(CoapPayload, <<"application/vnd.oma.lwm2m+json">>, Ref) ->
    % todo: support tls
    error("not support json"),
    make_read_resource_response(Ref, <<"json">>, CoapPayload).


coap_read_object_response_to_mqtt_payload(CoapPayload, <<"text/plain">>, #{?MQ_COMMAND_ID := CmdId, ?MQ_COMMAND := <<"Read">>}) ->
    ?LOG(error, "coap_response_to_mqtt_payload discard CoapPayload=~p, CmdId=~p due to missing resource id", [CoapPayload, CmdId]),
    error("plain text needs a resource id");
coap_read_object_response_to_mqtt_payload(CoapPayload, <<"application/octet-stream">>, #{?MQ_COMMAND_ID := CmdId}) ->
    ?LOG(error, "coap_response_to_mqtt_payload discard CoapPayload=~p, CmdId=~p due to missing resource id", [CoapPayload, CmdId]),
    error("opaque needs a resource id");
coap_read_object_response_to_mqtt_payload(CoapPayload, <<"application/vnd.oma.lwm2m+tls">>, Ref) ->
    % todo: support tls
    error("not support tlv"),
    make_read_resource_response(Ref, <<"json">>, CoapPayload);
coap_read_object_response_to_mqtt_payload(CoapPayload, <<"application/vnd.oma.lwm2m+json">>, Ref) ->
    % todo: support tls
    error("not support tlv"),
    make_read_resource_response(Ref, <<"json">>, CoapPayload).



coap_write_response_to_mqtt_payload(Ref, {ok, changed}) ->
    make_write_resource_response(Ref, <<"Changed">>);
coap_write_response_to_mqtt_payload(Ref, {error, Error}) ->
    make_write_resource_response(Ref, error_code(Error)).



coap_execute_response_to_mqtt_payload(Ref, {ok, changed}) ->
    make_write_resource_response(Ref, <<"Changed">>);
coap_execute_response_to_mqtt_payload(Ref, {error, Error}) ->
    make_write_resource_error(Ref, error_code(Error)).

coap_discover_response_to_mqtt_payload(Ref, CoapPayload, {ok, content}) ->
    make_read_resource_response(Ref, <<"text">>, CoapPayload);
coap_discover_response_to_mqtt_payload(Ref, _CoapPayload, {error, Error}) ->
    make_read_resource_error(Ref, error_code(Error)).



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

make_read_resource_response(Ref=#{}, Type, Value) ->
    jsx:encode(#{
                    ?MQ_COMMAND_ID          => maps:get(?MQ_COMMAND_ID, Ref),
                    ?MQ_OBJECT_ID           => maps:get(?MQ_OBJECT_ID, Ref),
                    ?MQ_OBJECT_INSTANCE_ID  => maps:get(?MQ_OBJECT_INSTANCE_ID, Ref),
                    ?MQ_RESOURCE_ID         => maps:get(?MQ_RESOURCE_ID, Ref),
                    ?MQ_VALUE_TYPE          => Type,
                    ?MQ_VALUE               => Value
                }).


make_read_resource_error(Ref=#{}, Error) ->
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
    {coap_message:request(con, Method, Payload, [{uri_path, Path}, {content_format, Format}]), InputCmd}.


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

