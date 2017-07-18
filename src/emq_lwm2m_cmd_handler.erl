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

-module(emq_lwm2m_cmd_handler).

-author("Feng Lee <feng@emqtt.io>").

-include("emq_lwm2m.hrl").
-include_lib("lwm2m_coap/include/coap.hrl").

-export([mqtt_payload_to_coap_request/1, coap_response_to_mqtt_payload/4]).

-define(LOG(Level, Format, Args), lager:Level("LWM2M-CNVT: " ++ Format, Args)).



mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Read">>, ?MQ_BASENAME := Path}) ->
    {_Method, PathList} = path_list(Path),
    {lwm2m_coap_message:request(con, get, <<>>, [{uri_path, PathList}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Write">>, ?MQ_VALUE := Value}) ->
    #{<<"bn">>:=Path} = Value,
    {Method, PathList} = path_list(Path),
    TlvData = emq_lwm2m_json:json_to_tlv(Value),
    Payload = emq_lwm2m_tlv:encode(TlvData),
    CoapRequest = lwm2m_coap_message:request(con, Method, Payload, [{uri_path, PathList}, {content_format, <<"application/vnd.oma.lwm2m+tlv">>}]),
    {CoapRequest, InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Execute">>, ?MQ_BASENAME := Path}) ->
    {_Method, PathList} = path_list(Path),
    Payload =   case maps:get(?MQ_ARGS, InputCmd, undefined) of
                    undefined -> <<>>;
                    Data      -> Data
                end,
    {lwm2m_coap_message:request(con, post, Payload, [{uri_path, PathList}, {content_format, <<"text/plain">>}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Discover">>, ?MQ_BASENAME := Path}) ->
    {_Method, PathList} = path_list(Path),
    {lwm2m_coap_message:request(con, get, <<>>, [{uri_path, PathList}, {'accept', ?LWM2M_FORMAT_LINK}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Write-Attributes">>, ?MQ_BASENAME := Path, ?MQ_VALUE := Query}) ->
    {_Method, PathList} = path_list(Path),
    {lwm2m_coap_message:request(con, put, <<>>, [{uri_path, PathList}, {uri_query, [Query]}]), InputCmd};
mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Observe">>, ?MQ_BASENAME := Path}) ->
    {_Method, PathList} = path_list(Path),
    {lwm2m_coap_message:request(con, get, <<>>, [{uri_path, PathList}, {observe, 0}]), InputCmd}.

coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Read">>}) ->
    %?LOG(debug, "coap_response_to_mqtt_payload read Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_read_response_to_mqtt_payload(Method, CoapPayload, Format, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Write">>}) ->
    %?LOG(debug, "coap_response_to_mqtt_payload write Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_write_response_to_mqtt_payload(Method, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Execute">>}) ->
    %?LOG(debug, "coap_response_to_mqtt_payload execute Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_execute_response_to_mqtt_payload(Method, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Discover">>}) ->
    %?LOG(debug, "coap_response_to_mqtt_payload discover Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_discover_response_to_mqtt_payload(CoapPayload, Method, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Write-Attributes">>}) ->
    %?LOG(debug, "coap_response_to_mqtt_payload write-attribute Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_writeattr_response_to_mqtt_payload(CoapPayload, Method, Ref);
coap_response_to_mqtt_payload(Method, CoapPayload, Format, Ref=#{?MQ_COMMAND := <<"Observe">>}) ->
    %?LOG(debug, "coap_response_to_mqtt_payload observe Method=~p, CoapPayload=~p, Format=~p, Ref=~p", [Method, CoapPayload, Format, Ref]),
    coap_observe_response_to_mqtt_payload(Method, CoapPayload, Format, Ref).

coap_read_response_to_mqtt_payload({error, Error}, _CoapPayload, _Format, Ref) ->
    make_error(Ref, error_code(Error));
coap_read_response_to_mqtt_payload({ok, content}, CoapPayload, Format, Ref) ->
    coap_read_response_to_mqtt_payload2(CoapPayload, Format, Ref).

coap_read_response_to_mqtt_payload2(CoapPayload, <<"text/plain">>, Ref=#{?MQ_BASENAME:=BaseName}) ->
    case catch emq_lwm2m_json:text_to_json(BaseName, CoapPayload) of
        {'EXIT',{no_xml_definition, _}} -> make_error(Ref, <<"No XML Definition">>);
        Result                          -> make_response(Ref, Result)
    end;
coap_read_response_to_mqtt_payload2(CoapPayload, <<"application/octet-stream">>, Ref=#{?MQ_BASENAME:=BaseName}) ->
    case catch emq_lwm2m_json:opaque_to_json(BaseName, CoapPayload) of
        {'EXIT',{no_xml_definition, _}} -> make_error(Ref, <<"No XML Definition">>);
        Result                          -> make_response(Ref, Result)
    end;
coap_read_response_to_mqtt_payload2(CoapPayload, <<"application/vnd.oma.lwm2m+tlv">>, Ref=#{?MQ_BASENAME:=BaseName}) ->
    Decode = emq_lwm2m_tlv:parse(CoapPayload),
    case catch emq_lwm2m_json:tlv_to_json(BaseName, Decode) of
        no_xml_definition -> make_error(Ref, <<"No XML Definition">>);
        Result -> make_response(Ref, Result)
    end;
coap_read_response_to_mqtt_payload2(CoapPayload, <<"application/vnd.oma.lwm2m+json">>, Ref) ->
    Result = jsx:decode(CoapPayload),
    make_response(Ref, Result).

coap_write_response_to_mqtt_payload({ok, changed}, Ref) ->
    make_response(Ref, <<"Changed">>);
coap_write_response_to_mqtt_payload({error, Error}, Ref) ->
    make_error(Ref, error_code(Error)).

coap_execute_response_to_mqtt_payload({ok, changed}, Ref) ->
    make_response(Ref, <<"Changed">>);
coap_execute_response_to_mqtt_payload({error, Error}, Ref) ->
    make_error(Ref, error_code(Error)).

coap_discover_response_to_mqtt_payload(CoapPayload, {ok, content}, Ref) ->
    make_response(Ref, CoapPayload);
coap_discover_response_to_mqtt_payload(_CoapPayload, {error, Error}, Ref) ->
    make_error(Ref, error_code(Error)).

coap_writeattr_response_to_mqtt_payload(_CoapPayload, {ok, changed}, Ref) ->
    make_response(Ref, <<"Changed">>);
coap_writeattr_response_to_mqtt_payload(_CoapPayload, {error, Error}, Ref) ->
    make_response(Ref, error_code(Error)).

coap_observe_response_to_mqtt_payload({error, Error}, _CoapPayload, _Format, Ref) ->
    make_error(Ref, error_code(Error));
coap_observe_response_to_mqtt_payload({ok, content}, CoapPayload, Format, Ref) ->
    coap_read_response_to_mqtt_payload2(CoapPayload, Format, Ref).

make_response(Ref=#{}, Value) ->
    jsx:encode(#{
                    ?MQ_COMMAND_ID  => maps:get(?MQ_COMMAND_ID, Ref),
                    ?MQ_COMMAND     => maps:get(?MQ_COMMAND, Ref),
                    ?MQ_RESULT      => Value
                }).

make_error(Ref=#{}, Error) ->
    jsx:encode(#{
                    ?MQ_COMMAND_ID  => maps:get(?MQ_COMMAND_ID, Ref),
                    ?MQ_COMMAND     => maps:get(?MQ_COMMAND, Ref),
                    ?MQ_ERROR       => Error
                }).

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

path_list(Path) ->
    case binary:split(Path, [<<$/>>], [global]) of
        [<<>>, ObjId, ObjInsId, ResId, <<>>] -> {put,  [ObjId, ObjInsId, ResId]};
        [<<>>, ObjId, ObjInsId, ResId]       -> {put,  [ObjId, ObjInsId, ResId]};
        [<<>>, ObjId, ObjInsId, <<>>]        -> {post, [ObjId, ObjInsId]};
        [<<>>, ObjId, ObjInsId]              -> {post, [ObjId, ObjInsId]};
        [<<>>, ObjId, <<>>]                  -> {post, [ObjId]};
        [<<>>, ObjId]                        -> {post, [ObjId]};
        [ObjId, ObjInsId, ResId, <<>>]       -> {put,  [ObjId, ObjInsId, ResId]};
        [ObjId, ObjInsId, ResId]             -> {put,  [ObjId, ObjInsId, ResId]};
        [ObjId, ObjInsId, <<>>]              -> {post, [ObjId, ObjInsId]};
        [ObjId, ObjInsId]                    -> {post, [ObjId, ObjInsId]};
        [ObjId, <<>>]                        -> {post, [ObjId]};
        [ObjId]                              -> {post, [ObjId]}
    end.


