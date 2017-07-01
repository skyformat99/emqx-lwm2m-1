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

-export([mqtt_payload_to_coap_request/1]).


-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-CNVT: " ++ Format, Args)).


mqtt_payload_to_coap_request(InputCmd = #{?MQ_COMMAND := <<"Read">>}) ->
    Path = build_path(emq_lwm2m_mqtt_payload:get_oid_rid(InputCmd)),
    coap_message:request(con, get, <<>>, [{uri_path, Path}]).




build_path({ObjectName, undefined, undefined}) ->
    ObjDef = emq_lwm2m_object:get_obj_def(ObjectName, false),
    Oid = emq_lwm2m_object:get_object_id(ObjDef),
    make_path("/~s", [Oid]);
build_path({ObjectName, ObjectInstanceId, undefined}) ->
    ObjDef = emq_lwm2m_object:get_obj_def(ObjectName, false),
    Oid = emq_lwm2m_object:get_object_id(ObjDef),
    make_path("/~s/~b", [Oid, ObjectInstanceId]);
build_path({ObjectName, ObjectInstanceId, ResourceId}) ->
    ObjDef = emq_lwm2m_object:get_obj_def(ObjectName, false),
    {Oid, Rid} = emq_lwm2m_object:get_object_and_resource_id(ResourceId, ObjDef),
    make_path("/~s/~b/~s", [Oid, ObjectInstanceId, Rid]).


make_path(Format, Args) ->
    [list_to_binary(lists:flatten(io_lib:format(Format, Args)))].

