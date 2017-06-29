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

-module(emq_lwm2m_mqtt_payload).

-author("Feng Lee <feng@emqtt.io>").

-include("emq_lwm2m.hrl").
-include_lib("gen_coap/include/coap.hrl").

-export([get_oid_rid/1]).


-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-MQTT-PLD: " ++ Format, Args)).


-define(OBJECT_ID,          <<"ObjectID">>).
-define(OBJECT_INSTANCE_ID, <<"ObjectInstanceID">>).
-define(RESOURCE_ID,        <<"ResourceID">>).


get_oid_rid(MqttPayload) ->
    ?LOG(debug, "get_oid_rid() MqttPayload=~p", [MqttPayload]),
    ObjectId         = get(?OBJECT_ID, MqttPayload),
    ObjectInstanceId = get(?OBJECT_INSTANCE_ID, MqttPayload),
    ResourceId       = get(?RESOURCE_ID, MqttPayload),
    {ObjectId, ObjectInstanceId, ResourceId}.




get(Key, MqttPayload) ->
    case maps:find(Key, MqttPayload) of
        {ok, Value} -> Value;
        error       -> undefined
    end.


