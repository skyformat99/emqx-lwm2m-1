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

-define(APP, emq_lwm2m).


-record(coap_mqtt_auth, {clientid, username, password}).


-define(MQ_COMMAND_ID,         <<"CmdID">>).
-define(MQ_COMMAND,            <<"Command">>).

-define(MQ_OBJECT_ID,          <<"ObjectID">>).
-define(MQ_OBJECT_INSTANCE_ID, <<"ObjectInstanceID">>).
-define(MQ_RESOURCE_ID,        <<"ResourceID">>).
-define(MQ_BASENAME,           <<"BaseName">>).

-define(MQ_VALUE_TYPE,         <<"ValueType">>).
-define(MQ_VALUE,              <<"Value">>).
-define(MQ_ERROR,              <<"Error">>).
-define(MQ_RESULT,             <<"Result">>).



-define(LWM2M_FORMAT_PLAIN_TEXT, 0).
-define(LWM2M_FORMAT_LINK, 40).
-define(LWM2M_FORMAT_OPAQUE, 42).
-define(LWM2M_FORMAT_TLV, 11542).
-define(LWMWM_FORMAT_JSON, 11543).




