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

-module(emq_lwm2m_SUITE).

-compile(export_all).

-define(PORT, 5683).

-define(LOGT(Format, Args), lager:debug("TEST_SUITE: " ++ Format, Args)).


-include_lib("gen_coap/include/coap.hrl").
-include_lib("eunit/include/eunit.hrl").


all() -> [case01_register].



init_per_suite(Config) ->
    lager_common_test_backend:bounce(debug),
    Config.

end_per_suite(Config) ->
    Config.


case01_register(_Config) ->
    test_mqtt_broker:start_link(),
    {ok, _Started} = application:ensure_all_started(emq_lwm2m),
    timer:sleep(100),

    Epn = "urn:oma:lwm2m:oma:3",
    CoapSock = coap_client:open_udp("127.0.0.1", ?PORT),
    Reply = coap_client:request(CoapSock, post, "coap://127.0.0.1/rd?ep="++Epn++"&lt=345&lwm2m=1", #coap_content{format = <<"text/plain">>, payload = <<"</1>, </2>, </3>, </4>, </5>">>}),
    ?assertMatch({ok,created, _}, Reply),
    timer:sleep(50),
    SubTopic = list_to_binary("lwm2m/"++Epn++"/command"),
    ?assertEqual([SubTopic], test_mqtt_broker:get_subscrbied_topics()),

    coap_client:close_udp(CoapSock),
    ok = application:stop(emq_lwm2m),
    test_mqtt_broker:stop().






receive_notification() ->
    receive
        {coap_notify, Pid, N2, Code2, Content2} ->
            {coap_notify, Pid, N2, Code2, Content2}
    after 2000 ->
        receive_notification_timeout
    end.



