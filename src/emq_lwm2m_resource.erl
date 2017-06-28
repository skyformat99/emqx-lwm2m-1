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

-module(emq_lwm2m_resource).

-author("Feng Lee <feng@emqtt.io>").

-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_protocol.hrl").
-include_lib("gen_coap/include/coap.hrl").


-behaviour(coap_resource).

-export([coap_discover/2, coap_get/4, coap_post/4, coap_put/4, coap_delete/3,
    coap_observe/4, coap_unobserve/1, handle_info/2, coap_ack/2]).

-include("emq_lwm2m.hrl").

-define(LWM2M_REGISTER_PREFIX, <<"rd">>).

-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-REG: " ++ Format, Args)).


% resource operations
coap_discover(_Prefix, _Args) ->
    [{absolute, "mqtt", []}].

coap_get(ChId, [?LWM2M_REGISTER_PREFIX], Name, Query) ->
    ?LOG(debug, "~p GET Name=~p, Query=~p~n", [ChId, Name, Query]),
    #coap_content{};
coap_get(ChId, Prefix, Name, Query) ->
    ?LOG(error, "ignore bad put request ChId=~p, Prefix=~p, Name=~p, Query=~p", [ChId, Prefix, Name, Query]),
    {error, bad_request}.

coap_post(ChId, [?LWM2M_REGISTER_PREFIX], Name, Content) ->
    Epn      = proplists:get_value("ep", Name),
    LifeTime = proplists:get_value("lt", Name),
    Lwm2mVer = proplists:get_value("lwm2m", Name),
    Binding  = proplists:get_value("b", Name),
    ?LOG(debug, "~p REGISTER command Epn=~p, LifeTime=~p, Lwm2mVer=~p, Binding=~p, Content=~p", [ChId, Epn, LifeTime, Lwm2mVer, Binding, Content]),
    % TODO: parse content
    Location = emq_lwm2m_endpointname:register_name(Epn),
    emq_lwm2m_mqtt_adapter:start_link(self(), Epn, ChId),
    {ok, created, list_to_binary(io_lib:format("/rd/~.16B", [Location]))};
coap_post(ChId, [?LWM2M_REGISTER_PREFIX, Location], Name, Content) ->
    LocationInt = binary_to_integer(Location, 16),
    LifeTime = proplists:get_value("lt", Name),
    Binding  = proplists:get_value("b", Name),
    ?LOG(debug, "~p UPDATE command location=~p, LifeTime=~p, Binding=~p, Content=~p", [ChId, Location, LifeTime, Binding, Content]),
    % TODO: update lifetime
    % TODO: parse content
    {ok, changed, <<>>};
coap_post(ChId, Prefix, Name, Content) ->
    ?LOG(error, "bad post request ChId=~p, Prefix=~p, Name=~p, Content=~p", [ChId, Prefix, Name, Content]),
    {error, bad_request}.

coap_put(_ChId, [?LWM2M_REGISTER_PREFIX], [Topic], #coap_content{payload = Payload}) ->
    ?LOG(debug, "put message, Topic=~p, Payload=~p~n", [Topic, Payload]),
    Pid = get(mqtt_client_pid),
    emq_lwm2m_mqtt_adapter:publish(Pid, Payload),
    ok;
coap_put(_ChId, Prefix, Name, Content) ->
    ?LOG(error, "put has error, Prefix=~p, Name=~p, Content=~p", [Prefix, Name, Content]),
    {error, bad_request}.

coap_delete(ChId, [?LWM2M_REGISTER_PREFIX, Location], _Name) ->
    LocationInt = binary_to_integer(Location, 16),
    ?LOG(debug, "~p DELETE command location=~p", [ChId, Location]),
    emq_lwm2m_mqtt_adapter:stop(ChId),
    emq_lwm2m_endpointname:unregister_location(LocationInt),
    ok;
coap_delete(_ChId, _Prefix, _Name) ->
    {error, bad_request}.


coap_observe(ChId, Prefix, Name, Ack) ->
    ?LOG(error, "unknown observe request ChId=~p, Prefix=~p, Name=~p, Ack=~p", [ChId, Prefix, Name, Ack]),
    {error, method_not_allowed}.

coap_unobserve({state, ChId, Prefix, Name}) ->
    ?LOG(error, "ignore unknown unobserve request ChId=~p, Prefix=~p, Name=~p", [ChId, Prefix, Name]),
    ok.

handle_info({dispatch_command, Command}, _ObState) ->
    {send_request, Command};

handle_info({coap_response, ChId, _Channel, _Ref, Message}, ObState) ->
    emq_lwm2m_mqtt_adapter:publish(ChId, Message),
    {noreply, ObState};

handle_info(Message, State) ->
    ?LOG(error, "Unknown Message ~p", [Message]),
    {noreply, State}.

coap_ack(_Ref, State) -> {ok, State}.






% end of file



