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

-module(emq_lwm2m_mqtt_adapter).

-author("Feng Lee <feng@emqtt.io>").

-behaviour(gen_server).

-include("emq_lwm2m.hrl").
-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_protocol.hrl").


%% API.
-export([start_link/3, publish/5, keepalive/1]).
-export([stop/1]).

%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {proto, peer, keepalive, coap_pid, rsp_topic, sub_topic}).

-define(DEFAULT_KEEP_ALIVE_DURATION,  60*2).

-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-MQTT: " ++ Format, Args)).



-ifdef(TEST).
-define(PROTO_INIT(A, B, C, D),         test_mqtt_broker:start(A, B, C, D)).
-define(PROTO_SUBSCRIBE(X, Y),          test_mqtt_broker:subscribe(X)).
-define(PROTO_UNSUBSCRIBE(X, Y),        test_mqtt_broker:unsubscribe(X)).
-define(PROTO_PUBLISH(A1, A2, P),       test_mqtt_broker:publish(A1, A2)).
-define(PROTO_DELIVER_ACK(A1, A2),      A2).
-define(PROTO_SHUTDOWN(A, B),           ok).
-else.
-define(PROTO_INIT(A, B, C, D),         proto_init(A, B, C, D)).
-define(PROTO_SUBSCRIBE(X, Y),          proto_subscribe(X, Y)).
-define(PROTO_UNSUBSCRIBE(X, Y),        proto_unsubscribe(X, Y)).
-define(PROTO_PUBLISH(A1, A2, P),       proto_publish(A1, A2, P)).
-define(PROTO_DELIVER_ACK(Msg, State),  proto_deliver_ack(Msg, State)).
-define(PROTO_SHUTDOWN(A, B),           emqttd_protocol:shutdown(A, B)).
-endif.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------


start_link(CoapPid, ClientId, ChId) ->
    gen_server:start_link({via, emq_lwm2m_registry, ChId}, ?MODULE, {CoapPid, ClientId, ChId}, []).

stop(ChId) ->
    gen_server:stop(emq_lwm2m_registry:whereis_name(ChId)).

publish(ChId, Method, Response, DataFormat, Ref) ->
    gen_server:call({via, emq_lwm2m_registry, ChId}, {publish, Method, Response, DataFormat, Ref}).

keepalive(ChId)->
    gen_server:cast({via, emq_lwm2m_registry, ChId}, keepalive).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init({CoapPid, ClientId, ChId}) ->
    ?LOG(debug, "try to start adapter ClientId=~p, ChId=~p", [ClientId, ChId]),
    case ?PROTO_INIT(ClientId, undefined, undefined, ChId) of
        {ok, Proto}           ->
            Topic = <<"lwm2m/", ClientId/binary, "/command">>,
            NewProto = ?PROTO_SUBSCRIBE(Topic, Proto),
            RspTopic = <<"lwm2m/", ClientId/binary, "/response">>,
            {ok, #state{coap_pid = CoapPid, proto = NewProto, peer = ChId, rsp_topic = RspTopic, sub_topic = Topic}};
        Other                 ->
            {stop, Other}
    end.

handle_call({publish, Method, CoapResponse, DataFormat, Ref}, _From, State=#state{proto = Proto, rsp_topic = Topic}) ->
    Message = emq_lwm2m_cmd_converter:coap_response_to_mqtt_payload(Method, CoapResponse, DataFormat, Ref),
    NewProto = ?PROTO_PUBLISH(Topic, Message, Proto),
    {reply, ok, State#state{proto = NewProto}};


handle_call(info, From, State = #state{proto = ProtoState, peer = Channel}) ->
    ProtoInfo  = emqttd_protocol:info(ProtoState),
    ClientInfo = [{peername, Channel}],
    {reply, Stats, _, _} = handle_call(stats, From, State),
    {reply, lists:append([ClientInfo, ProtoInfo, Stats]), State};

handle_call(stats, _From, State = #state{proto = ProtoState}) ->
    {reply, lists:append([emqttd_misc:proc_stats(), emqttd_protocol:stats(ProtoState)]), State};

handle_call(kick, _From, State) ->
    {stop, {shutdown, kick}, ok, State};

handle_call({set_rate_limit, _Rl}, _From, State) ->
    ?LOG(error, "set_rate_limit is not support", []),
    {reply, ok, State};

handle_call(get_rate_limit, _From, State) ->
    ?LOG(error, "get_rate_limit is not support", []),
    {reply, ok, State};

handle_call(session, _From, State = #state{proto = ProtoState}) ->
    {reply, emqttd_protocol:session(ProtoState), State};

handle_call(Request, _From, State) ->
    ?LOG(error, "adapter unexpected call ~p", [Request]),
    {reply, ignored, State, hibernate}.

handle_cast(keepalive, State=#state{keepalive = undefined}) ->
    {noreply, State, hibernate};
handle_cast(keepalive, State=#state{keepalive = Keepalive}) ->
    NewKeepalive = emq_lwm2m_timer:kick_timer(Keepalive),
    {noreply, State#state{keepalive = NewKeepalive}, hibernate};

handle_cast(Msg, State) ->
    ?LOG(error, "unexpected cast ~p", [Msg]),
    {noreply, State, hibernate}.

handle_info({deliver, Msg = #mqtt_message{topic = TopicName, payload = Payload}},
             State = #state{proto = Proto, coap_pid = CoapPid}) ->
    %% handle PUBLISH from broker
    ?LOG(debug, "get message from broker Topic=~p, Payload=~p", [TopicName, Payload]),
    NewProto = ?PROTO_DELIVER_ACK(Msg, Proto),
    deliver_to_coap(Payload, CoapPid),
    {noreply, State#state{proto = NewProto}};

handle_info({suback, _MsgId, [_GrantedQos]}, State) ->
    {noreply, State, hibernate};

handle_info({subscribe,_}, State) ->
    {noreply, State};

handle_info({keepalive, start, Interval}, StateData) ->
    ?LOG(debug, "Keepalive at the interval of ~p", [Interval]),
    KeepAlive = emq_lwm2m_timer:start_timer(Interval, {keepalive, check}),
    {noreply, StateData#state{keepalive = KeepAlive}, hibernate};

handle_info({keepalive, check}, StateData = #state{keepalive = KeepAlive}) ->
    case emq_lwm2m_timer:is_timeout(KeepAlive) of
        false ->
            ?LOG(debug, "Keepalive checked ok", []),
            NewKeepAlive = emq_lwm2m_timer:restart_timer(KeepAlive),
            {noreply, StateData#state{keepalive = NewKeepAlive}};
        true ->
            ?LOG(debug, "Keepalive timeout", []),
            {stop, keepalive_timeout, StateData}
    end;


handle_info(emit_stats, State) ->
    ?LOG(info, "emit_stats is not supported", []),
    {noreply, State, hibernate};

handle_info(timeout, State) ->
    {stop, {shutdown, idle_timeout}, State};

handle_info({shutdown, Error}, State) ->
    {stop, {shutdown, Error}, State};

handle_info({shutdown, conflict, {ClientId, NewPid}}, State) ->
    ?LOG(warning, "clientid '~s' conflict with ~p", [ClientId, NewPid]),
    {stop, {shutdown, conflict}, State};

handle_info(Info, State) ->
    ?LOG(error, "unexpected info ~p", [Info]),
    {noreply, State, hibernate}.

terminate(Reason, #state{proto = Proto, keepalive = KeepAlive, sub_topic = SubTopic}) ->
    emq_lwm2m_timer:cancel_timer(KeepAlive),
    CleanFun =  fun(Error) ->
                    NewProto = ?PROTO_UNSUBSCRIBE(SubTopic, Proto),
                    ?PROTO_SHUTDOWN(Error, NewProto)
                end,
    case {Proto, Reason} of
        {undefined, _} ->
            ok;
        {_, {shutdown, Error}} ->
            CleanFun(Error);
        {_, Reason} ->
            CleanFun(Reason)
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

proto_init(ClientId, Username, Password, Channel) ->
    SendFun = fun(_Packet) -> ok end,
    PktOpts = [{max_clientid_len, 96}, {max_packet_size, 512}],
    Proto = emqttd_protocol:init(Channel, SendFun, PktOpts),
    ConnPkt = #mqtt_packet_connect{client_id  = ClientId,
                                   username   = Username,
                                   password   = Password,
                                   clean_sess = true,
                                   keep_alive = application:get_env(?APP, keepalive, ?DEFAULT_KEEP_ALIVE_DURATION)},
    case emqttd_protocol:received(?CONNECT_PACKET(ConnPkt), Proto) of
        {ok, Proto1}                              -> {ok, Proto1};
        {stop, {shutdown, auth_failure}, _Proto2} -> {stop, auth_failure};
        Other                                     -> error(Other)
    end.

proto_subscribe(Topic, Proto) ->
    ?LOG(debug, "subscribe Topic=~p", [Topic]),
    case emqttd_protocol:received(?SUBSCRIBE_PACKET(1, [{Topic, ?QOS1}]), Proto) of
        {ok, Proto1}  -> Proto1;
        Other         -> error(Other)
    end.

proto_unsubscribe(Topic, Proto) ->
    ?LOG(debug, "unsubscribe Topic=~p", [Topic]),
    case emqttd_protocol:received(?UNSUBSCRIBE_PACKET(1, [Topic]), Proto) of
        {ok, Proto1}  -> Proto1;
        Other         -> error(Other)
    end.

proto_publish(Topic, Payload, Proto) ->
    ?LOG(debug, "publish Topic=~p, Payload=~p", [Topic, Payload]),
    Publish = #mqtt_packet{header   = #mqtt_packet_header{type = ?PUBLISH, qos = ?QOS0},
                           variable = #mqtt_packet_publish{topic_name = Topic, packet_id = 1},
                           payload  = Payload},
    case emqttd_protocol:received(Publish, Proto) of
        {ok, Proto1}  -> Proto1;
        Other         -> error(Other)
    end.

proto_deliver_ack(#mqtt_message{qos = ?QOS0, pktid = _PacketId}, Proto) ->
    Proto;
proto_deliver_ack(#mqtt_message{qos = ?QOS1, pktid = PacketId}, Proto) ->
    case emqttd_protocol:received(?PUBACK_PACKET(?PUBACK, PacketId), Proto) of
        {ok, NewProto} -> NewProto;
        Other          -> error(Other)
    end;
proto_deliver_ack(#mqtt_message{qos = ?QOS2, pktid = PacketId}, Proto) ->
    case emqttd_protocol:received(?PUBACK_PACKET(?PUBREC, PacketId), Proto) of
        {ok, NewProto} ->
            case emqttd_protocol:received(?PUBACK_PACKET(?PUBCOMP, PacketId), NewProto) of
                {ok, CurrentProto} -> CurrentProto;
                Another            -> error(Another)
            end;
        Other          -> error(Other)
    end.



deliver_to_coap(JsonData, CoapPid) ->
    ?LOG(debug, "deliver_to_coap CoapPid=~p (alive=~p), JsonData=~p", [CoapPid, is_process_alive(CoapPid), JsonData]),
    Command = jsx:decode(JsonData, [return_maps]),
    {CoapRequest, Ref} = emq_lwm2m_cmd_converter:mqtt_payload_to_coap_request(Command),
    CoapPid ! {dispatch_command, CoapRequest, Ref}.



