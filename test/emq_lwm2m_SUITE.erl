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

-include("emq_lwm2m.hrl").
-include_lib("gen_coap/include/coap.hrl").
-include_lib("eunit/include/eunit.hrl").


all() -> [case01_register, case10_read].



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
    MsgId = 12,
    {ok, UdpSock} = test_open_udp_socket(),
    test_send_coap_request( UdpSock,
                            post,
                            "coap://127.0.0.1/rd?ep="++Epn++"&lt=345&lwm2m=1", #coap_content{format = <<"text/plain">>,
                            payload = <<"</1>, </2>, </3>, </4>, </5>">>},
                            [],
                            MsgId),
    {Method, MsgId, _Token, _Payload} = test_recv_coap_response(UdpSock),
    ?assertMatch({ok,created}, Method),
    timer:sleep(50),
    SubTopic = list_to_binary("lwm2m/"++Epn++"/command"),
    ?assertEqual([SubTopic], test_mqtt_broker:get_subscrbied_topics()),

    test_close_udp_socket(UdpSock),
    ok = application:stop(emq_lwm2m),
    test_mqtt_broker:stop().


% TODO: case02_update(_Config)


% TODO: case03_deregister(_Config)


case10_read(_Config) ->
    application:set_env(?APP, xml_dir, "../../test/xml"),
    ?LOGT("pwd is ~p", [os:cmd("pwd")]),
    test_mqtt_broker:start_link(),
    {ok, _Started} = application:ensure_all_started(emq_lwm2m),
    timer:sleep(100),

    Epn = "urn:oma:lwm2m:oma:3",
    MsgId1 = 15,
    {ok, UdpSock} = test_open_udp_socket(),
    test_send_coap_request( UdpSock,
                            post,
                            "coap://127.0.0.1/rd?ep="++Epn++"&lt=345&lwm2m=1",
                            #coap_content{format = <<"text/plain">>, payload = <<"</1>, </2>, </3/0>, </4>, </5>">>},
                            [],
                            MsgId1),
    {Method1, MsgId1, _Token1, _Payload1} = test_recv_coap_response(UdpSock),
    ?assertMatch({ok,created}, Method1),
    timer:sleep(50),
    SubTopic = list_to_binary("lwm2m/"++Epn++"/command"),
    ?assertEqual([SubTopic], test_mqtt_broker:get_subscrbied_topics()),


    CommandTopic = <<"lwm2m/", (list_to_binary(Epn))/binary, "/command">>,
    Command = [{?MQ_COMMAND, <<"Read">>}, {?MQ_OBJECT_ID, <<"Device">>}, {?MQ_OBJECT_INSTANCE_ID, 0}, {?MQ_RESOURCE_ID, <<"Manufacturer">>}],
    CommandJson = jsx:encode(Command),
    test_mqtt_broker:dispatch(CommandTopic, CommandJson, CommandTopic),
    timer:sleep(50),
    {Method2, _MsgId2, _Token2, Path2, _Query2, Payload2} = test_recv_coap_request(UdpSock),
    ?assertMatch(get, Method2),
    ?assertMatch(<<"3/0/0">>, Path2),
    timer:sleep(50),

    % TODO:
    error("client send response"),

    test_close_udp_socket(UdpSock),
    ok = application:stop(emq_lwm2m),
    test_mqtt_broker:stop().


receive_notification() ->
    receive
        {coap_notify, Pid, N2, Code2, Content2} ->
            {coap_notify, Pid, N2, Code2, Content2}
    after 2000 ->
        receive_notification_timeout
    end.


%% TODO: add a case that xml is corrupted






test_open_udp_socket() ->
    gen_udp:open(0, [binary, {active, false}]).

test_close_udp_socket(Socket) ->
    gen_udp:close(Socket).

test_send_coap_request(UdpSock, Method, Uri, Content, Options, MsgId) ->
    is_record(Content, coap_content) orelse error("Content must be a #coap_content!"),
    is_list(Options) orelse error("Options must be a list"),
    case resolve_uri(Uri) of
        {coap, {IpAddr, Port}, Path, Query} ->
            Request0 = coap_message:request(con, Method, Content, [{uri_path, Path}, {uri_query, Query} | Options]),
            Request = Request0#coap_message{id = MsgId},
            ?LOGT("send_coap_request Request=~p", [Request]),
            RequestBinary = coap_message_parser:encode(Request),
            ?LOGT("test udp socket send to ~p:~p, data=~p", [IpAddr, Port, RequestBinary]),
            ok = gen_udp:send(UdpSock, IpAddr, Port, RequestBinary);
        {SchemeDiff, ChIdDiff, _, _} ->
            error(lists:flatten(io_lib:format("scheme ~s or ChId ~s does not match with socket", [SchemeDiff, ChIdDiff])))
    end.

test_recv_coap_response(UdpSock) ->
    {ok, {Address, Port, Packet}} = gen_udp:recv(UdpSock, 0, 2000),
    ?LOGT("test udp receive from ~p:~p, data1=~p", [Address, Port, Packet]),
    #coap_message{type = ack, method = Method, id=Id, token = Token, payload = Payload} = coap_message_parser:decode(Packet),
    ?LOGT("receive coap response Method=~p, Id=~p, Token=~p, Payload=~p", [Method, Id, Token, Payload]),
    {Method, Id, Token, Payload}.


test_recv_coap_request(UdpSock) ->
    {ok, {Address, Port, Packet}} = gen_udp:recv(UdpSock, 0, 2000),
    ?LOGT("test udp receive from ~p:~p, data2=~p", [Address, Port, Packet]),
    #coap_message{type = con, method = Method, id=Id, token = Token, payload = Payload, options = Options} = coap_message_parser:decode(Packet),
    Path = get_path(Options, <<>>),
    Query = get_query(Options, []),
    ?LOGT("receive coap request Method=~p, Id=~p, Token=~p, Options=~p, Payload=~p, Path=~p, Query=~p", [Method, Id, Token, Options, Payload, Path, Query]),
    {Method, Id, Token, Path, Query, Payload}.


resolve_uri(Uri) ->
    {ok, {Scheme, _UserInfo, Host, PortNo, Path, Query}} =
        http_uri:parse(Uri, [{scheme_defaults, [{coap, ?DEFAULT_COAP_PORT}, {coaps, ?DEFAULT_COAPS_PORT}]}]),
    {ok, PeerIP} = inet:getaddr(Host, inet),
    {Scheme, {PeerIP, PortNo}, split_path(Path), split_query(Query)}.

split_path([]) -> [];
split_path([$/]) -> [];
split_path([$/ | Path]) -> split_segments(Path, $/, []).

split_query([]) -> [];
split_query([$? | Path]) -> split_segments(Path, $&, []).

split_segments(Path, Char, Acc) ->
    case string:rchr(Path, Char) of
        0 ->
            [make_segment(Path) | Acc];
        N when N > 0 ->
            split_segments(string:substr(Path, 1, N-1), Char,
                [make_segment(string:substr(Path, N+1)) | Acc])
    end.

make_segment(Seg) ->
    list_to_binary(http_uri:decode(Seg)).

get_path([], Acc) ->
    %?LOGT("get_path Acc=~p", [Acc]),
    Acc;
get_path([{uri_path, Path1}|T], Acc) ->
    %?LOGT("Path=~p, Acc=~p", [Path1, Acc]),
    get_path(T, join_path(Path1, Acc));
get_path([{_, _}|T], Acc) ->
    get_path(T, Acc).

get_query([], Acc) ->
    lists:reverse(Acc);
get_query([{uri_query, Q1}|T], Acc) ->
    get_query(T, [Q1|Acc]);
get_query([{_, _}|T], Acc) ->
    get_query(T, Acc).

join_path([], Acc) ->
    Acc;
join_path([H|T], Acc) ->
    join_path(T, <<Acc/binary, H/binary>>).


