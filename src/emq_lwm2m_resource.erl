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

-module(emq_lwm2m_resource).

-author("Feng Lee <feng@emqtt.io>").

-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_protocol.hrl").
-include_lib("lwm2m_coap/include/coap.hrl").


-behaviour(lwm2m_coap_resource).

-export([coap_discover/2, coap_get/4, coap_post/5, coap_put/5, coap_delete/3,
    coap_observe/4, coap_unobserve/1, handle_info/2, coap_ack/2]).

-include("emq_lwm2m.hrl").

-define(LWM2M_REGISTER_PREFIX, <<"rd">>).

-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-RESOURCE: " ++ Format, Args)).

-record(lwm2m_query, {epn, life_time, sms, lwm2m_ver}).

-record(lwm2m_context, {epn, location, life_time}).


% resource operations
coap_discover(_Prefix, _Args) ->
    [{absolute, "mqtt", []}].

coap_get(ChId, [?LWM2M_REGISTER_PREFIX], Name, Query) ->
    ?LOG(debug, "~p ~p GET Name=~p, Query=~p~n", [self(),ChId, Name, Query]),
    #coap_content{};
coap_get(ChId, Prefix, Name, Query) ->
    ?LOG(error, "ignore bad put request ChId=~p, Prefix=~p, Name=~p, Query=~p", [ChId, Prefix, Name, Query]),
    {error, bad_request}.

% LWM2M REGISTER COMMAND
coap_post(ChId, [?LWM2M_REGISTER_PREFIX], [], Query, Content) ->
    #lwm2m_query{epn = Epn, lwm2m_ver = Ver, life_time = LifeTime} = parse_query(Query),
    case check_lwm2m_version(Ver) of
        true ->
            Location = list_to_binary(io_lib:format("~.16B", [random:uniform(65535)])),
            ?LOG(debug, "~p ~p REGISTER command Query=~p, Content=~p, Location=~p", [self(), ChId, Query, Content, Location]),
            put(lwm2m_context, #lwm2m_context{epn = Epn, location = Location, life_time = LifeTime}),
            % TODO: parse content
            emq_lwm2m_mqtt_adapter:start_link(self(), Epn, ChId),
            {ok, created, #coap_content{payload = list_to_binary(io_lib:format("/rd/~s", [Location]))}};
        false ->
            ?LOG(error, "refuse REGISTER from ~p due to wrong LWM2M version ~p", [ChId, Ver]),
            quit(ChId),
            {error, not_acceptable}
    end;


% LWM2M UPDATE COMMAND
coap_post(ChId, [?LWM2M_REGISTER_PREFIX], [Location], Query, Content) ->
    #lwm2m_query{life_time = LifeTime} = parse_query(Query),
    #lwm2m_context{location = TrueLocation} = get(lwm2m_context),
    ?LOG(debug, "~p ~p UPDATE command location=~p, LifeTime=~p, Query=~p, Content=~p", [self(), ChId, Location, LifeTime, Query, Content]),
    % TODO: update lifetime
    % TODO: parse content
    case Location of
        TrueLocation ->
            {ok, changed, #coap_content{}};
        _Other       ->
            ?LOG(error, "Location mismatch ~p vs ~p", [Location, TrueLocation]),
            {error, bad_request}
    end;
coap_post(ChId, Prefix, Name, Query, Content) ->
    ?LOG(error, "bad post request ChId=~p, Prefix=~p, Name=~p, Query=~p, Content=~p", [ChId, Prefix, Name, Query, Content]),
    {error, bad_request}.

coap_put(_ChId, Prefix, Name, Query, Content) ->
    ?LOG(error, "put has error, Prefix=~p, Name=~p, Query=~p, Content=~p", [Prefix, Name, Query, Content]),
    {error, bad_request}.

% LWM2M DE-REGISTER COMMAND
coap_delete(ChId, [?LWM2M_REGISTER_PREFIX], [Location]) ->
    #lwm2m_context{location = TrueLocation} = get(lwm2m_context),
    ?LOG(debug, "~p ~p DELETE command location=~p", [self(), ChId, Location]),
    case Location of
        TrueLocation ->
            emq_lwm2m_mqtt_adapter:stop(ChId),
            quit(ChId);
        _Other ->
            ?LOG(error, "ignore DE-REGISTER command due to mismatch location ~p vs ~p", [Location, TrueLocation]),
            ignore
    end,
    ok;
coap_delete(_ChId, _Prefix, _Name) ->
    {error, bad_request}.


coap_observe(ChId, Prefix, Name, Ack) ->
    ?LOG(error, "unknown observe request ChId=~p, Prefix=~p, Name=~p, Ack=~p", [ChId, Prefix, Name, Ack]),
    {error, method_not_allowed}.

coap_unobserve({state, ChId, Prefix, Name}) ->
    ?LOG(error, "ignore unknown unobserve request ChId=~p, Prefix=~p, Name=~p", [ChId, Prefix, Name]),
    ok.

handle_info({dispatch_command, CoapRequest, Ref}, _ObState) ->
    ?LOG(debug, "dispatch_command CoapRequest=~p, Ref=~p", [CoapRequest, Ref]),
    {send_request, CoapRequest, Ref};

handle_info({coap_response, ChId, _Channel, Ref, #coap_message{method = Method, payload = Payload, options = Options}}, ObState) ->
    DataFormat = data_format(Options),
    emq_lwm2m_mqtt_adapter:publish(ChId, Method, Payload, DataFormat, Ref),
    {noreply, ObState};

handle_info(Message, State) ->
    ?LOG(error, "Unknown Message ~p", [Message]),
    {noreply, State}.

coap_ack(_Ref, State) -> {ok, State}.


parse_query(InputQuery) ->
    parse_query(InputQuery, #lwm2m_query{}).

parse_query([], Query=#lwm2m_query{}) ->
    Query;
parse_query([<<$e, $p, $=, Rest/binary>>|T], Query=#lwm2m_query{}) ->
    parse_query(T, Query#lwm2m_query{epn = Rest});
parse_query([<<$l, $t, $=, Rest/binary>>|T], Query=#lwm2m_query{}) ->
    parse_query(T, Query#lwm2m_query{life_time = binary_to_integer(Rest)});
parse_query([<<$l, $w, $m, $2, $m, $=, Rest/binary>>|T], Query=#lwm2m_query{}) ->
    parse_query(T, Query#lwm2m_query{lwm2m_ver = Rest}).

data_format([]) ->
    <<"text/plain">>;
data_format([{content_format, Format}|_]) ->
    Format;
data_format([{_, _}|T]) ->
    data_format(T).



check_lwm2m_version(<<"1.0">>) -> true;
check_lwm2m_version(<<"1">>)   -> true;
check_lwm2m_version(_)         -> false.


quit(ChId) ->
    self() !{coap_error, ChId, undefined, [], shutdown}.


% end of file



