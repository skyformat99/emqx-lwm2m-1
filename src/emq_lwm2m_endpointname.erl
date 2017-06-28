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

-module(emq_lwm2m_endpointname).

-author("Feng Lee <feng@emqtt.io>").

-include("emq_lwm2m.hrl").

-behaviour(gen_server).

%% API.
-export([start_link/0, register_name/1, unregister_location/1, get_epn/1, stop/0]).

%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {next_id = 0}).

-define(ETS_EPN_TAB, lwm2m_epn_to_location).
-define(ETS_LOCATION_TAB, lwm2m_location_to_epn).

-define(LOG(Level, Format, Args),
    lager:Level("CoAP-EPN: " ++ Format, Args)).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_name(EndPointName) ->
    gen_server:call(?MODULE, {register_name, EndPointName}).

unregister_location(Location) ->
    gen_server:call(?MODULE, {unregister_name, Location}).

get_epn(Location) ->
    case ets:lookup(?ETS_LOCATION_TAB, Location) of
        [] ->                undefined;
        [{Location, Id}] ->  Id
    end.

stop() ->
    gen_server:stop(?MODULE).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    ets:new(?ETS_EPN_TAB, [set, named_table, protected]),
    ets:new(?ETS_LOCATION_TAB, [set, named_table, protected]),
    {ok, #state{}}.

handle_call({register_name, Name}, _From, State=#state{next_id = Id}) ->
    case ets:lookup(?ETS_EPN_TAB, Name) of
        [] ->
            ets:insert(?ETS_EPN_TAB, {Name, Id}),
            ets:insert(?ETS_LOCATION_TAB, {Id, Name}),
            {reply, Id, State#state{next_id = Id+1}};
        [{Name, Id}]  ->
            {reply, Id, State}
    end;

handle_call({unregister_name, Location}, _From, State) ->
    case ets:lookup(?ETS_LOCATION_TAB, Location) of
        [] ->
            ok;
        [{Location, Name}] ->
            ets:delete(?ETS_EPN_TAB, Name),
            ets:delete(?ETS_LOCATION_TAB, Location)
    end,
	{reply, ok, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.



handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?ETS_EPN_TAB),
    ets:delete(?ETS_LOCATION_TAB),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

