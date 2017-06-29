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

-module(emq_lwm2m_object_database).

-author("Feng Lee <feng@emqtt.io>").

-include("emq_lwm2m.hrl").
-include_lib("gen_coap/include/coap.hrl").

%% API
-export([start_link/0, stop/0, find_name/1, find_objectid/1]).

%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-OBJ-DB: " ++ Format, Args)).

-define(LWM2M_OBJECT_DEF_TAB, lwm2m_object_def_tab).
-define(LWM2M_OBJECT_NAME_TO_ID_TAB, lwm2m_object_name_to_id_tab).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

find_objectid(ObjectId) ->
    case ets:lookup(?LWM2M_OBJECT_DEF_TAB, ObjectId) of
        []                -> undefined;
        [{ObjectId, Xml}] -> Xml
    end.

find_name(Name) ->
    case ets:lookup(?LWM2M_OBJECT_NAME_TO_ID_TAB, Name) of
        []                 ->
            undefined;
        [{Name, ObjectId}] ->
            case ets:lookup(?LWM2M_OBJECT_DEF_TAB, ObjectId) of
                []                -> undefined;
                [{ObjectId, Xml}] -> Xml
            end
    end.

stop() ->
    gen_server:stop(?MODULE).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    ets:new(?LWM2M_OBJECT_DEF_TAB, [set, named_table, protected]),
    ets:new(?LWM2M_OBJECT_NAME_TO_ID_TAB, [set, named_table, protected]),
    BaseDir = application:get_env(?APP, xml_dir, "etc/lwm2m_xml"),
    load(BaseDir),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?LWM2M_OBJECT_DEF_TAB),
    ets:delete(?LWM2M_OBJECT_NAME_TO_ID_TAB),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

load(BaseDir) ->
    AllXmlFiles = filelib:wildcard(BaseDir++"/*.xml"),
    load_loop(AllXmlFiles).

load_loop([]) ->
    ok;
load_loop([FileName|T]) ->
    Xml = load_xml(FileName),
    ObjectId = proplists:get_value("ObjectID", Xml),
    Name = proplists:get_value("Name", Xml),
    ets:insert(?LWM2M_OBJECT_DEF_TAB, {ObjectId, Xml}),
    ets:insert(?LWM2M_OBJECT_NAME_TO_ID_TAB, {Name, ObjectId}),
    load_loop(T).


load_xml(FileName) ->
    {ok, XmlString} = file:read_file(FileName),
    {ok, Xml} = erlsom:simple_form(XmlString),
    Xml.

