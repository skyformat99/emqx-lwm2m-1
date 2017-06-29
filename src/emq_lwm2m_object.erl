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

-module(emq_lwm2m_object).

-author("Feng Lee <feng@emqtt.io>").

-include("emq_lwm2m.hrl").
-include_lib("gen_coap/include/coap.hrl").


-export([get_object_id/1, get_object_and_resource_id/2]).

-define(LOG(Level, Format, Args),
    lager:Level("LWM2M-OBJ: " ++ Format, Args)).




get_object_id(ObjDefinition) ->
    proplists:get_value("ObjectID", ObjDefinition).


get_object_and_resource_id(ResourceNameBinary, ObjDefinition) ->
    ObjectId = proplists:get_value("ObjectID", ObjDefinition),
    ResourceList = proplists:get_value("Resources", ObjDefinition),
    ResourceId = search_res_name(ResourceNameBinary, ResourceList),
    {ObjectId, ResourceId}.

search_res_name(ResourceNameBinary, []) ->
    ?LOG(error, "~p is an invalid resource name", [ResourceNameBinary]),
    error(invalid_resource_name);
search_res_name(ResourceNameBinary, [{"Item", [{"ID", Id}], Attributes}|T]) ->
    Name = list_to_binary(proplists:get_value("Name", Attributes)),
    case Name of
        ResourceNameBinary -> Id;
        _Other             -> search_res_name(ResourceNameBinary, T)
    end.



