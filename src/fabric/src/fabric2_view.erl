% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(fabric2_view).

-export([
    query/7
]).

-include_lib("couch_mrview/include/couch_mrview.hrl").

%% @doc execute a given view.
%%      There are many additional query args that can be passed to a view,
%%      see <a href="http://wiki.apache.org/couchdb/HTTP_view_API#Querying_Options">
%%      query args</a> for details.
% -spec query(db(), [{atom(), any()}] | [],
%         #doc{} | binary(), iodata(), callback(), any(), #mrargs{}) -> any().
query(Db, Options, DDoc, ViewName, Callback, Acc0, QueryArgs0) ->
    DbName = fabric2_db:name(Db),
%%    View = name(ViewName),
    case fabric_util:is_users_db(DbName) of
        true ->
            FakeDb = fabric_util:open_cluster_db(DbName, Options),
            couch_users_db:after_doc_read(DDoc, FakeDb);
        false ->
            ok
    end,
%%    {ok, #mrst{views=Views, language=Lang}} =
%%        couch_views_util:ddoc_to_mrst(DbName, DDoc),
%%    QueryArgs1 = couch_mrview_util:set_view_type(QueryArgs0, View, Views),
%%    QueryArgs1 = fabric_util:validate_args(Db, DDoc, QueryArgs0),
    QueryArgs1 = couch_mrview_util:validate_args(Db, DDoc, QueryArgs0),
%%    VInfo = couch_mrview_util:extract_view(Lang, QueryArgs1, View, Views),
    case is_reduce_view(QueryArgs1) of
        true ->
            throw({not_implemented});
        false ->
            MapQueryArgs = mrargs_to_map((QueryArgs1)),
            couch_views:map_query(Db, DDoc, ViewName, Callback,
                Acc0, MapQueryArgs)
    end.


is_reduce_view(_) ->
    false.


name(Thing) ->
    couch_util:to_binary(Thing).


mrargs_to_map(#mrargs{} = Args) ->
    #{
        start_key => Args#mrargs.start_key,
        start_key_docid => Args#mrargs.start_key_docid,
        end_key => Args#mrargs.end_key,
        end_key_docid => Args#mrargs.end_key_docid,
        keys => Args#mrargs.keys,
        direction => Args#mrargs.direction,
        limit => Args#mrargs.limit,
        skip => Args#mrargs.skip,
        update => Args#mrargs.update,
        multi_get => Args#mrargs.multi_get,
        inclusive_end => Args#mrargs.inclusive_end,
        include_docs => Args#mrargs.include_docs,
        doc_options => Args#mrargs.doc_options,
        update_seq => Args#mrargs.update_seq,
        conflicts => Args#mrargs.conflicts,
        sorted => Args#mrargs.sorted
    }.


