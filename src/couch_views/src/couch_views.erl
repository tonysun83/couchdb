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

-module(couch_views).

-export([
    map_query/6
]).

-include("couch_views.hrl").


map_query(Db, DDoc, ViewName, Callback, Acc0, Args0) ->
    Args = process_args(Args0),
    #{name := DbName} = Db,
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    maybe_build_index(Db, Mrst, Args),
    Resp = couch_views_reader:read(Db, DDoc, ViewName, Callback, Acc0, Args),

    UpdateAfter = maps:get(update, Args) == lazy,
    if UpdateAfter == false -> ok; true ->
        maybe_add_couch_job(Db, Mrst)
    end,
    Resp.


process_args(#{} = Args) ->
    Args1 = remove_ununsed_values(Args),
    Defaults = #{
            direction => fwd,
            inclusive_end => true,
            update => true,
            skip => 0,
            limit => ?MAX_VIEW_LIMIT
        },

    maps:merge(Defaults, Args1).


remove_ununsed_values(Args) ->
    maps:filter(fun (_, V) -> V /= undefined end, Args).


maybe_build_index(_Db, _Mrst, #{update := false}) ->
    false;

maybe_build_index(_Db, _Mrst, #{update := lazy}) ->
    false;

maybe_build_index(Db, Mrst, _Args) ->
    {Status, Seq} = fabric2_fdb:transactional(Db, fun(TxDb) ->
        case view_up_to_date(TxDb, Mrst) of
            {true, UpdateSeq} ->
                {ready, UpdateSeq};
            {false, LatestSeq} ->
                maybe_add_couch_job(TxDb, Mrst),
                {false, LatestSeq}
        end
    end),

    if Status == ready -> true; true ->
        subscribe_and_wait_for_index(Db, Mrst, Seq)
    end.


view_up_to_date(Db, Mrst) ->
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        UpdateSeq = couch_views_fdb:get_update_seq(TxDb, Mrst),
        LastChange = fabric2_fdb:get_last_change(TxDb),
        {UpdateSeq == LastChange, LastChange}
    end).


maybe_add_couch_job(TxDb, Mrst) ->
    case couch_views_jobs:status(TxDb, Mrst) of
        running ->
            ok;
        pending ->
            ok;
        Status when Status == finished orelse Status == not_found ->
            couch_views_jobs:add(TxDb, Mrst)
    end.


subscribe_and_wait_for_index(Db, Mrst, Seq) ->
    case couch_views_jobs:subscribe(Db, Mrst) of
        {error, Error} ->
            throw({error, Error});
        {ok, finished, _} ->
            ready;
        {ok, Subscription, _JobState, _} ->
            wait_for_index_ready(Subscription, Db, Mrst, Seq)
    end.


wait_for_index_ready(Subscription, Db, Mrst, Seq) ->
    Out = couch_views_jobs:wait(Subscription),
    case Out of
        {finished, _JobData} ->
            ready;
        {pending, _JobData} ->
            wait_for_index_ready(Subscription, Db, Mrst, Seq);
        {running, #{last_seq := LastSeq}} ->
            if LastSeq =< Seq -> ready; true ->
                wait_for_index_ready(Subscription, Db, Mrst, Seq)
            end;
        {running, _JobData} ->
            wait_for_index_ready(Subscription, Db, Mrst, Seq);
        {error, Error} ->
            throw({error, Error})
    end.
