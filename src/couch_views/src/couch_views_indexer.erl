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

-module(couch_views_indexer).

-export([
    update/2,
    update/4,

    % For tests
    map_docs/2,
    write_doc/4
]).


-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/src/fabric2.hrl").
-include_lib("eunit/include/eunit.hrl").

% TODO: 
%  * Handle timeouts of transaction and other errors

update(Db, Mrst) ->
    Noop = fun (_) -> ok end,
    update(Db, Mrst, Noop, []).


update(#{} = Db, Mrst, ProgressCallback, ProgressArgs)
        when is_function(ProgressCallback, 6) ->
    try
        Seq = couch_views_fdb:get_update_seq(Db, Mrst),
        State = #{
            since_seq => Seq,
            count => 0,
            limit => config:get_integer("couch_views", "change_limit", 100),
            doc_acc => [],
            last_seq => Seq,
            callback => ProgressCallback,
            callback_args => ProgressArgs,
            mrst => Mrst
        },
        update_int(Db, State)
    catch error:database_does_not_exist ->
        #{db_prefix := DbPrefix} = Db,
        couch_log:notice("couch_views_indexer stopped"
        "- ~p database does not exist", [DbPrefix])
    end.


update_int(#{} = Db, State) ->
    {ok, FinalState} = fabric2_fdb:transactional(Db, fun(TxDb) ->
        State1 = maps:put(tx_db, TxDb, State),
        fold_changes(State1)
    end),

    #{
        count := Count,
        limit := Limit,
        doc_acc := DocAcc,
        last_seq := LastSeq,
        callback := Cb,
        callback_args := CallbackArgs,
        mrst := Mrst
    } = FinalState,

    {MappedResults, Mrst1} = map_docs(Mrst, DocAcc),
    write_docs(Db, Mrst1, MappedResults, FinalState),

    case Count < Limit of
        true ->
            Cb(undefined, finished, CallbackArgs, Db, Mrst, LastSeq);
        false ->
            NextState = maps:merge(FinalState, #{
                limit => Limit,
                count => 0,
                doc_acc => [],
                since_seq => LastSeq,
                last_seq => 0,
                mrst => Mrst1
            }),
            update_int(Db, NextState)
    end.


fold_changes(State) ->
    #{
        since_seq := SinceSeq,
        limit := Limit,
        tx_db := TxDb
    } = State,

    fabric2_db:fold_changes(TxDb, SinceSeq,
        fun process_changes/2, State, [{limit, Limit}]).


process_changes(Change, Acc) ->
    #{
        doc_acc := DocAcc,
        count := Count,
        tx_db := TxDb,
        mrst := Mrst
    } = Acc,

    #{
        id := Id,
        sequence := LastSeq,
        deleted := Deleted
    } = Change,

    IncludeDesign = lists:keymember(<<"include_design">>, 1,
        Mrst#mrst.design_opts),

    Acc1 = case {Id, IncludeDesign} of
        {<<"_design/", _/binary>>, false} ->
            % {ok, Doc} = fabric2_db:open_doc(Db, Id),
            maps:merge(Acc, #{
                count => Count + 1,
                last_seq => LastSeq
                });
        _ ->

            % Making a note here that we should make fetching all the docs
            % a parallel fdb operation
            Doc = if Deleted -> []; true ->
                case fabric2_db:open_doc(TxDb, Id) of
                    {ok, Doc0} -> Doc0;
                    {not_found, _} -> []
                end
            end,

            Change1 = maps:put(doc, Doc, Change),
            maps:merge(Acc, #{
                doc_acc => DocAcc ++ [Change1],
                count => Count + 1,
                last_seq => LastSeq
            })
    end,
    {ok, Acc1}.


map_docs(Mrst, Docs) ->
    % Run all the non deleted docs through the view engine and
    Mrst1 = get_query_server(Mrst),
    QServer = Mrst1#mrst.qserver,

    MapFun = fun
        (#{deleted := true} = Change) ->
            maps:put(results, [], Change);

        (Change) ->
            #{doc := Doc} = Change,
            couch_stats:increment_counter([couchdb, mrview, map_doc]),
            {ok, RawResults} = couch_query_servers:map_doc_raw(QServer, Doc),
            JsonResults = couch_query_servers:raw_to_ejson(RawResults),
            ListResults = [[list_to_tuple(Res) || Res <- FunRs]
                || FunRs <- JsonResults],
            maps:put(results, ListResults, Change)
    end,
    MappedResults = lists:map(MapFun, Docs),
    {MappedResults, Mrst1}.


start_query_server(#mrst{} = Mrst) ->
    #mrst{
        language=Language,
        lib=Lib,
        views=Views
    } = Mrst,
    Defs = [View#mrview.def || View <- Views],
    {ok, QServer} = couch_query_servers:start_doc_map(Language, Defs, Lib),
    Mrst#mrst{qserver=QServer}.


get_query_server(#mrst{} = Mrst) ->
    case Mrst#mrst.qserver of
        nil -> start_query_server(Mrst);
        _ -> Mrst
    end.


write_docs(Db, Mrst, Docs, State) ->
    #mrst{
        views = Views,
        sig = Sig
    } = Mrst,

    #{
        callback := Cb,
        callback_args := CallbackArgs
    } = State,

    IdxNames = lists:map(fun (View) ->
        View#mrview.id_num
    end, Views),

    lists:foreach(fun (Doc) ->
        #{sequence := Seq} = Doc,
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            couch_views_fdb:update_view_seq(TxDb, Sig, Seq),
            Cb(TxDb, update, CallbackArgs, Db, Mrst, Seq),
            write_doc(TxDb, Sig, Doc, IdxNames)
        end)
    end, Docs).


write_doc(TxDb, Sig, #{deleted := true} = Doc, ViewIds) ->
    #{id := DocId} = Doc,
    lists:foreach(fun (IdxName) ->
        maybe_clear_id_and_map_index(TxDb, Sig, DocId, IdxName)
    end, ViewIds);

write_doc(TxDb, Sig, Doc, ViewIds) ->
    #{id := DocId, results := Results} = Doc,
    lists:foreach(fun
        ({IdxName, []}) ->
            maybe_clear_id_and_map_index(TxDb, Sig, DocId, IdxName);
        ({IdxName, IdxResults}) ->
            lists:foldl(fun (IdxResult, DocIdsCleared) ->
                {IdxKey, _} = IdxResult,
                OldIdxKey = couch_views_fdb:get_id_index(TxDb, Sig,
                    DocId, IdxName),
                IsAlreadyCleared = lists:member(DocId, DocIdsCleared),
                case OldIdxKey == not_found orelse IsAlreadyCleared == true of
                    true ->
                        couch_views_fdb:set_id_index(TxDb, Sig, IdxName,
                            DocId, IdxKey),
                        couch_views_fdb:set_map_index_results(TxDb, Sig,
                            IdxName, DocId, IdxResults);
                    false ->
                        couch_views_fdb:clear_id_index(TxDb, Sig,
                            DocId, IdxName),
                        couch_views_fdb:clear_map_index(TxDb, Sig, IdxName,
                            DocId, OldIdxKey),
                        couch_views_fdb:set_id_index(TxDb, Sig, DocId,
                            IdxName, IdxKey),
                        couch_views_fdb:set_map_index_results(TxDb, Sig,
                            IdxName, DocId, IdxResults)
                end,
                [DocId | DocIdsCleared]
            end, [], IdxResults)
    end, lists:zip(ViewIds, Results)).


maybe_clear_id_and_map_index(TxDb, Sig, DocId, IdxName) ->
    OldIdxKey = couch_views_fdb:get_id_index(TxDb, Sig,
        DocId, IdxName),
    if OldIdxKey == not_found -> ok; true ->
        couch_views_fdb:clear_id_index(TxDb, Sig,
            DocId, IdxName),
        couch_views_fdb:clear_map_index(TxDb, Sig, IdxName,
            DocId, OldIdxKey)
    end.
