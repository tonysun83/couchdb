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

-module(couch_views_indexer_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).

setup() ->
    test_util:start_couch([fabric]).


teardown(State) ->
    test_util:stop_couch(State).


foreach_setup() ->
    ok.


foreach_teardown(_) ->
    meck:unload().


index_server_test_() ->
    {
        "Test Couch Views indexer",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            {
                foreach,
                fun foreach_setup/0, fun foreach_teardown/1,
                [
                    ?TDEF(map_docs_no_results_for_deleted),
                    ?TDEF(map_docs_returns_sorted_results),
                    ?TDEF(write_doc_clears_for_deleted_doc),
                    ?TDEF(write_doc_adds_for_new_doc),
                    ?TDEF(write_doc_clears_and_sets_for_update),
                    ?TDEF(write_doc_clears_for_no_new_update),
                    ?TDEF(write_doc_clears_and_updates_duplicates)
                ]
            }

        }
    }.


map_docs_no_results_for_deleted() ->
    DbName = ?tempdb,

    DDoc = create_ddoc(),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),

    Doc = #{
        id => <<"id">>,
        sequence => <<1111>>,
        rev_id => <<"1-123">>,
        deleted => true
    },

    meck:expect(couch_query_servers, start_doc_map, fun(_, _, _) ->
        {ok, fake}
    end),

    {Results, _} = couch_views_indexer:map_docs(Mrst, [Doc]),

    [#{results := DocResult}] = Results,
    ?assertEqual([], DocResult).


map_docs_returns_sorted_results() ->
    DbName = ?tempdb,
    Doc = #{
        id => <<"id">>,
        sequence => <<1111>>,
        rev_id => <<"1-123">>,
        doc => doc(1)
    },

    CompleteResult = [[{1, 1}], []],

    DDoc = create_ddoc(),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),


    {Results, _} = couch_views_indexer:map_docs(Mrst, [Doc]),
    [#{results := DocResult}] = Results,
    ?assertEqual(CompleteResult, DocResult).


write_doc_clears_for_deleted_doc() ->
    TxDb = #{},
    Sig = <<123>>,
    Doc = #{deleted => true, id => 1},
    ViewIds = [1],
    OldIdxKey = old_key,

    meck:expect(couch_views_fdb, get_id_index, 4, old_key),
    meck:expect(couch_views_fdb, clear_id_index, 4, ok),
    meck:expect(couch_views_fdb, clear_map_index, 5, ok),

    couch_views_indexer:write_doc(TxDb, Sig, Doc, ViewIds),
    ?assert(meck:called(couch_views_fdb, get_id_index, [TxDb, Sig, 1, 1])),
    ?assert(meck:called(couch_views_fdb, clear_id_index, [TxDb, Sig, 1, 1])),
    ?assert(meck:called(couch_views_fdb, clear_map_index,
        [TxDb, Sig, 1, 1, OldIdxKey])),
    ?assertEqual(length(meck:history(couch_views_fdb)), 3).


write_doc_adds_for_new_doc() ->
    TxDb = #{},
    Sig = <<123>>,
    Key = <<"key">>,
    Value = 1,
    Results = [{Key, Value}],
    Doc = #{
        deleted => false,
        id => 1,
        results => [Results]
    },
    ViewIds = [1],

    meck:expect(couch_views_fdb, get_id_index, 4, not_found),
    meck:expect(couch_views_fdb, set_id_index, 5, ok),
    meck:expect(couch_views_fdb, set_map_index_results, 5, ok),

    couch_views_indexer:write_doc(TxDb, Sig, Doc, ViewIds),
    ?assert(meck:called(couch_views_fdb, get_id_index, [TxDb, Sig, 1, 1])),
    ?assert(meck:called(couch_views_fdb, set_id_index,
        [TxDb, Sig, 1, 1, Key])),
    ?assert(meck:called(couch_views_fdb, set_map_index_results,
        [TxDb, Sig, 1, 1, Results])),
    ?assertEqual(length(meck:history(couch_views_fdb)), 3).


write_doc_clears_and_sets_for_update() ->
    TxDb = #{},
    Sig = <<123>>,
    Key = <<"key">>,
    Value = 1,
    Results = [{Key, Value}],
    Doc = #{
        deleted => false,
        id => 1,
        results => [Results]
    },
    ViewIds = [1],
    OldKey = oldkey,

    meck:expect(couch_views_fdb, get_id_index, 4, OldKey),
    meck:expect(couch_views_fdb, clear_id_index, 4, ok),
    meck:expect(couch_views_fdb, clear_map_index, 5, ok),
    meck:expect(couch_views_fdb, set_id_index, 5, ok),
    meck:expect(couch_views_fdb, set_map_index_results, 5, ok),

    couch_views_indexer:write_doc(TxDb, Sig, Doc, ViewIds),
    ?assert(meck:called(couch_views_fdb, get_id_index, [TxDb, Sig, 1, 1])),
    ?assert(meck:called(couch_views_fdb, clear_id_index, [TxDb, Sig, 1, 1])),
    ?assert(meck:called(couch_views_fdb, clear_map_index,
        [TxDb, Sig, 1, 1, OldKey])),
    ?assert(meck:called(couch_views_fdb, set_id_index,
        [TxDb, Sig, 1, 1, Key])),
    ?assert(meck:called(couch_views_fdb, set_map_index_results,
        [TxDb, Sig, 1, 1, Results])),
    ?assertEqual(length(meck:history(couch_views_fdb)), 5).


write_doc_clears_for_no_new_update() ->
    TxDb = #{},
    Sig = <<123>>,
    Results = [],
    Doc = #{
        deleted => false,
        id => 1,
        results => [Results]
    },
    ViewIds = [1],
    OldKey = oldkey,

    meck:expect(couch_views_fdb, get_id_index, 4, OldKey),
    meck:expect(couch_views_fdb, clear_id_index, 4, ok),
    meck:expect(couch_views_fdb, clear_map_index, 5, ok),

    couch_views_indexer:write_doc(TxDb, Sig, Doc, ViewIds),
    ?assert(meck:called(couch_views_fdb, get_id_index, [TxDb, Sig, 1, 1])),
    ?assert(meck:called(couch_views_fdb, clear_id_index, [TxDb, Sig, 1, 1])),
    ?assert(meck:called(couch_views_fdb, clear_map_index,
        [TxDb, Sig, 1, 1, OldKey])),
    ?assertEqual(length(meck:history(couch_views_fdb)), 3).


write_doc_clears_and_updates_duplicates() ->
    TxDb = #{},
    Sig = <<123>>,
    Key = <<"key">>,
    Results = [{Key, 1}, {Key, 2}],
    Doc = #{
        deleted => false,
        id => 1,
        results => [Results]
    },
    ViewIds = [1],
    OldKey = oldkey,

    meck:expect(couch_views_fdb, get_id_index, 4, OldKey),
    meck:expect(couch_views_fdb, clear_id_index, 4, ok),
    meck:expect(couch_views_fdb, clear_map_index, 5, ok),
    meck:expect(couch_views_fdb, set_id_index, 5, ok),
    meck:expect(couch_views_fdb, set_map_index_results, 5, ok),

    couch_views_indexer:write_doc(TxDb, Sig, Doc, ViewIds),
    ?assertEqual(meck:num_calls(couch_views_fdb, get_id_index,
        [TxDb, Sig, 1, 1]), 2),
    ?assertEqual(meck:num_calls(couch_views_fdb, clear_id_index,
        [TxDb, Sig, 1, 1]), 1),
    ?assertEqual(meck:num_calls(couch_views_fdb, set_id_index,
        [TxDb, Sig, 1, 1, Key]), 2),
    ?assertEqual(meck:num_calls(couch_views_fdb, clear_map_index,
        [TxDb, Sig, 1, 1, OldKey]), 1),
    ?assertEqual(meck:num_calls(couch_views_fdb, set_map_index_results,
        [TxDb, Sig, 1, 1, Results]), 2),
    ?assertEqual(length(meck:history(couch_views_fdb)), 8).


create_ddoc() ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"map_fun1">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
            ]}},
            {<<"map_fun2">>, {[
                {<<"map">>, <<"function(doc) {}">>}
            ]}}
        ]}}
    ]}).


doc(Id) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"val">>, Id}
    ]}).
