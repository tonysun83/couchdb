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

-module(couch_views_reader).

-export([
    read/6
]).


-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/src/fabric2.hrl").


read(Db, DDoc, ViewName, Callback, Acc0, Args) ->
    #{name := DbName} = Db,

    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    #mrst{
        sig = Sig,
        views = Views
    } = Mrst,

    IdxName = get_idx_name(ViewName, Views),
    State0 = #{
        acc => Acc0,
        skip => maps:get(skip, Args, 0),
        include_docs => maps:get(include_docs, Args, false),
        db => Db
    },

    DefaultOpts = [{streaming_mode, want_all}],
    {Start, End, QueryOpts} = convert_args_to_fdb(Db, Sig, IdxName, Args,
        DefaultOpts),
    Opts = QueryOpts ++ DefaultOpts,

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        Future = couch_views_fdb:get_map_range(TxDb, Start, End, Opts),

        UnPack = get_unpack_fun(TxDb, Opts, Callback),
        State1 = lists:foldl(UnPack, State0, erlfdb:wait(Future)),

        #{acc := Acc1} = State1,
        Callback(complete, Acc1)
    end).


get_idx_name(ViewName, Views) ->
    {value, View} = lists:search(fun (View) ->
        lists:member(ViewName, View#mrview.map_names)
    end, Views),
    View#mrview.id_num.


convert_args_to_fdb(Db, Sig, IdxName, Args, Opts) ->
    #{
        direction := Direction
    } = Args,

    {Start1, End1} = get_range_keys(Db, Sig, IdxName, Args),

    Opts1 = case maps:is_key(limit, Args) of
        false ->
            Opts;
        true ->
            Skip = maps:get(skip, Args, 0),
            Limit = maps:get(limit, Args),
            % Limit is multiplied by two because there are two rows per key
            % value.
            % Skip is added because that is done in the fold so we need
            % to fetch the number of documents
            % along with the docs we would skip.
            % Limit = (Doc limit + Skip) * Num of Rows per Map KV
            [{limit, (Limit + Skip) * 2} | Opts]
    end,

    Opts2 = case Direction of
        fwd ->
            Opts1;
        rev ->
            [{reverse, true} | Opts1]
    end,
    {Start1, End1, Opts2}.


get_range_keys(Db, Sig, IdxName, Args) ->
    #{
        inclusive_end := InclusiveEnd,
        direction := Direction
    } = Args,

    {MapStartKey, MapEndKey} = case Direction of
        fwd -> {start_key, end_key};
        rev -> {end_key, start_key}
    end,

    {Start0, End0} = couch_views_fdb:get_map_range_keys(Db, Sig, IdxName),

    Start1 = case maps:is_key(MapStartKey, Args) of
        false ->
            Start0;
        true ->
            StartKey = maps:get(MapStartKey, Args),
            Start = couch_views_fdb:get_map_index_key(Db, Sig, IdxName,
                StartKey),
            erlfdb_key:first_greater_or_equal(Start)
    end,

    End1 = case maps:is_key(MapEndKey, Args) of
        false ->
            End0;
        true ->
            EndKey = maps:get(MapEndKey, Args),
            EndBin = couch_views_fdb:get_map_index_key(Db, Sig, IdxName,
                EndKey),
            EndBin1 = case InclusiveEnd of
                true -> <<EndBin/binary, 16#FF>>;
                false -> EndBin
            end,
            erlfdb_key:first_greater_than(EndBin1)
    end,
    {Start1, End1}.


get_unpack_fun(TxDb, Opts, Callback) ->
    UnPackFwd = fun({K, V}, State) ->
        case couch_views_fdb:unpack_map_row(TxDb, K, V) of
            {key, _Id, RowKey} ->
                maps:put(current_key, RowKey, State);
            {value, Id, RowValue} ->
                #{
                    current_key := RowKey,
                    acc := Acc,
                    skip := Skip,
                    db := Db
                } = State,

                case Skip > 0 of
                    true ->
                        maps:put(skip, Skip - 1, State);
                    false ->
                        Row = [{id, Id}, {key, RowKey}, {value, RowValue}],

                        IncludeDoc = maps:get(include_docs, State, false),
                        Row1 = maybe_include_doc(Db, Id, Row, IncludeDoc),

                        {ok, AccNext} = Callback({row, Row1}, Acc),
                        maps:put(acc, AccNext, State)
                end
        end
    end,

    UnPackRev = fun({K, V}, State) ->
        case couch_views_fdb:unpack_map_row(TxDb, K, V) of
            {key, Id, RowKey} ->
                #{
                    current_value := RowValue,
                    acc := Acc,
                    skip := Skip,
                    db := Db
                } = State,

                case Skip > 0 of
                    true ->
                        maps:put(skip, Skip - 1, State);
                    false ->
                        Row = [{id, Id}, {key, RowKey}, {value, RowValue}],

                        IncludeDoc = maps:get(include_docs, State, false),
                        Row1 = maybe_include_doc(Db, Id, Row, IncludeDoc),

                        {ok, AccNext} = Callback({row, Row1}, Acc),
                        maps:put(acc, AccNext, State)
                end;
            {value, _Id, RowValue} ->
                maps:put(current_value, RowValue, State)
        end
    end,

    case lists:keyfind(reverse, 1, Opts) of
        {reverse, true} -> UnPackRev;
        _ -> UnPackFwd
    end.


maybe_include_doc(_Db, _Id, Row, false) ->
    Row;

maybe_include_doc(Db, Id, Row, true) ->
    Doc1 = case fabric2_db:open_doc(Db, Id) of
        {ok, Doc} -> couch_doc:to_json_obj(Doc, []);
        {not_found, _} -> []
    end,
    Row ++ [{doc, Doc1}].
