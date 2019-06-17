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

-module(couch_views_fdb).

-export([
    get_update_seq/2,
    update_view_seq/3,
    get_seq_key/2,

    clear_id_index/4,
    set_id_index/5,
    get_id_index/4,
    create_id_index_key/4,

    clear_map_index/5,
    set_map_index_results/5,
    get_map_index_key/4,
    get_map_range_keys/3,
    get_map_range/4,
    unpack_map_row/3
]).


-define(LIST_VALUE, 0).
-define(JSON_VALUE, 1).
-define(VALUE, 2).


-include_lib("fabric/src/fabric2.hrl").
-include("couch_views.hrl").

% View Build Sequence Access
% (<db>, ?DB_VIEWS, Sig, ?VIEW_UPDATE_SEQ) = Sequence

get_update_seq(Db, #mrst{sig = Sig}) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        Key = get_seq_key(Sig, DbPrefix),
        Tx = maps:get(tx, TxDb),
        case erlfdb:wait(erlfdb:get(Tx, Key)) of
            not_found -> 0;
            UpdateSeq -> UpdateSeq
        end
    end).


update_view_seq(Db, Sig, Seq) ->
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            db_prefix := DbPrefix,
            tx := Tx
        } = TxDb,
        SeqKey = get_seq_key(Sig, DbPrefix),
        erlfdb:set(Tx, SeqKey, Seq)
    end).


get_seq_key(Sig, DbPrefix) ->
    erlfdb_tuple:pack({?DB_VIEWS, Sig, ?VIEW_UPDATE_SEQ}, DbPrefix).


% Id Index access

% (<db>, ?VIEWS, <sig>, ?VIEW_ID_INDEX, <_id>, <view_id>) -> [emitted keys]

clear_id_index(TxDb, Sig, DocId, IdxName) ->
    #{
        db_prefix := DbPrefix,
        tx := Tx
    } = TxDb,
    IdKey = create_id_index_key(DbPrefix, Sig, DocId, IdxName),
    ok = erlfdb:clear(Tx, IdKey).


set_id_index(TxDb, Sig, IdxName, DocId, IdxKey) ->
    #{
        db_prefix := DbPrefix,
        tx := Tx
    } = TxDb,
    IdKey = create_id_index_key(DbPrefix, Sig, DocId, IdxName),
    erlfdb:set(Tx, IdKey, couch_views_encoding:encode(IdxKey)).


get_id_index(TxDb, Sig, Id, IdxName) ->
    #{
        db_prefix := DbPrefix,
        tx := Tx
    } = TxDb,
    IdKey = create_id_index_key(DbPrefix, Sig, Id, IdxName),
    case erlfdb:wait(erlfdb:get(Tx, IdKey)) of
        not_found -> not_found;
        IdxKey -> couch_views_encoding:decode(IdxKey)
    end.


create_id_index_key(DbPrefix, Sig, DocId, IdxName) ->
    BaseIdKey = {?DB_VIEWS, Sig, ?VIEW_ID_RANGE, DocId, IdxName},
    erlfdb_tuple:pack(BaseIdKey, DbPrefix).


% Map Index Access
% {<db>, ?DB_VIEWS, Sig, ?VIEW_MAP_RANGE, Idx, Key, DocId,
%   RowType, Counter} = Values
% RowType = Emitted Keys or Emitted Value


clear_map_index(TxDb, Sig, IdxName, DocId, IdxKeys) when is_list(IdxKeys) ->
    lists:foreach(fun (IdxKey) ->
        clear_map_index(TxDb, Sig, IdxName, DocId, IdxKey)
    end, IdxKeys);

clear_map_index(TxDb, Sig, IdxName, DocId, IdxKey) ->
    #{db_prefix := DbPrefix, tx := Tx} = TxDb,
    Key = couch_views_encoding:encode(IdxKey),
    BaseKey = {?DB_VIEWS, Sig, ?VIEW_MAP_RANGE, IdxName, Key, DocId},
    {StartKey, EndKey} = erlfdb_tuple:range(BaseKey, DbPrefix),
    ok = erlfdb:clear_range(Tx, StartKey, EndKey).


set_map_index_results(TxDb, Sig, IdxName, DocId, Results) ->
    #{db_prefix := DbPrefix, tx := Tx} = TxDb,
    lists:foldl(fun ({IdxKey, IdxValue}, Counter) ->
        RowKey = create_map_key(DbPrefix, Sig, IdxName, IdxKey, DocId,
            ?VIEW_ROW_KEY, Counter),
        RowValue = create_map_key(DbPrefix, Sig, IdxName, IdxKey, DocId,
            ?VIEW_ROW_VALUE, Counter),

        EncodedKey = pack_value(IdxKey),
        EncodedValue = pack_value(IdxValue),

        ok = erlfdb:set(Tx, RowKey, EncodedKey),
        ok = erlfdb:set(Tx, RowValue, EncodedValue),
        Counter + 1
    end, 0, Results).


get_map_index_key(#{db_prefix := DbPrefix}, Sig, IdxName, Key) ->
    EncKey = couch_views_encoding:encode(Key),
    erlfdb_tuple:pack({?DB_VIEWS, Sig, ?VIEW_MAP_RANGE,
            IdxName, EncKey}, DbPrefix).


get_map_range_keys(#{db_prefix := DbPrefix}, Sig, IdxName) ->
    erlfdb_tuple:range({?DB_VIEWS, Sig, ?VIEW_MAP_RANGE, IdxName}, DbPrefix).


get_map_range(TxDb, Start, End, Opts) ->
    #{tx := Tx} = TxDb,
    erlfdb:get_range(Tx, Start, End, Opts).


unpack_map_row(#{db_prefix := DbPrefix}, Key, Value) ->
    case erlfdb_tuple:unpack(Key, DbPrefix) of
        {?DB_VIEWS, _Sig, ?VIEW_MAP_RANGE, _Idx, _RowKey, Id,
            ?VIEW_ROW_KEY, _Counter} ->
            RowKey = unpack_value(Value),
            {key, Id, RowKey};

        {?DB_VIEWS, _Sig, ?VIEW_MAP_RANGE, _Idx, _RowValue, Id,
            ?VIEW_ROW_VALUE, _Counter} ->
            RowValue = unpack_value(Value),
            {value, Id, RowValue}
    end.


create_map_key(DbPrefix, Sig, IdxName, IdxKey, DocId, RowType, Counter) ->
    Key = couch_views_encoding:encode(IdxKey),
    BaseKey = {?DB_VIEWS, Sig, ?VIEW_MAP_RANGE,
        IdxName, Key, DocId, RowType, Counter},
    erlfdb_tuple:pack(BaseKey, DbPrefix).


% Internal used to packed and unpack Values


pack_value(Val) when is_list(Val) ->
    erlfdb_tuple:pack({?LIST_VALUE, list_to_tuple(Val)});

pack_value(Val) when is_tuple(Val) ->
    {Props} = Val,
    erlfdb_tuple:pack({?JSON_VALUE, list_to_tuple(Props)});

pack_value(Val) ->
    erlfdb_tuple:pack({?VALUE, Val}).


unpack_value(Bin) ->
    case erlfdb_tuple:unpack(Bin) of
        {?LIST_VALUE, Val} ->
            tuple_to_list(Val);
        {?JSON_VALUE, Val} ->
            {tuple_to_list(Val)};
        {?VALUE, Val} ->
            Val
    end.
