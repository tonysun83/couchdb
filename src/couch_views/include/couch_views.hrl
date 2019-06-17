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

% indexing
-define(VIEW_UPDATE_SEQ, 1).
-define(VIEW_ID_RANGE,   2).
-define(VIEW_MAP_RANGE,  3).
-define(VIEW_BUILDS,     4).
-define(VIEW_STATUS,     5).
-define(VIEW_WATCH,      6).
-define(VIEW_ROW_KEY,    7).
-define(VIEW_ROW_VALUE,  8).

% jobs api
-define(INDEX_JOB_TYPE, <<"views">>).


-record(mrst, {
    sig=nil,
    fd=nil,
    db_name,
    idx_name,
    language,
    design_opts=[],
    seq_indexed=false,
    keyseq_indexed=false,
    partitioned=false,
    lib,
    views,
    % update_seq=0,
    % purge_seq=0,
    % first_build,
    % partial_resp_pid,
    % doc_acc,
    % doc_queue,
    % write_queue,
    qserver=nil
}).


-record(mrview, {
    id_num,
    % update_seq=0,
    % purge_seq=0,
    map_names=[],
    reduce_funs=[],
    def,
    seq_indexed=false,
    keyseq_indexed=false,
    options=[]
}).


-define(MAX_VIEW_LIMIT, 16#10000000).


-record(mrargs, {
    view_type,
    % reduce,

    % preflight_fun,

    start_key,
    start_key_docid,
    end_key,
    end_key_docid,
    keys,

    direction = fwd,
    limit = ?MAX_VIEW_LIMIT,
    skip = 0,
    % group_level = 0,
    % group = undefined,
    stable = false,
    update = true,
    multi_get = false,
    inclusive_end = true,
    include_docs = false,
    doc_options = [],
    update_seq=false,
    conflicts,
    % callback,
    sorted = true
    % extra = []
}).
