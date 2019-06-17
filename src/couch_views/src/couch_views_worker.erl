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

-module(couch_views_worker).

-export([
    start/2,
    job_progress/6
]).


start(Job, JobData) ->
    {ok, Db, Mrst} = get_indexing_info(JobData),
    % maybe we should spawn here
    couch_views_indexer:update(Db, Mrst, fun job_progress/6, Job).


job_progress(Tx, Progress, Job, Db, Mrst, LastSeq) ->
    case Progress of
        update ->
            couch_views_jobs:update(Tx, Job, Db, Mrst, LastSeq);
        finished ->
            couch_views_jobs:finish(Tx, Job, Db, Mrst, LastSeq)
    end.


get_indexing_info(JobData) ->
    #{
        <<"db_name">> := DbName,
        <<"ddoc_id">> := DDocId
    } = JobData,
    {ok, Db} = fabric2_db:open(DbName, []),
    {ok, DDoc} = fabric2_db:open_doc(Db, DDocId),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    {ok, Db, Mrst}.
