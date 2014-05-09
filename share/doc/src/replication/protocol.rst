.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

.. _replication/protocol:

============================
CouchDB Replication Protocol
============================

:Version: 3

The `CouchDB Replication Protocol` is a protocol for synchronising JSON
documents between 2 peers over HTTP/1.1 by using the public :ref:`CouchDB REST
API <api>` and is based on the Apache CouchDB MVCC_ Data model.


Preface
=======

Language
--------

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in :rfc:`2119`.


Goals
-----

The primary goal of this specification is to describe the `CouchDB Replication
Protocol` under the hood.

The secondary goal is to provide enough detailed information about the protocol
to make it easy to build tools on any language and platform that can synchronize
data with CouchDB.


Definitions
-----------

JSON:
    :abbr:`JSON (JavaScript Object Notation)` is a text format for the
    serialization of structured data. It is described in `ECMA-262`_ and
    :rfc:`4627`.

URI:
    An URI is defined by :rfc:`2396`. It can be an URL as defined
    in :rfc:`1738`.

ID:
    An identifier (could be a UUID) as described in :rfc:`4122`.

Revision:
    A `MVCC`_ token value of following pattern: ``N-sig`` where ``N`` is ALWAYS
    a positive integer and ``sig`` is the Document signature (custom).
    Don't mix it up with the revision in version control systems!

Leaf Revision:
    The last Document Revision in a series of changes. Documents may have
    multiple Leaf Revisions (aka Conflict Revisions) due to concurrent updates.

Document:
    A document is a JSON object with an ID and Revision defined in ``_id`` and
    ``_rev`` fields respectively. Document's ID MUST be unique across
    the Database where it stored.

Database:
    A collection of Documents with a unique URI.

Changes Feed:
    A stream of Document-changing events (create, update, delete) for
    the specified Database.

Sequence ID:
    An ID provided by the Changes Feed. It MUST be incremental,
    but SHOULD NOT be always an integer.

Source:
    Database from where the Documents are replicated.

Target:
    Database where the Documents are replicated to.

Replication:
    The one-way directed synchronization process of Source and Target endpoints.

Checkpoint:
    Intermediate Recorded Sequence ID that used for Replication recovery.

Replicator:
    A service or an application which initiates and runs Replication.

Filter Function:
    A special function of any programming language that is used to filter
    Documents during Replication (see :ref:`filterfun`)

Filter Function Name:
    An ID of a Filter Function that may be used as a symbolic reference (aka
    callback function) to apply the related Filter Function to Replication.

Filtered Replication:
    Replication of Documents from Source to Target which pass a Filter Function.

Full Replication:
    Replication of all Documents from Source to Target.

Push Replication:
    Replication process where Source is a local endpoint and Target is remote.

Pull Replication:
    Replication process where Source is a remote endpoint and Target is local.

Continuous Replication:
    Replication that "never stops": after processing all events from
    Changes Feed, Replicator doesn't close the connection, but awaits new change
    events from the Source. The connection keeps alive by periodical heartbeats.

Replication Log:
    A special Document that holds Replication history (recorded Checkpoints
    and few more statistics) between Source and Target.

Replication ID:
    A unique value that unambiguously identifies the Replication Log.


Replication Protocol Algorithm
==============================

The `CouchDB Replication Protocol` is not something *magical*, but
an agreement on usage of the public :ref:`CouchDB HTTP REST API <api>` in some
specific way to effectively replicate Documents from Source to Target.

The reference implementation, written in Erlang_, is provided by the
couch_replicator_ module in Apache CouchDB.

It is RECOMMENDED to follow this algorithm specification and use the same
HTTP endpoints and run requests with the same parameters to provide completely
compatible solution. Custom Replicator implementations MAY use different
HTTP API endpoints and requests parameters depending on their local specifics
as like as they MAY implement only part of Replication Protocol to run only Push
or Pull Replication. However, while such solutions could also run Replication
process, they loose compatibility with CouchDB Replicator.


Verify Peers
------------

.. code-block:: text

  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+
  ' Verify Peers:                                                                  '
  '                                                                                '
  '                  404 Not Found   +---------------------------------+           '
  '       +------------------------- |     Check Source Existence      |           '
  '       |                          +---------------------------------+           '
  '       |                          |          HEAD /source           |           '
  '       |                          +---------------------------------+           '
  '       |                            |                                           '
  '       |                            | 200 OK                                    '
  '       |                            v                                           '
  '       |                          +---------------------------------+           '
  '       |                          |     Check Target Existence      | ---+      '
  '       |                          +---------------------------------+    |      '
  '       |                          |          HEAD /target           |    |      '
  '       |                          +---------------------------------+    |      '
  '       |                            |                                    |      '
  '       |                            | 404 Not Found                      |      '
  '       v                            v                                    |      '
  '   +-------+      404 Not Found   +---------------------------------+    |      '
  '   | Abort | <------------------- |      May be Create Target?      |    |      '
  '   +-------+                      +---------------------------------+    |      '
  '       ^                            |                                    |      '
  '       |                            | Yes                         200 OK |      '
  '       |                            v                                    |      '
  '       |          Failure         +---------------------------------+    |      '
  '       +------------------------- |          Create Target          |    |      '
  '                                  +---------------------------------+    |      '
  '                                  |           PUT /target           |    |      '
  '                                  +---------------------------------+    |      '
  '                                    |                                    |      '
  + - - - - - - - - - - - - - - - - -  | - - - - - - - - - - - - - - - - -  | - - -+
                                       |                                    |
  + - - - - - - - - - - - - - - - - -  | - - - - - - - - - - - - - - - - -  | - - -+
  ' Get Peers Information:             |                                    |      '
  '                                    | 201 Created                        |      '
  '                                    v                                    |      '
  '                                  +---------------------------------+    |      '
  '                                  |     Get Source Information      | <--+      '
  '                                  +---------------------------------+           '
  '                                                                                '
  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+

First of all, Replicator SHOULD ensure that both Source and Target are exists
by using :head:`/{db}` requests.


Check Source Existence
^^^^^^^^^^^^^^^^^^^^^^

  **Request**:

  .. code-block:: http

    HEAD /source HTTP/1.1
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Sat, 05 Oct 2013 08:50:39 GMT
    Server: CouchDB (Erlang/OTP)


Check Source Existence
^^^^^^^^^^^^^^^^^^^^^^

  **Request**:

  .. code-block:: http

    HEAD /target HTTP/1.1
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Sat, 05 Oct 2013 08:51:11 GMT
    Server: CouchDB (Erlang/OTP)


May be Create Target?
^^^^^^^^^^^^^^^^^^^^^

In case of non-existent Target, Replicator MAY made additional :put:`/{db}`
request to create the Target:

  **Request**:

  .. code-block:: http

    PUT /target HTTP/1.1
    Accept: application/json
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Content-Length: 12
    Content-Type: application/json
    Date: Sat, 05 Oct 2013 08:58:41 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "ok": true
    }

However, Replicator MAY NOT succeeded on this operation due to insufficient
privileges (which are granted by provided credential) and receiving
:statuscode:`401` or :statuscode:`403` error SHOULD be expected and
well handled:

  .. code-block:: http

    HTTP/1.1 500 Internal Server Error
    Cache-Control: must-revalidate
    Content-Length: 108
    Content-Type: application/json
    Date: Fri, 09 May 2014 13:50:32 GMT
    Server: CouchDB (Erlang OTP)

    {
      "error": "unauthorized",
      "reason": "unauthorized to access or create database http://localhost:5984/target"
    }


Abort
^^^^^

In case of non-existent Source or Target, Replication SHOULD be aborted with
an HTTP error response:

  .. code-block:: http

    HTTP/1.1 500 Internal Server Error
    Cache-Control: must-revalidate
    Content-Length: 56
    Content-Type: application/json
    Date: Sat, 05 Oct 2013 08:55:29 GMT
    Server: CouchDB (Erlang OTP)

    {
      "error": "db_not_found",
      "reason": "could not open source"
    }


Get Peers Information
---------------------

.. code-block:: text

  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+
  ' Verify Peers:                                                    '
  '                         +------------------------+               '
  '                         | Check Target Existence |               '
  '                         +------------------------+               '
  '                                     |                            '
  '                                     | 200 OK                     '
  '                                     |                            '
  + - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - -+
                                        |
  + - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - -+
  ' Get Peers Information:              |                            '
  '                                     v                            '
  '                         +------------------------+               '
  '                         | Get Source Information |               '
  '                         +------------------------+               '
  '                         |      GET /source       |               '
  '                         +------------------------+               '
  '                                     |                            '
  '                                     | 200 OK                     '
  '                                     v                            '
  '                         +------------------------+               '
  '                         | Get Target Information |               '
  '                         +------------------------+               '
  '                         |      GET /target       |               '
  '                         +------------------------+               '
  '                                     |                            '
  '                                     | 200 OK                     '
  '                                     |                            '
  + - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - -+
                                        |
  + - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - -+
  ' Find out Common Ancestry:           |                            '
  '                                     |                            '
  '                                     v                            '
  '                         +-------------------------+              '
  '                         | Generate Replication ID |              '
  '                         +-------------------------+              '
  '                                                                  '
  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+


Replicator retrieves basic information both from Source and Target using
:get:`/{db}` request to them. The response MUST contains JSON object with
the next mandatory fields:

- **instance_start_time** (*string*): Timestamp of when the Database was
  opened, expressed in *microseconds* since the epoch.
- **update_seq** (*number* / *string*): The current database Sequence ID.

Any other fields are optional. The information that Replicator seeks
is the ``update_seq`` field: this value will be used to define *temporary*
(because Database data always could be updated) upper bounder for changes feed
listening and statistic calculating to show proper Replication progress.


Get Source Information
^^^^^^^^^^^^^^^^^^^^^^

  **Request**:

  .. code-block:: http

    GET /source HTTP/1.1
    Accept: application/json
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 256
    Content-Type: application/json
    Date: Tue, 08 Oct 2013 07:53:08 GMT
    Server: CouchDB (Erlang OTP)

    {
      "committed_update_seq": 61772,
      "compact_running": false,
      "data_size": 70781613961,
      "db_name": "source",
      "disk_format_version": 6,
      "disk_size": 79132913799,
      "doc_count": 41961,
      "doc_del_count": 3807,
      "instance_start_time": "1380901070238216",
      "purge_seq": 0,
      "update_seq": 61772
    }


Get Target Information
^^^^^^^^^^^^^^^^^^^^^^

  **Request**:

  .. code-block:: http

    GET /target/ HTTP/1.1
    Accept: application/json
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Content-Length: 363
    Content-Type: application/json
    Date: Tue, 08 Oct 2013 12:37:01 GMT
    Server: CouchDB (Erlang/OTP)

    {
        "compact_running": false,
        "db_name": "target",
        "disk_format_version": 5,
        "disk_size": 77001455,
        "doc_count": 1832,
        "doc_del_count": 1,
        "instance_start_time": "0",
        "other": {
            "data_size": 50829452
        },
        "purge_seq": 0,
        "update_seq": "1841-g1AAAADveJzLYWBgYMlgTmGQT0lKzi9KdUhJMtbLSs1LLUst0kvOyS9NScwr0ctLLckBKmRKZEiy____f1YSAwPjSaJ1JTkAyaR6mMYrRGvMYwGSDA1ACqh3P1jzKRI1H4Bohth8KgsAfSlO9w"
    }


Find out Common Ancestry
------------------------

.. code-block:: text

  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  ' Get Peers Information:                                                    '
  '                                                                           '
  '                             +-------------------------------------------+ '
  '                             |           Get Target Information          | '
  '                             +-------------------------------------------+ '
  '                               |                                           '
  + - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - +
                                  |
  + - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - +
  ' Find out Common Ancestry:     v                                           '
  '                             +-------------------------------------------+ '
  '                             |          Generate Replication ID          | '
  '                             +-------------------------------------------+ '
  '                               |                                           '
  '                               |                                           '
  '                               v                                           '
  '                             +-------------------------------------------+ '
  '                             |      Get Replication Log from Source      | '
  '                             +-------------------------------------------+ '
  '                             |     GET /source/_local/replication-id     | '
  '                             +-------------------------------------------+ '
  '                               |                                           '
  '                               | 200 OK                                    '
  '                               | 404 Not Found                             '
  '                               v                                           '
  '                             +-------------------------------------------+ '
  '                             |      Get Replication Log from Target      | '
  '                             +-------------------------------------------+ '
  '                             |     GET /target/_local/replication-id     | '
  '                             +-------------------------------------------+ '
  '                               |                                           '
  '                               | 200 OK                                    '
  '                               | 404 Not Found                             '
  '                               v                                           '
  '                             +-------------------------------------------+ '
  '                             |          Compare Replication Logs         | '
  '                             +-------------------------------------------+ '
  '                               |                                           '
  '                               | Use latest common sequence as start point '
  '                               |                                           '
  + - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - +
                                  |
                                  |
  + - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - +
  ' Locate Changed Documents:     |                                           '
  '                               |                                           '
  '                               v                                           '
  '                             +-------------------------------------------+ '
  '                             |        Listen Source Changes Feed         | '
  '                             +-------------------------------------------+ '
  '                                                                           '
  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +


Generate Replication ID
^^^^^^^^^^^^^^^^^^^^^^^

Before Replication will be started, Replicator MUST generate the Replication ID.
This value is used to track Replication History, resume and continue previously
interrupted Replication process.

The algorithm of Replication ID generation is depends on Replicator
implementation with the only one restriction: it MUST unique define Replication
process. As for CouchDB Replicator, the algorithm takes into account:

- Persistent Peer UUID value. For CouchDB, the local
  :config:option:`Server UUID <couchdb/uuid>` is used
- Source and Target URI and is Source or Target local or remote Databases
- If Target need to be created or not
- If Replication Continuous or not
- OAuth headers if any
- Any custom headers
- :ref:`Filter function <filterfun>` code if used
- Changes Feed query parameters if any

.. note::

   See `couch_replicator_utils.erl`_ for the detailed Replication ID generation
   implementation.

   .. _couch_replicator_utils.erl: https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=blob;f=src/couch_replicator/src/couch_replicator_utils.erl;h=d7778db;hb=HEAD


Retrieve Replication Logs from Source and Target
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once Replication ID have been generated, Replicator SHOULD seek Replication Log
by this ID value both on Source and Target using :get:`/{db}/_local/{docid}`
request:

  **Request**:

  .. code-block:: http

    GET /source/_local/b3e44b920ee2951cb2e123b63044427a HTTP/1.1
    Accept: application/json
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 1019
    Content-Type: application/json
    Date: Thu, 10 Oct 2013 06:18:56 GMT
    ETag: "0-8"
    Server: CouchDB (Erlang OTP)

    {
      "_id": "_local/b3e44b920ee2951cb2e123b63044427a",
      "_rev": "0-8",
      "history": [
        {
          "doc_write_failures": 0,
          "docs_read": 2,
          "docs_written": 2,
          "end_last_seq": 5,
          "end_time": "Thu, 10 Oct 2013 05:56:38 GMT",
          "missing_checked": 2,
          "missing_found": 2,
          "recorded_seq": 5,
          "session_id": "d5a34cbbdafa70e0db5cb57d02a6b955",
          "start_last_seq": 3,
          "start_time": "Thu, 10 Oct 2013 05:56:38 GMT"
        },
        {
          "doc_write_failures": 0,
          "docs_read": 1,
          "docs_written": 1,
          "end_last_seq": 3,
          "end_time": "Thu, 10 Oct 2013 05:56:12 GMT",
          "missing_checked": 1,
          "missing_found": 1,
          "recorded_seq": 3,
          "session_id": "11a79cdae1719c362e9857cd1ddff09d",
          "start_last_seq": 2,
          "start_time": "Thu, 10 Oct 2013 05:56:12 GMT"
        },
        {
          "doc_write_failures": 0,
          "docs_read": 2,
          "docs_written": 2,
          "end_last_seq": 2,
          "end_time": "Thu, 10 Oct 2013 05:56:04 GMT",
          "missing_checked": 2,
          "missing_found": 2,
          "recorded_seq": 2,
          "session_id": "77cdf93cde05f15fcb710f320c37c155",
          "start_last_seq": 0,
          "start_time": "Thu, 10 Oct 2013 05:56:04 GMT"
        }
      ],
      "replication_id_version": 3,
      "session_id": "d5a34cbbdafa70e0db5cb57d02a6b955",
      "source_last_seq": 5
    }


The Replication Log SHOULD contain the next fields:

- **history** (*array* of *object*): Replication history. **Required**

  - **doc_write_failures** (*number*): Amount of failed writes
  - **docs_read** (*number*): Amount of read documents
  - **docs_written** (*number*): Amount of written documents
  - **end_last_seq** (*number*): Last processed Update Sequence ID
  - **end_time** (*string*): Replication completion datetime in :rfc:`2822`
    format
  - **missing_checked** (*number*): Amount of checked revisions on Source
  - **missing_found** (*number*): Amount of missing revisions found on Target
  - **recorded_seq** (*number*): Recorded intermediate Checkpoint. **Required**
  - **session_id** (*string*): Unique session ID. Commonly, a random UUID value
    is used. **Required**
  - **start_last_seq** (*number*): Start update Sequence ID
  - **start_time** (*string*): Replication start datetime in :rfc:`2822` format

- **replication_id_version** (*number*): Replication protocol version. Defines
  Replication ID calculation algorithm, HTTP API calls and the others
  routines. **Required**
- **session_id** (*string*): Unique ID of the last session. Shortcut to
  the ``session_id`` field of the latest ``history`` object. **Required**
- **source_last_seq** (*number*): Last processed Checkpoint. Shortcut to
  the ``recorded_seq`` field of the latest ``history`` object. **Required**


This requests also MAY fall with :statuscode:`404` response:

  **Request**:

  .. code-block:: http

    GET /source/_local/b6cef528f67aa1a8a014dd1144b10e09 HTTP/1.1
    Accept: application/json
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 404 Object Not Found
    Cache-Control: must-revalidate
    Content-Length: 41
    Content-Type: application/json
    Date: Tue, 08 Oct 2013 13:31:10 GMT
    Server: CouchDB (Erlang OTP)

    {
      "error": "not_found",
      "reason": "missing"
    }

That's OK. This means that there is no information about current Replication
and it seems that it wasn't ever been run and Replicator MUST run
Full Replication.


Compare Replication Logs
^^^^^^^^^^^^^^^^^^^^^^^^

In case of successful retrieval of Replication Logs both from Source and Target,
Replicator MUST determine their common ancestry by following the next algorithm:

- Compare ``session_id`` values for the chronological last session - if they
  matches, Source and Target has common Replication history and it seems
  to be valid. Use ``source_last_seq`` value for startup Checkpoint

- In case of mismatch, iterate over ``history`` collection to search the latest
  (chronologically) common ``session_id`` for Source and Target. Use value of
  ``recorded_seq`` field as startup Checkpoint

If Source and Target has no common ancestry, the Replicator MUST run
Full Replication.


Locate Changed Documents
------------------------

.. code-block:: text

  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  ' Find out Common Ancestry:                                                                   '
  '                                                                                             '
  '              +-----------------------------------+                                          '
  '              |     Compare Replication Logs      |                                          '
  '              +-----------------------------------+                                          '
  '                                                |                                            '
  '                                                |                                            '
  + - - - - - - - - - - - - - - - - - - - - - - -  |  - - - - - - - - - - - - - - - - - - - - - +
                                                   |
  + - - - - - - - - - - - - - - - - - - - - - - -  |  - - - - - - - - - - - - - - - - - - - - - +
  ' Locate Changed Documents:                      |                                            '
  '                                                |                                            '
  '                                                |                                            '
  '                                                v                                            '
  '             +------------------------------------+                                          '
  '   +-------> |        Listen Changes Feed         | ----------+                              '
  '   |         +------------------------------------+           |                              '
  '   |         |       GET  /source/_changes        |           |                              '
  '   |         |       POST /source/_changes        |           |                              '
  '   |         +------------------------------------+           |                              '
  '   |                                            |             |                              '
  '   |                                            |             |                              '
  '   |                      There are new changes |             | No more changes              '
  '   |                                            |             |                              '
  '   |                                            v             v                              '
  '   |         +------------------------------------+         +----------------------------+   '
  '   |         |       Read Batch of Changes        |         |    Replication Completed   |   '
  '   |         +------------------------------------+         +----------------------------+   '
  '   |                                            |                                            '
  '   | No                                         |                                            '
  '   |                                            v                                            '
  '   |         +------------------------------------+                                          '
  '   |         | Compare Docs Revisions with Target |                                          '
  '   |         +------------------------------------+                                          '
  '   |         |       POST /target/_revs_diff      |                                          '
  '   |         +------------------------------------+                                          '
  '   |                                            |                                            '
  '   |                                     200 OK |                                            '
  '   |                                            v                                            '
  '   |         +------------------------------------+                                          '
  '   +-------- |       Any Difference Found?        |                                          '
  '             +------------------------------------+                                          '
  '                                                |                                            '
  '                                            Yes |                                            '
  '                                                |                                            '
  + - - - - - - - - - - - - - - - - - - - - - - -  |  - - - - - - - - - - - - - - - - - - - - - +
                                                   |
  + - - - - - - - - - - - - - - - - - - - - - - -  |  - - - - - - - - - - - - - - - - - - - - - +
  ' Replicate Changes:                             |                                            '
  '                                                v                                            '
  '             +------------------------------------+                                          '
  '             |     Fetch Next Changed Document    |                                          '
  '             +------------------------------------+                                          '
  '                                                                                             '
  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +


Listen Changes Feed
^^^^^^^^^^^^^^^^^^^

When start up Checkpoint has been defined, Replicator SHOULD read
:ref:`Changes Feed <changes>` of Source by using :get:`/{db}/_changes` request.
This request to Changes Feed MUST be made with the next query parameters:

- ``feed`` parameter defined type of response from Changes Feed: for Continuous
  replication it MUST have value ``continuous``, otherwise it SHOULD have
  ``normal`` value.

- ``style=all_docs`` query parameter instructs Source that it MUST include
  all Revision leaves for each document's event in output.

- For Continuous Replication the ``heartbeat`` parameter defines heartbeat
  period in *milliseconds*. The RECOMMENDED value by default is ``10000``
  (10 seconds).

- If startup Checkpoint was found during Replication Logs comparison,
  the ``since`` query parameter MUST be passed with this value.
  In case of Full Replication it MAY be equaled ``0`` (number zero) or
  be omitted.

- Additionally, ``filter`` parameter MAY be specified in case of using
  :ref:`filter function <changes/filter>` on server side.


Read Batch of Changes
^^^^^^^^^^^^^^^^^^^^^

Reading whole feed with single shot may be not resource optimal solution and
it is RECOMMENDED to process the feed by small chunks. However, there is
no specific recommendation on chunks size since it heavily depended from
available resources: large chunks requires more memory while they are reduces
I/O operations and vice versa.

Note, that Changes Feed output format is different for request with
:ref:`feed=normal <changes/normal>` and with
:ref:`feed=continuous <changes/continuous>` query parameter.

Normal Feed:

  **Request**:

  .. code-block:: http

    GET /source/_changes?feed=normal&style=all_docs&heartbeat=10000 HTTP/1.1
    Accept: application/json
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Fri, 09 May 2014 16:20:41 GMT
    Server: CouchDB (Erlang OTP)
    Transfer-Encoding: chunked

    {"results":[
    {"seq":14,"id":"f957f41e7d517efd0e9902a79d5da540","changes":[{"rev":"3-46a39451b3cb2482d1d75c5315e7af73"}],"deleted":true}
    {"seq":29,"id":"ddf339ddf5fc74e274bfe78e1dccbd0c","changes":[{"rev":"10-304cae84fd862832ea9814f02920d4b2"}]}
    {"seq":37,"id":"d3cc62f5cfd7a6b579f356bfdb8acc2d","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    {"seq":39,"id":"f13bd08bb40ecefd9a246c884e5791c0","changes":[{"rev":"1-b35ddc60b362eddfb3a736f8075e6f4b"}]}
    {"seq":41,"id":"e0a998677655f7cbeafbf66587b30d18","changes":[{"rev":"2-c1c6c44c4bc3c9344b037c8690468605"}]}
    {"seq":42,"id":"a75bdfc59749f6e99551ae25786fd4df","changes":[{"rev":"1-967a00dff5e02add41819138abb3284d"}]}
    {"seq":43,"id":"a5f467a03a436f790c788b76a51dac57","changes":[{"rev":"1-5575e26acdeb1df561bb5b70b26ba151"}]}
    {"seq":45,"id":"470c30046193b2ba2a05b9ab9050c8fa","changes":[{"rev":"11-c292a412e4e7bafb71030be373037ea2"}]}
    {"seq":46,"id":"b1cb850826be324587dbdee51d1076cf","changes":[{"rev":"10-ABC"}]}
    {"seq":47,"id":"49ec0489e24f27e9fe29c83f687b8366","changes":[{"rev":"157-b01fd3ec501b4f205f916d6318e0154f"},{"rev":"123-6f7c1b7c97a9e4f0d22bdf130e8fd817"}]}
    {"seq":49,"id":"dad103794f2b77952d3adedad021ab2b","changes":[{"rev":"1-93464cc455ed8cb5bbaffaf9c1108459"},{"rev":"6-5b8a52c22580e922e792047cff3618f3"}]}
    {"seq":50,"id":"734648778ee90cab40a33d346c3eb441","changes":[{"rev":"1-9f08f9cb34395f44dba8f5ba4658f6f8"}]}
    {"seq":51,"id":"7ae193028f68cde9a15f207afa842f11","changes":[{"rev":"1-57bfa8896af4bfbdacc11a26dc2bd140"}]}
    {"seq":63,"id":"6a7a6c86ada4bbc13308a83db0931b53","changes":[{"rev":"5-acf6e96ac71e54dd3fd8fe6087acb83d"}],"deleted":true}
    {"seq":64,"id":"dfb9850aced9d56bbd56141111207509","changes":[{"rev":"1-102f3569e833fa883cd5e72e6c64078b"}]}
    {"seq":65,"id":"c532afa7a005f53bbac724d15df809ca","changes":[{"rev":"1-64918e35f4f5c1a57485d55cc2f23a86"}]}
    {"seq":66,"id":"af8a95089b914774baac91053f5ab676","changes":[{"rev":"1-3db2d29c3c3ff59561620b7901c1c901"}]}
    {"seq":67,"id":"caa3ddedf0134feb17d47e141c27849c","changes":[{"rev":"1-64918e35f4f5c1a57485d55cc2f23a86"}]}
    {"seq":68,"id":"79f3b4e9a83eb6caac5f26b82895c7bc","changes":[{"rev":"1-102f3569e833fa883cd5e72e6c64078b"}]}
    {"seq":69,"id":"1d89d16fa7523f80468ad4915991d04d","changes":[{"rev":"1-3db2d29c3c3ff59561620b7901c1c901"}]}
    {"seq":71,"id":"abae73489fd04deee0034ff5750005d0","changes":[{"rev":"2-7051cbe5c8faecd085a3fa619e6e6337"}]}
    {"seq":77,"id":"6c25534f0733a65b6b3ebc3be2b33f57","changes":[{"rev":"9-CDE"},{"rev":"3-00e7ae9eeb50cabd81126e9f39b871cf"},{"rev":"1-ABC"}]}
    {"seq":78,"id":"SpaghettiWithMeatballs","changes":[{"rev":"22-5f950945d8c4671cb28e4e3a0db0e2c5"}]}
    ],
    "last_seq":78}

Continuous Feed:

  **Request**:

  .. code-block:: http

    GET /source/_changes?feed=continuous&style=all_docs&heartbeat=10000 HTTP/1.1
    Accept: application/json
    Host: localhost:5984
    User-Agent: CouchDB

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Fri, 09 May 2014 16:22:22 GMT
    Server: CouchDB (Erlang OTP)
    Transfer-Encoding: chunked

    {"seq":14,"id":"f957f41e7d517efd0e9902a79d5da540","changes":[{"rev":"3-46a39451b3cb2482d1d75c5315e7af73"}],"deleted":true}
    {"seq":29,"id":"ddf339ddf5fc74e274bfe78e1dccbd0c","changes":[{"rev":"10-304cae84fd862832ea9814f02920d4b2"}]}
    {"seq":37,"id":"d3cc62f5cfd7a6b579f356bfdb8acc2d","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    {"seq":39,"id":"f13bd08bb40ecefd9a246c884e5791c0","changes":[{"rev":"1-b35ddc60b362eddfb3a736f8075e6f4b"}]}
    {"seq":41,"id":"e0a998677655f7cbeafbf66587b30d18","changes":[{"rev":"2-c1c6c44c4bc3c9344b037c8690468605"}]}
    {"seq":42,"id":"a75bdfc59749f6e99551ae25786fd4df","changes":[{"rev":"1-967a00dff5e02add41819138abb3284d"}]}
    {"seq":43,"id":"a5f467a03a436f790c788b76a51dac57","changes":[{"rev":"1-5575e26acdeb1df561bb5b70b26ba151"}]}
    {"seq":45,"id":"470c30046193b2ba2a05b9ab9050c8fa","changes":[{"rev":"11-c292a412e4e7bafb71030be373037ea2"}]}
    {"seq":46,"id":"b1cb850826be324587dbdee51d1076cf","changes":[{"rev":"10-ABC"}]}
    {"seq":47,"id":"49ec0489e24f27e9fe29c83f687b8366","changes":[{"rev":"157-b01fd3ec501b4f205f916d6318e0154f"},{"rev":"123-6f7c1b7c97a9e4f0d22bdf130e8fd817"}]}
    {"seq":49,"id":"dad103794f2b77952d3adedad021ab2b","changes":[{"rev":"1-93464cc455ed8cb5bbaffaf9c1108459"},{"rev":"6-5b8a52c22580e922e792047cff3618f3"}]}
    {"seq":50,"id":"734648778ee90cab40a33d346c3eb441","changes":[{"rev":"1-9f08f9cb34395f44dba8f5ba4658f6f8"}]}
    {"seq":51,"id":"7ae193028f68cde9a15f207afa842f11","changes":[{"rev":"1-57bfa8896af4bfbdacc11a26dc2bd140"}]}
    {"seq":63,"id":"6a7a6c86ada4bbc13308a83db0931b53","changes":[{"rev":"5-acf6e96ac71e54dd3fd8fe6087acb83d"}],"deleted":true}
    {"seq":64,"id":"dfb9850aced9d56bbd56141111207509","changes":[{"rev":"1-102f3569e833fa883cd5e72e6c64078b"}]}
    {"seq":65,"id":"c532afa7a005f53bbac724d15df809ca","changes":[{"rev":"1-64918e35f4f5c1a57485d55cc2f23a86"}]}
    {"seq":66,"id":"af8a95089b914774baac91053f5ab676","changes":[{"rev":"1-3db2d29c3c3ff59561620b7901c1c901"}]}
    {"seq":67,"id":"caa3ddedf0134feb17d47e141c27849c","changes":[{"rev":"1-64918e35f4f5c1a57485d55cc2f23a86"}]}
    {"seq":68,"id":"79f3b4e9a83eb6caac5f26b82895c7bc","changes":[{"rev":"1-102f3569e833fa883cd5e72e6c64078b"}]}
    {"seq":69,"id":"1d89d16fa7523f80468ad4915991d04d","changes":[{"rev":"1-3db2d29c3c3ff59561620b7901c1c901"}]}
    {"seq":71,"id":"abae73489fd04deee0034ff5750005d0","changes":[{"rev":"2-7051cbe5c8faecd085a3fa619e6e6337"}]}
    {"seq":75,"id":"SpaghettiWithMeatballs","changes":[{"rev":"21-5949cfcd437e3ee22d2d98a26d1a83bf"}]}
    {"seq":77,"id":"6c25534f0733a65b6b3ebc3be2b33f57","changes":[{"rev":"9-CDE"},{"rev":"3-00e7ae9eeb50cabd81126e9f39b871cf"},{"rev":"1-ABC"}]}
    {"seq":78,"id":"SpaghettiWithMeatballs","changes":[{"rev":"22-5f950945d8c4671cb28e4e3a0db0e2c5"}]}

For both Changes Feed formats record-per-line style is preserved to simplify
iterative fetching and decoding JSON objects with less memory footprint.


Calculate Revision Difference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

After reading batch of changes from Changes Feed, Replicator forms special
JSON mapping object for Document ID and related leaf Revisions and sends
the result to Target via :post:`/{db}/_revs_diff` request:

  **Request**:

  .. code-block:: http

    POST /target/_revs_diff HTTP/1.1
    Accept: application/json
    Content-Length: 287
    Content-Type: application/json
    Host: localhost:5984
    User-Agent: CouchDB

    {
      "baz": [
        "2-7051cbe5c8faecd085a3fa619e6e6337"
      ],
      "foo": [
        "3-6a540f3d701ac518d3b9733d673c5484"
      ],
      "bar": [
        "1-d4e501ab47de6b2000fc8a02f84a0c77",
        "1-967a00dff5e02add41819138abb3284d"
      ]
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 88
    Content-Type: application/json
    Date: Fri, 25 Oct 2013 14:44:41 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "baz": {
        "missing": [
          "2-7051cbe5c8faecd085a3fa619e6e6337"
        ]
      },
      "bar": {
        "missing": [
          "1-d4e501ab47de6b2000fc8a02f84a0c77"
        ]
      }
    }

In the response Replicator receives Document ID -- Revisions mapping as well,
but for Revisions that are not exists in Target and REQUIRED to be transferred
from Source.

If all Revisions was found for specified Documents the response will contains
empty JSON object:

  **Request**

  .. code-block:: http

    POST /target/_revs_diff HTTP/1.1
    Accept: application/json
    Content-Length: 160
    Content-Type: application/json
    Host: localhost:5984
    User-Agent: CouchDB

    {
      "foo": [
        "3-6a540f3d701ac518d3b9733d673c5484"
      ],
      "bar": [
        "1-967a00dff5e02add41819138abb3284d"
      ]
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 2
    Content-Type: application/json
    Date: Fri, 25 Oct 2013 14:45:00 GMT
    Server: CouchDB (Erlang/OTP)

    {}


Replication Completed
^^^^^^^^^^^^^^^^^^^^^

When no more changes left to process and no more Documents left to replicate,
Replicator finishes the Replication process. If Replication wasn't Continuous,
Replicator MAY return response to client with some statistic about the process.

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 414
    Content-Type: application/json
    Date: Fri, 09 May 2014 15:14:19 GMT
    Server: CouchDB (Erlang OTP)

    {
      "history": [
        {
          "doc_write_failures": 2,
          "docs_read": 2,
          "docs_written": 0,
          "end_last_seq": 2939,
          "end_time": "Fri, 09 May 2014 15:14:19 GMT",
          "missing_checked": 1835,
          "missing_found": 2,
          "recorded_seq": 2939,
          "session_id": "05918159f64842f1fe73e9e2157b2112",
          "start_last_seq": 0,
          "start_time": "Fri, 09 May 2014 15:14:18 GMT"
        }
      ],
      "ok": true,
      "replication_id_version": 3,
      "session_id": "05918159f64842f1fe73e9e2157b2112",
      "source_last_seq": 2939
    }


Replicate Changes
-----------------

.. code-block:: text

  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  ' Locate Changed Documents:                                                                     '
  '                                                                                               '
  '                     +-------------------------------------+                                   '
  '                     |      Any Difference was Found?      |                                   '
  '                     +-------------------------------------+                                   '
  '                       |                                                                       '
  '                       | Yes                                                                   '
  '                       |                                                                       '
  + - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
                          |
  + - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  ' Replicate Changes:    |                                                                       '
  '                       v                                                                       '
  '                     +-------------------------------------+                                   '
  '   +---------------> |     Fetch Next Changed Document     | <----+                            '
  '   |                 +-------------------------------------+      |                            '
  '   |                 |          GET /source/docid          |      |                            '
  '   |                 +-------------------------------------+      |                            '
  '   |                   |                                          |                            '
  '   |                   |                                          |                            '
  '   |                   |                                          | 201 Created                '
  '   |                   | 200 OK                                   | 401 Unauthorized           '
  '   |                   |                                          | 403 Forbidden              '
  '   |                   |                                          |                            '
  '   |                   v                                          |                            '
  '   |                 +-------------------------------------+    +---------------------------+  '
  '   |      +--------- |  Document Has Changed Attachments?  |    | Update Document on Target |  '
  '   |      |          +-------------------------------------+    +---------------------------+  '
  '   |      |            |                                        |     PUT /target/docid     |  '
  '   |      |            |                                        +---------------------------+  '
  '   |      |            | Yes                                      ^                            '
  '   |      |            |                                          |                            '
  '   |      |            v                                          | Yes                        '
  '   |      |          +-------------------------------------+      |                            '
  '   |      | No       |        Are They Big Enough?         | -----+                            '
  '   |      |          +-------------------------------------+                                   '
  '   |      |            |                                                                       '
  '   |      |            | No                                                                    '
  '   |      |            v                                                                       '
  '   |      |          +-------------------------------------+                                   '
  '   |      +--------> |     Put Document Into the Stack     |                                   '
  '   |                 +-------------------------------------+                                   '
  '   |                   |                                                                       '
  '   |                   |                                                                       '
  '   |                   v                                                                       '
  '   |        No       +-------------------------------------+                                   '
  '   +---------------- |           Stack is Full?            |                                   '
  '   |                 +-------------------------------------+                                   '
  '   |                   |                                                                       '
  '   |                   | Yes                                                                   '
  '   |                   |                                                                       '
  '   |                   v                                                                       '
  '   |                 +-------------------------------------+                                   '
  '   |                 | Upload Stack of Documents to Target |                                   '
  '   |                 +-------------------------------------+                                   '
  '   |                 |       POST /target/_bulk_docs       |                                   '
  '   |                 +-------------------------------------+                                   '
  '   |                   |                                                                       '
  '   |                   | 201 Created                                                           '
  '   |                   v                                                                       '
  '   |                 +-------------------------------------+                                   '
  '   |                 |          Ensure in Commit           |                                   '
  '   |                 +-------------------------------------+                                   '
  '   |                 |  POST /target/_ensure_full_commit   |                                   '
  '   |                 +-------------------------------------+                                   '
  '   |                   |                                                                       '
  '   |                   | 201 Created                                                           '
  '   |                   v                                                                       '
  '   |                 +-------------------------------------+                                   '
  '   |                 |    Record Replication Checkpoint    |                                   '
  '   |                 +-------------------------------------+                                   '
  '   |                 |  PUT /source/_local/replication-id  |                                   '
  '   |                 |  PUT /target/_local/replication-id  |                                   '
  '   |                 +-------------------------------------+                                   '
  '   |                   |                                                                       '
  '   |                   | 201 Created                                                           '
  '   |                   v                                                                       '
  '   |        No       +-------------------------------------+                                   '
  '   +---------------- | All Documents from Batch Processed? |                                   '
  '                     +-------------------------------------+                                   '
  '                                                         |                                     '
  '                                                     Yes |                                     '
  '                                                         |                                     '
  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - +
                                                            |
  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - +
  ' Locate Changed Documents:                               |                                     '
  '                                                         v                                     '
  '                     +-------------------------------------+                                   '
  '                     |         Listen Changes Feed         |                                   '
  '                     +-------------------------------------+                                   '
  '                                                                                               '
  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +


Fetch Changed Documents
^^^^^^^^^^^^^^^^^^^^^^^

.. versionchanged:: 1.5 added ``latest=true`` query parameter

At this step Replicator MUST fetch all Document Leaf Revisions from Source
that are missed at Target. This operation is effective if Replication WILL
use previously calculated Revisions difference since there are defined all
missed Documents and their Revisions.

To fetch the Document Replicator made :get:`/{db}/{docid}` request with
the next query parameters:

- ``revs=true``: Instructs the Source to include list of all known revisions
  into the Document at ``_revisions`` field. This information is needed to
  synchronize Document's ancestors history between Source and Target

- The ``open_revs`` query parameter contains value as JSON array with list of
  Leaf Revisions that are need to be fetched. If specified Revision exists,
  Document MUST be returned for this Revision. Otherwise, Source MUST return
  object with single field ``missing`` with missed Revision as value. In case
  when Document contains attachments Source MUST return information only for
  those ones that had been changed (added or updated) since specified Revision
  values. If attachment was deleted, Document MUST NOT have stub information
  for him

- ``latest=true``: Ensures, that Source will return latest Document Revision
  regardless which one was specified in ``open_revs`` query parameter.
  This parameter solves race condition problem when requested Document may be
  changed in between this step and handling related event on Changes Feed

In the response Source SHOULD return :mimetype:`multipart/mixed` or response
instead of :mimetype:`application/json` unless :header:`Accept` isn't instructs
to return such response. The :mimetype:`multipart/mixed` content type allows
to handle the response data as stream, since there could be multiple documents
(one per each Leaf Revision) plus several attachments data. These attachments
are mostly binary and JSON has no way to handle such data except as base64
encoded string what is very ineffective for transfer and processing operations.

With :mimetype:`multipart/mixed` response Replicator handles multiple Document
Leaf Revisions and their attachments one by one as raw data without any
additional encoding applied. There is also one agreement to make data processing
more effective: Document ALWAYS goes before his attachments, so Replicator has
no need to process all data to map related Documents-Attachments and may handle
it as stream with lesser memory footprint.


  **Request**:

  .. code-block:: http

    GET /source/SpaghettiWithMeatballs?revs=true&open_revs=[%225-00ecbbc%22,%221-917fa23%22,%223-6bcedf1%22]&latest=true HTTP/1.1
    Accept: multipart/mixed
    Host: localhost:5984
    User-Agent: CouchDB


  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Content-Type: multipart/mixed; boundary="7b1596fc4940bc1be725ad67f11ec1c4"
    Date: Thu, 07 Nov 2013 15:10:16 GMT
    Server: CouchDB (Erlang OTP)
    Transfer-Encoding: chunked

    --7b1596fc4940bc1be725ad67f11ec1c4
    Content-Type: application/json

    {
      "_id": "SpaghettiWithMeatballs",
      "_rev": "1-917fa23",
      "_revisions": {
        "ids": [
          "917fa23"
        ],
        "start": 1
      },
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
        "spaghetti",
        "tomato sauce",
        "meatballs"
      ],
      "name": "Spaghetti with meatballs"
    }
    --7b1596fc4940bc1be725ad67f11ec1c4
    Content-Type: multipart/related; boundary="a81a77b0ca68389dda3243a43ca946f2"

    --a81a77b0ca68389dda3243a43ca946f2
    Content-Type: application/json

    {
      "_attachments": {
        "recipe.txt": {
          "content_type": "text/plain",
          "digest": "md5-R5CrCb6fX10Y46AqtNn0oQ==",
          "follows": true,
          "length": 87,
          "revpos": 7
        }
      },
      "_id": "SpaghettiWithMeatballs",
      "_rev": "7-474f12e",
      "_revisions": {
        "ids": [
          "474f12e",
          "5949cfc",
          "00ecbbc",
          "fc997b6",
          "3552c87",
          "404838b",
          "5defd9d",
          "dc1e4be"
        ],
        "start": 7
      },
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
        "spaghetti",
        "tomato sauce",
        "meatballs",
        "love"
      ],
      "name": "Spaghetti with meatballs"
    }
    --a81a77b0ca68389dda3243a43ca946f2
    Content-Disposition: attachment; filename="recipe.txt"
    Content-Type: text/plain
    Content-Length: 87

    1. Cook spaghetti
    2. Cook meetballs
    3. Mix them
    4. Add tomato sauce
    5. ...
    6. PROFIT!


    --a81a77b0ca68389dda3243a43ca946f2--
    --7b1596fc4940bc1be725ad67f11ec1c4
    Content-Type: application/json; error="true"

    {"missing":"3-6bcedf1"}
    --7b1596fc4940bc1be725ad67f11ec1c4--


After receiving the response, Replicator puts all received data into local stack
for further bulk upload to utilize network bandwidth effectively. The local
stack size could be limited by Documents amount or bytes of handled JSON data.
When stack going to be full, Replicator uploads all handled Document in bulk
mode to Target. While bulk operations are highly RECOMMENDED to be used,
in certain cases  Replicator MAY upload Documents to Target one by one.
See below for explanations.

.. note::

   Alternative Replicator implementations MAY use alternative ways to retrieve
   Documents from Source. For instance, `PouchDB`_ doesn't uses Multipart API
   and fetches only latest Document Revision with inline attachments as single
   JSON object. While this is still valid CouchDB HTTP API usage, such solutions
   MAY require to have different API implementation for non-CouchDB Peers.

.. _PouchDB: https://github.com/daleharvey/pouchdb/blob/master/src/pouch.replicate.js


Upload Batch of Changed Documents
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To upload multiple Documents with single shot, Replicator send
:post:`/{db}/_bulk_docs` request to Target with payload as JSON object contained
next mandatory fields:

- **docs** (*array* of *objects*): List of Document objects to update on Target.
  These Documents MUST contains ``_revisions`` field that holds list of his full
  Revision history to let Target create Leaf Revision that correctly preserve
  his ancestry
- **new_edits** (*boolean*): Special flag that instructs Target to store
  Documents with specified Revision (field ``_rev``) value as-is without
  generating new one. Always ``false``

The request also MAY contains :header:`X-Couch-Full-Commit` that controls
CouchDB :config:option:`commit policy <couchdb/delayed_commits>`. Other Peers
implementations MAY ignore this header or use it to control similar local
feature.

  **Request**:

  .. code-block:: http

    POST /target/_bulk_docs HTTP/1.1
    Accept: application/json
    Content-Length: 826
    Content-Type:application/json
    Host: localhost:5984
    User-Agent: CouchDB
    X-Couch-Full-Commit: false

    {
      "docs": [
        {
          "_id": "SpaghettiWithMeatballs",
          "_rev": "1-917fa2381192822767f010b95b45325b",
          "_revisions": {
            "ids": [
              "917fa2381192822767f010b95b45325b"
            ],
            "start": 1
          },
          "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
          "ingredients": [
            "spaghetti",
            "tomato sauce",
            "meatballs"
          ],
          "name": "Spaghetti with meatballs"
        },
        {
          "_id": "LambStew",
          "_rev": "1-34c318924a8f327223eed702ddfdc66d",
          "_revisions": {
            "ids": [
              "34c318924a8f327223eed702ddfdc66d"
            ],
            "start": 1
          },
          "servings": 6,
          "subtitle": "Delicious with scone topping",
          "title": "Lamb Stew"
        },
        {
          "_id": "FishStew",
          "_rev": "1-9c65296036141e575d32ba9c034dd3ee",
          "_revisions": {
            "ids": [
              "9c65296036141e575d32ba9c034dd3ee"
            ],
            "start": 1
          },
          "servings": 4,
          "subtitle": "Delicious with fresh bread",
          "title": "Fish Stew"
        }
      ],
      "new_edits": false
    }

In response Target MUST return JSON array with list of Document update status.
If Document have been stored successfully, the list item MUST contains field
``ok`` with ``true`` value. Otherwise it MUST contains ``error`` and ``reason``
fields with error type and human-friendly reason description.

Document updating failure isn't fatal fatal situation since Target MAY reject
it by some reasons. It's RECOMMENDED to use error type ``forbidden`` for
rejections, but some other errors might take in place (like invalid field name
etc.). Replicator SHOULD NOT retry to upload rejected documents unless he has
serious reasons for that (e.g. there is special error type for that).

Note that while updating failed for one Document in the response below,
Target still returned :statuscode:`201` response. Same will be true if all
updating will fall for all uploaded Documents.

  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 246
    Content-Type: application/json
    Date: Sun, 10 Nov 2013 19:02:26 GMT
    Server: CouchDB (Erlang/OTP)

    [
      {
        "ok": true,
        "id": "SpaghettiWithMeatballs",
        "rev":" 1-917fa2381192822767f010b95b45325b"
      },
      {
        "ok": true,
        "id": "FishStew",
        "rev": "1-9c65296036141e575d32ba9c034dd3ee"
      },
      {
        "error": "forbidden",
        "id": "LambStew",
        "reason": "sorry",
        "rev": "1-34c318924a8f327223eed702ddfdc66d"
      }
    ]


Upload Document with Attachments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There is a special optimization case when Replicator WILL NOT use bulk upload
of changed Documents. This case is applied when Documents contains a lot of
attached files or they are too big to been effectively encoded with Base64.

.. note::

  CouchDB defines limit of ``8`` attachments per Document and each attached file
  size should not be greater than ``64 KiB``. While this is RECOMMENDED
  limitations, other Replicator implementations MAY have their own values.

For this case Replicator makes :put:`/{db}/{docid}?new_edits=false
</{db}/{docid}>` request with :mimetype:`multipart/related` content type. Such
request allows easily stream Document and all his attachments one by one without
any serialization overhead.

  **Request**:

  .. code-block:: http

    PUT /target/SpaghettiWithMeatballs?new_edits=false HTTP/1.1
    Accept: application/json
    Content-Length: 1030
    Content-Type: multipart/related; boundary="864d690aeb91f25d469dec6851fb57f2"
    Host: localhost:5984
    User-Agent: CouchDB

    --2fa48cba80d0cdba7829931fe8acce9d
    Content-Type: application/json

    {
      "_attachments": {
        "recipe.txt": {
          "content_type": "text/plain",
          "digest": "md5-R5CrCb6fX10Y46AqtNn0oQ==",
          "follows": true,
          "length": 87,
          "revpos": 7
        }
      },
      "_id": "SpaghettiWithMeatballs",
      "_rev": "7-474f12eb068c717243487a9505f6123b",
      "_revisions": {
        "ids": [
          "474f12eb068c717243487a9505f6123b",
          "5949cfcd437e3ee22d2d98a26d1a83bf",
          "00ecbbc54e2a171156ec345b77dfdf59",
          "fc997b62794a6268f2636a4a176efcd6",
          "3552c87351aadc1e4bea2461a1e8113a",
          "404838bc2862ce76c6ebed046f9eb542",
          "5defd9d813628cea6e98196eb0ee8594"
        ],
        "start": 7
      },
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
        "spaghetti",
        "tomato sauce",
        "meatballs",
        "love"
      ],
      "name": "Spaghetti with meatballs"
    }
    --2fa48cba80d0cdba7829931fe8acce9d
    Content-Disposition: attachment; filename="recipe.txt"
    Content-Type: text/plain
    Content-Length: 87

    1. Cook spaghetti
    2. Cook meetballs
    3. Mix them
    4. Add tomato sauce
    5. ...
    6. PROFIT!


    --2fa48cba80d0cdba7829931fe8acce9d--


  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 105
    Content-Type: application/json
    Date: Fri, 08 Nov 2013 16:35:27 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "ok": true,
      "id": "SpaghettiWithMeatballs",
      "rev": "7-474f12eb068c717243487a9505f6123b"
    }


Unlike bulk updating via :post:`/{db}/_bulk_docs` endpoint, the response MAY
come with different status code. For instance, in case when Document is rejected
Target SHOULD response with :statuscode:`403`:

  **Response**:

  .. code-block:: http

    HTTP/1.1 403 Forbidden
    Cache-Control: must-revalidate
    Content-Length: 39
    Content-Type: application/json
    Date: Fri, 08 Nov 2013 16:35:27 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "error": "forbidden",
      "reason": "sorry"
    }

Replicator SHOULD NOT retry requests in case of :statuscode:`401`,
:statuscode:`403`, :statuscode:`409` and :statuscode:`412` since repeating
couldn't solve the issue with user credentials or uploaded data.


Ensure In Commit
^^^^^^^^^^^^^^^^

Once batch of changes was successfully uploaded to Target, Replicator makes
:post:`/{db}/_ensure_full_commit` request to ensure that every transferred bit
is lay down on disk or other *persistent* storage place. Target MUST return
:statuscode:`201` response with JSON object contained next mandatory fields:

- **instance_start_time** (*string*): Timestamp of when the database was
  opened, expressed in *microseconds* since the epoch
- **ok** (*boolean*): Operation status. Constantly ``true``

  **Request**:

  .. code-block:: http

    POST /target/_ensure_full_commit HTTP/1.1
    Accept: application/json
    Content-Type: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 53
    Content-Type: application/json
    Date: Web, 06 Nov 2013 18:20:43 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "instance_start_time": "1381218659871282",
      "ok": true
    }


Record Replication Checkpoint
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Since batch of changes was uploaded and committed successfully, Replicator
updates Replication Log both on Source and Target recording current Replication
state. This operation is REQUIRED to let in case of Replication failure resume
it from last point of success, not from very begin.

Replicator updates Replication Log on Source:

  **Request**:

  .. code-block:: http

    PUT /source/_local/afa899a9e59589c3d4ce5668e3218aef HTTP/1.1
    Accept: application/json
    Content-Length: 591
    Content-Type: application/json
    Host: localhost:5984
    User-Agent: CouchDB

    {
      "_id": "_local/afa899a9e59589c3d4ce5668e3218aef",
      "_rev": "0-1",
      "_revisions": {
        "ids": [
          "31f36e40158e717fbe9842e227b389df"
        ],
        "start": 1
      },
      "history": [
        {
          "doc_write_failures": 0,
          "docs_read": 6,
          "docs_written": 6,
          "end_last_seq": 26,
          "end_time": "Thu, 07 Nov 2013 09:42:17 GMT",
          "missing_checked": 6,
          "missing_found": 6,
          "recorded_seq": 26,
          "session_id": "04bf15bf1d9fa8ac1abc67d0c3e04f07",
          "start_last_seq": 0,
          "start_time": "Thu, 07 Nov 2013 09:41:43 GMT"
        }
      ],
      "replication_id_version": 3,
      "session_id": "04bf15bf1d9fa8ac1abc67d0c3e04f07",
      "source_last_seq": 26
    }


  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 75
    Content-Type: application/json
    Date: Thu, 07 Nov 2013 09:42:17 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "id": "_local/afa899a9e59589c3d4ce5668e3218aef",
      "ok": true,
      "rev": "0-2"
    }

...and on Target too:

  **Request**:

  .. code-block:: http

    PUT /target/_local/afa899a9e59589c3d4ce5668e3218aef HTTP/1.1
    Accept: application/json
    Content-Length: 591
    Content-Type: application/json
    Host: localhost:5984
    User-Agent: CouchDB

    {
      "_id": "_local/afa899a9e59589c3d4ce5668e3218aef",
      "_rev": "1-31f36e40158e717fbe9842e227b389df",
      "_revisions": {
        "ids": [
          "31f36e40158e717fbe9842e227b389df"
        ],
        "start": 1
      },
      "history": [
        {
          "doc_write_failures": 0,
          "docs_read": 6,
          "docs_written": 6,
          "end_last_seq": 26,
          "end_time": "Thu, 07 Nov 2013 09:42:17 GMT",
          "missing_checked": 6,
          "missing_found": 6,
          "recorded_seq": 26,
          "session_id": "04bf15bf1d9fa8ac1abc67d0c3e04f07",
          "start_last_seq": 0,
          "start_time": "Thu, 07 Nov 2013 09:41:43 GMT"
        }
      ],
      "replication_id_version": 3,
      "session_id": "04bf15bf1d9fa8ac1abc67d0c3e04f07",
      "source_last_seq": 26
    }


  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 106
    Content-Type: application/json
    Date: Thu, 07 Nov 2013 09:42:17 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "id": "_local/afa899a9e59589c3d4ce5668e3218aef",
      "ok": true,
      "rev": "2-9b5d1e36bed6ae08611466e30af1259a"
    }


Continue Read the Changes
-------------------------

Once batch of changes had been processed and transferred to Target successfully,
Replicator continue listen Changes Feed for new changes. In there is no new
changes to process the Replication considered to be done.

For Continuous Replication Replicator MUST continue await for new changes from
Source side.


Protocol Robustness
===================

Since `CouchDB Replication Protocol` works on top of HTTP, which is based on
TCP/IP itself, Replicator SHOULD expect to be working within unstable
environment with delays, losses and other bad surprises that might eventually
occurs. Replicator SHOULD NOT count every HTTP request failure as *fatal error*.
It SHOULD be smart enough to detect timeouts, repeat fallen requests, be ready
to process incomplete or malformed data and so on. *Data must flow* - that's
the rule.


Error Responses
===============

In case when something goes wrong, Peer MUST response with JSON object with
the next REQUIRED fields:

- **error** (*string*): Error type for programs and developers
- **reason** (*string*): Error description for humans


Bad Request
-----------

If request contains malformed data (like invalid JSON) the Peer MUST response
with HTTP :statuscode:`400` and ``bad_request`` as error type:

.. code-block:: javascript

  {
    "error": "bad_request",
    "reason": "invalid json"
  }

Forbidden
---------

If Peer REQUIRES for providing user's credentials and the request miss them,
the Peer MUST response with HTTP :statuscode:`401` and ``forbidden`` as error
type:

.. code-block:: javascript

  {
    "error": "forbidden",
    "reason": "please authorize"
  }

Unauthorized
------------

If Peer receives invalid user's credentials it MUST response with
HTTP :statuscode:`403` and ``unauthorized`` as error type:

.. code-block:: javascript

  {
    "error": "unauthorized",
    "reason": "invalid name or password"
  }


Resource Not Found
------------------

If requested resource, Database or Document wasn't found on Peer, it MUST
response with HTTP :statuscode:`404` and ``not_found`` as error type:

.. code-block:: javascript

  {
    "error": "not_found",
    "reason": "database \"target\" does not exists"
  }


Method Not Allowed
------------------

If requested Database or Document wasn't found on Peer, it MUST response with
HTTP :statuscode:`405` and ``method_not_allowed`` as error type:

.. code-block:: javascript

  {
    "error": "method_not_allowed",
    "reason": "Only GET, PUT, DELETE allowed"
  }


Resource Conflict
-----------------

Resource conflict error raises for concurrent updates of the same resource by
multiple clients. In this case Peer MUST response with HTTP :statuscode:`409`
and ``conflict`` as error type:

.. code-block:: javascript

  {
    "error": "conflict",
    "reason": "document update conflict"
  }


Precondition Failed
-------------------

The HTTP :statuscode:`412` response may be sent in case on attempt to run
request of Database creation (error type ``db_exists``) while it already exists
or some attachments information missed (error type ``missing_stub``.
There is no explicit error type restrictions, but it RECOMMEND to use error
types that are previously mentioned:

.. code-block:: javascript

  {
    "error": "db_exists",
    "reason": "database \"target\" exists"
  }


Server Error
------------

Raised in case when error is *fatal* and Replicator cannot do anything to
continue Replication. In this case Replicator MUST return HTTP :statuscode:`500`
response with error description (no restrictions on error type applied):

.. code-block:: javascript

  {
    "error": "worker_died",
    "reason": "kaboom!"
  }


Optimisations
=============

There are next RECOMMENDED solutions to optimize Replication process:

- Keep amount of HTTP requests at reasonable minimum

- Try to work with connection pool and make parallel/multiple requests whatever
  it's possible

- Don't close socket after each request: respect keep-alive option

- Use continuous sessions (cookies, etc.) to reduce authentication overhead

- Try to use bulk requests for every operations with Documents

- Find out optimal batch size for Changes feed processing

- Preserve Replication Logs and resume Replication from the last Checkpoint
  whatever it's possible

- Optimize filter functions: let them run faster as possible

- Get ready for surprises: network is very unstable environment


API Reference
=============

Common Methods
--------------

- :head:`/{db}` -- Check Database existence
- :get:`/{db}` -- Retrieve Database information
- :get:`/{db}/_local/{docid}` -- Read the last Checkpoint
- :put:`/{db}/_local/{docid}` -- Save a new Checkpoint


For Target
----------

- :put:`/{db}` -- Create Target if it not exists and option was provided
- :post:`/{db}/_revs_diff` -- Locate Revisions that are not known to Target
- :post:`/{db}/_bulk_docs` -- Upload Revisions to Target
- :put:`/{db}/{docid}` -- Upload a single Document with attachments to Target
- :post:`/{db}/_ensure_full_commit` -- Ensure that all changes are stored
  on disk

For Source
----------

- :get:`/{db}/_changes` -- Fetch changes since the last pull of Source
- :post:`/{db}/_changes` -- Fetch changes for specified Document IDs since
  the last pull of Source
- :get:`/{db}/{docid}` -- Retrieve a single Document from Source
  with attachments


Reference
=========

* `Refuge RCouch wiki <https://github.com/refuge/rcouch/wiki/Replication-Algorithm>`_
* `CouchBase Lite IOS wiki <https://github.com/couchbase/couchbase-lite-ios/wiki/Replication-Algorithm>`_
* `CouchDB documentation <http://wiki.apache.org/couchdb/Replication>`_

.. _ECMA-262: http://www.ecma-international.org/publications/files/ecma-st/ECMA-262.pdf
.. _MVCC: http://en.wikipedia.org/wiki/Multiversion_concurrency_control
.. _CouchDB: http://couchdb.apache.org
.. _Erlang: http://erlang.org
.. _couch_replicator: https://github.com/apache/couchdb/tree/master/src/couch_replicator
.. _change notifications: http://guide.couchdb.org/draft/notifications.html
