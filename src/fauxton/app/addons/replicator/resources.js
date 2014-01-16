// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.
define([
  "app",
  "api",
  'addons/activetasks/resources',
  "addons/documents/resources"
],

function (app, FauxtonAPI, ActiveTasks, Documents) {
  var Replication = {};

  //these are probably dupes from the database addon. I'm going to keep them seperate for now.

  //Extend from top level document instead
  Replication.DBModel = Backbone.Model.extend({
    url: function(){
      return app.host + "/" + this.id;
    },
    label: function () {
      //for autocomplete
        return this.get("name");
    },
    authValidation: function(password){
      var username = app.session.get('userCtx').name,
          that = this;
      return $.ajax({
        cache: false,
        type: "POST", 
        url: "/_session", 
        dataType: "json",
        data: {name: username, password: password},
        success: function(){
          that.passworderror = false;
        },
        error: function(resp){
          that.passworderror = true;
        }
      });
    }
  });


//extend alldocs funky, but theoretically 
  Replication.DBList = Backbone.Collection.extend({
    model: Replication.DBModel,
    url: function() {
      return app.host + "/_all_dbs";
    },
    getFiltered: function(dbname){
      var filtered = this.filter(function(rep) {
          return rep.get("name") === dbname;
          });
      return new Replication.DBList(filtered);
    },
    getReplicatorDB: function(){
      return this.getFiltered("_replicator");
    },
    createReplicatorDB: function(){
      var db = new this.model();
      return db.save({
        id: "_replicator",
        name: "_replicator"
      });
    },
    parse: function(resp) {
      // TODO: pagination!
      return _.map(resp, function(database) {
        return {
          id: database,
          name: database
        };
      });
    }
  });



//extend from active tasks
  Replication.Task = Backbone.Model.extend({});

  Replication.Tasks = Backbone.Collection.extend({
    model: Replication.Task,
    url: function () {
      return app.host + '/_active_tasks';
    },
    fetch: function (options) {
     var fetchoptions = options || {};
     fetchoptions.cache = false;
     return Backbone.Model.prototype.fetch.call(this, fetchoptions);
    },
    parse: function(resp){
      //only want replication tasks to return
      return _.filter(resp, function(task){
        return task.type === "replication";
      });
    }
  });

  Replication.Replicate = Backbone.Model.extend({
    idAttribute: 'id',
    documentation: "replication_doc",
    url: function(){
      var docid = this.get('id') ? "/"+this.get('id'):"";
      return app.host + "/_replicator"+docid;
    },
    destroy: function() {
      var url = this.url() + "?rev=" + this.get('_rev');
      return $.ajax({
        url: url,
        dataType: 'json',
        type: 'DELETE'
      });
    }
  });

  Replication.item = Backbone.Model.extend({
    defaults: {
      source: "Source is missing",
      target: "Target is missing",
      create_target: false,
      continuous: false,
      doc: {}
    },
    initialize: function(){

      var regex = "^(http|https|ftp)://([a-zA-Z0-9.-]+(:[a-zA-Z0-9.&amp;%$-]+)*@)*((25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])|([a-zA-Z0-9-]+.)*[a-zA-Z0-9-]+.(com|edu|gov|int|mil|net|org|biz|arpa|info|name|pro|aero|coop|museum|[a-zA-Z]{2}))(:[0-9]+)*(/($|[a-zA-Z0-9.,?'\\+&amp;%$#=~_-]+))*$";
      this.urlregex = new RegExp(regex);
    },
    hasSource: function(){
      //has a source
      return this.get("source") !== this.defaults.source;
    },
    hasTarget: function(){
      //has a target 
      return this.get("target") !== this.defaults.target;
    },
    hasUserCtx: function(){
      //has username & has roles
      return this.get("user_ctx") === undefined;
    },
    isBoolean: function(test){
      return typeof test === "boolean";
    },
    isURL: function(type){
      var checkType = type || "target",
          url = this.get(checkType);
      return this.urlregex.test(url);
    },
    isLocal: function(type){
      var checkType = type || "target",
          thisURL = this.get(checkType);
        
      return (thisURL.indexOf(app.host) !== -1);
    },

    getDataforReplication:  function(){
      return {
        source: this.get('doc').source,
        target: this.get('doc').target,
        create_target: this.get('create_target'),
        user_ctx: this.get('user_ctx'),
        continuous: this.get('continuous')
      };
    }
  });

  Replication.Replicator = Backbone.Collection.extend({
    model: Replication.item,
    idAttribute: '_id',
    documentation: "replication_doc",
    url: function(){
      return app.host + "/_replicator/_all_docs?limit=100&include_docs=true";
    },
    getfiltered: function(type){
      var filtered = this.filter(function(rep) {
          return rep.get("_replication_state") === type;
          });
      return new Replication.Replicator(filtered);
    },
    getCompleted: function(){
      return this.getfiltered("completed");
    },
    getErrored: function(){
      return this.getfiltered("error");
    },
    getURL: function(source){
      if (typeof source == "object") {
        return source.url;
      } else {
        return source;
      }
    },
    parse: function(resp){
      var rows = resp.rows,
          getSource = this.getURL;

      return _.map(rows, function(row) {
        var source = getSource(row.doc.source),
            target = getSource(row.doc.target);

        return {
          _id: row.id,
          _rev: row.value.rev,
          value: row.value,
          key: row.key,
          create_target: row.doc.create_target || undefined,
          continuous: row.doc.continuous || undefined,
          _replication_state:  row.doc._replication_state,
          source: source || undefined,
          target: target || undefined,
          doc: row.doc,
          user_ctx: row.value.user_ctx
        };
      });
    }
  });


  return Replication;
});
