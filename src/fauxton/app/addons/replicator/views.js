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
       "addons/fauxton/components",
       "addons/replicator/resources",
       "addons/replicator/happy"
],
function(app, FauxtonAPI, Components, replication) {
  var View = {},
  Events ={},
  pollingInfo ={
    rate: 5,
    intervalId: null
  },
  retryReplication = {};

  _.extend(Events, Backbone.Events);

  View.Sidebar = FauxtonAPI.View.extend({
    template: 'addons/replicator/templates/sidebartabs',
    afterRender: function(){ 
      var historyFrag = Backbone.history.fragment.replace("replication/",""),
          thisSection = (historyFrag=="replication")?'new':historyFrag;
      $(".replication-nav-stacked").find('.active').removeClass('active');
      $("#sidebar-content").find("[data-type-select='"+thisSection+"']").parents('li').addClass("active");
    }
  });


  View.ReplicationItem = FauxtonAPI.View.extend({
    tagName: "tr",
    template: "addons/replicator/templates/active",
    events: {
      "click .cancel": "cancelReplication"
    },
    establish: function(){
      return [this.model.fetch()];
    },
    cancelReplication: function(e){
      //need to pass "cancel": true with source & target
      var $currentTarget = this.$(e.currentTarget),
      repID = $currentTarget.attr('data-rep-id'),
      docID = $currentTarget.attr('data-doc-id'),
      that = this;

      this.newRepModel = new replication.Replicate({
        "id": docID,
        "replication_id": repID,
        "cancel": true
      });
      this.newRepModel.fetch().then(function(){
        that.deleteit();
      });
    },
    deleteit: function(){
      this.newRepModel.destroy().then(function(model, xhr, options){
          var notification = FauxtonAPI.addNotification({
            msg: "Replication stopped.",
            type: "success",
            clear: true
          });
        },
        function(model, xhr, options){
          var errorMessage = JSON.parse(xhr.responseText);
          var notification = FauxtonAPI.addNotification({
            msg: errorMessage.reason,
            type: "error",
            clear: true
          });
        });
    },
    serialize: function(){
      return {
        docs_written: this.model.get('docs_written'),
        target: this.model.get('target').replace(/\:(\w*)\b/,":*****"),
        source: this.model.get('source').replace(/\:(\w*)\b/,":*****"),
        continuous: this.model.get('continuous'),
        repid: this.model.get('replication_id'),
        docid: this.model.get('doc_id')
      };
    }
  });

  View.ActiveReplications = FauxtonAPI.View.extend({
    tagName: "table",
    className:  "replication-active replication-table table table-striped",
    template: "addons/replicator/templates/statustable",
    initialize:  function(){
      Events.bind('update:tasks', this.establish, this);
      this.listenTo(this.collection, "reset", this.render);
    },
    establish: function(){
      return [this.collection.fetch({reset: true})];
    },
    setPolling: function(){
      var that = this;
      this.cleanup();

      pollingInfo.intervalId = setInterval(function() {
        that.establish();
      }, pollingInfo.rate*1000);

    },
    cleanup: function(){
      clearInterval(pollingInfo.intervalId);
    },
    beforeRender:  function(){
      if (this.collection.length > 0 ) {
        this.collection.forEach(function(item) {
          this.insertView("#replication-status", new View.ReplicationItem({ 
            model: item
          }));
        }, this);
      } else {
        this.insertView("#replication-status", new View.ReplicatorNone({
          type: "active"
        }));
      }
    },
    afterRender: function(){
      this.setPolling();
    }
  });


  View.ReplicationErrors = FauxtonAPI.View.extend({
    tagName: "table",
    className:  "replication-errors  replication-table table table-striped",
    template: "addons/replicator/templates/statustable",
    initialize: function(options){
      this.databases = options.databases;
      this.replicatorDB = false;
    },
    establish: function(){
      var deferred = FauxtonAPI.Deferred(),
          that = this;

      FauxtonAPI.when(this.databases.fetch()).always(function(resp) {
        if (that.databases.getReplicatorDB().length > 0){

          that.collection.fetch().done(function(){
            that.replicatorDB = true;
            deferred.resolve();
          });
        } else {
          that.replicatorDB = false;
          deferred.resolve();
        }
      });
      return [deferred];
    },
    beforeRender: function(){
      //put this into the collection
      if (this.replicatorDB){
        var errors = this.collection.getErrored();
        if (errors.length > 0){
          errors.forEach(function(item) {
            //CAP R
            this.insertView("#replication-status", new View.ReplicationError({ 
              model: item
            }));
          }, this);
        } else {
          this.insertView("#replication-status", new View.ReplicatorNone({
            type: "errored"
          }));
        }
      } else {
        this.insertView("#replication-status", new View.ReplicatorNone({
          type: "errored"
        }));
      }
    }
  });


  View.ReplicatorNone = FauxtonAPI.View.extend({
    tagName: "tr",
    template: "addons/replicator/templates/nope",
    initialize:  function(options){
      this.type = options.type;
    },
    serialize: function(){
      return {
        type:  this.type
      };
    }
  });

  View.ReplicationValidator = FauxtonAPI.View.extend({
    tagName: "tr",
    template: "addons/replicator/templates/validator",
    events: {
      "click .close": "remove" 
    },
    serialize: function(){
      return {
        source: this.model.hasSource(),
        target: this.model.hasTarget(),
        user_ctx: this.model.hasUserCtx(),
        auth: false,
        model: this.model
      };
    }
  });


  View.ReplicationError = FauxtonAPI.View.extend({
    template: "addons/replicator/templates/error",
    tagName: "tr",
    events: {
      "click button.validate": "validate",
      "click button.retry": "retryRep"
    },
    retryRep:  function(){
      retryReplication.source = {
        url: this.model.get("source"),
        local: this.model.isLocal("source")
      };
      retryReplication.target = {
        url: this.model.get("target"),
        local: this.model.isLocal("target")
      };
      FauxtonAPI.navigate('/replication/new');
    },
    validate: function(){
      if (this.validateview){this.validateview.remove();}
      this.validateview = new View.ReplicationValidator({
        model:  this.model
      });
      this.$el.after(this.validateview.render().el);
    },
    serialize:  function(){
      return {
        status: this.model.get("_replication_state"),
        target: this.model.get("target").replace(/\:(\w*)\b/,":*****"),
        source: this.model.get("source").replace(/\:(\w*)\b/,":*****"),
        continuous: this.model.get("continuous")||false,
        repid: this.model.get("_id"),
        docid: this.model.get("_id")
      };
    }
  });

  View.ReplicationComplete = FauxtonAPI.View.extend({
    template: "addons/replicator/templates/complete",
    tagName: "tr",
    events: {
      "click button.retry": "postReplication"
    },
    postReplication: function(){
      var data = this.model.getDataforReplication(),
      that = this;

      this.newRepModel = new replication.Replicate({});
      this.newRepModel.save(data,{
        success: function(model, xhr, options){
          var notification;
          notification = FauxtonAPI.addNotification({
            msg: "Replication from "+model.get('source').url+" to "+ model.get('target').url+" has been posted to the _replicator database.",
            type: "success",
            clear: true
          });
          model.fetch().done(
            function(resp){ 
              if (resp._replication_state === "error") {
                notification = FauxtonAPI.addNotification({
                  msg: "Replication error. Check the _replicator database for errors.",
                  type: "error",
                  clear: true
                });
                FauxtonAPI.navigate('/replication/active');
              } else if (resp._replication_state === "triggered"){
                notification = FauxtonAPI.addNotification({
                  msg: "Replication has been triggered.",
                  type: "success",
                  clear: true
                });
              } else {
                notification = FauxtonAPI.addNotification({
                  msg: "This replication has been posted to the _replicator database but hasn't been fired yet. Check back in a few mins to see it's state.",
                  clear: true
                });
              }
            }
          );
        },
        error: function(model, xhr, options){
          var errorMessage = JSON.parse(xhr.responseText);
          var notification = FauxtonAPI.addNotification({
            msg: errorMessage.reason,
            type: "error",
            clear: true
          });
        }
      });
    },
    serialize:  function(){
      return {
        status: this.model.get("_replication_state"),
        target: this.model.get("target").replace(/\:(\w*)\b/,":*****"),
        source: this.model.get("source").replace(/\:(\w*)\b/,":*****"),
        continuous: this.model.get("continuous")||false,
        repid: this.model.get("_id"),
        docid: this.model.get("_id")
      };
    }
  });

  View.CompletedReplications = FauxtonAPI.View.extend({
    tagName: "table",
    className:  "replication-complete replication-table table table-striped",
    template: "addons/replicator/templates/statustable",
    initialize: function(options){
      this.databases = options.databases;
      this.replicatorDB = false;
    },
    establish: function(){
      var deferred = FauxtonAPI.Deferred(),
          that = this;

      FauxtonAPI.when(this.databases.fetch()).always(function(resp) {
        if (that.databases.getReplicatorDB().length > 0){

          that.collection.fetch().done(function(){
            that.replicatorDB = true;
            deferred.resolve();
          });
        } else {
          that.replicatorDB = false;
          deferred.resolve();
        }
      });
      return [deferred];
    },
    beforeRender: function(){
      if (this.replicatorDB){
        var completed = this.collection.getCompleted();
        if (completed.length > 0){
          completed.forEach(function(item) {
            this.insertView("#replication-status", new View.ReplicationComplete({ 
              model: item
            }));
          }, this);
        } else {
          this.insertView("#replication-status", new View.ReplicatorNone({
            type: "completed"
          }));
        }
      } else {
        this.insertView("#replication-status", new View.ReplicatorNone({
          type: "completed"
        }));
      }
    }
  });



  View.ReplicationForm = FauxtonAPI.View.extend({
    template: "addons/replicator/templates/form",
    events:  {
      "submit #replication": "getPassword",
      "change #create_target input[type='radio']": "showTargetTabs",
      "click #create_target label": "buttonActiveState"
    },
    initialize: function(options){
      this.status = options.status;
      this.selectedSource = options.source;
      Events.bind('replicate:start', this.replicateStart, this);
    },
    afterRender: function(){
      // client side form validation
      this.formvalidation();
      //focus on first input
      $(this.el).find("input:visible").eq(0).focus();
    },
    beforeRender:  function(){
      //insert the source and target tabs
      var sourceDB = this.selectedSource;
      this.insertView("#source_form", new View.LocalRemoteTabs({
        selectedDB: sourceDB  || "",
        type: "source"
      }));
      this.targetForm = this.insertView("#target_form", new View.LocalRemoteTabs({
        type: "target"
      }));

    },
    buttonActiveState: function(e){
      //Set the button state on LOCAL or NEW
      var $currentTarget = this.$(e.currentTarget);
      $currentTarget.parents("#create_target").find('.active').removeClass('active');
      $currentTarget.addClass('active');
    },
    enableFields: function(){
      this.$el.find('input','select').attr('disabled',false);
    },
    disableFields: function(){
      this.$el.find('input[type="text"]:hidden','select:hidden').not("[type='radio']").attr('disabled',true);
    },
    establish: function(){
      return [this.collection.fetch()];
    },
    formvalidation: function(e){
      //client side form validation
      var that = this;
      this.$('form#replication').isHappy({
        fields: {
          'input[name="source"]:visible': {
            required: true,
            tests: [
              {
                test: function(val){
                  return $('input[name="target"]:visible').val() !== val;
                },
                message: 'Your source cannot be the same DB as your target',
              },
              {
                test: function(val){
                  if (that.$('input#source_url').is(':visible')){
                    return true;
                  } else {
                    var alreadyExists = that.collection.where({"name":val});
                    return alreadyExists.length !== 0;
                  }
                },
                message: "This database doesn't exist."
              },
              {
                test: function(val){
                  if (!$('input[name="source"]:visible').is("#source_url")){
                    return true;
                  } else {
                    var regex = "^(http|https|ftp)://([a-zA-Z0-9.-]+(:[a-zA-Z0-9.&amp;%$-]+)*@)*((25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])|([a-zA-Z0-9-]+.)*[a-zA-Z0-9-]+.(com|edu|gov|int|mil|net|org|biz|arpa|info|name|pro|aero|coop|museum|[a-zA-Z]{2}))(:[0-9]+)*(/($|[a-zA-Z0-9.,?'\\+&amp;%$#=~_-]+))*$",
                    urlregex = new RegExp(regex);
                    return urlregex.test(val);
                  }
                },
                message: 'Remote databases must be written as urls.',
              }
            ]
          },
          'input[name="target"]:visible': {
            required: true,
            tests: [
              {
                test: function(val){
                  return $('input[name="source"]:visible').val() !== val;
                },
                message: 'Your target cannot be the same DB as your source.'
              },

              {
                test: function(val){
                  var alreadyExists = that.collection.where({"name":val});
                  if (that.$('input#target_url').is(':visible') ){
                    return true;
                  } else if (that.$('[name="create_target"]:checked').val()==="false") {
                    return alreadyExists.length !== 0;
                  } else {
                    return alreadyExists.length === 0;
                  }
                },
                message: "This database doesn't exist. Select New Database if you want to create it."
              },
              {
                test: function(val){
                  if (!$('input[name="target"]:visible').is("#target_url")){
                    return true;
                  } else {
                    var regex = "^(http|https|ftp)://([a-zA-Z0-9.-]+(:[a-zA-Z0-9.&amp;%$-]+)*@)*((25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0).(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])|([a-zA-Z0-9-]+.)*[a-zA-Z0-9-]+.(com|edu|gov|int|mil|net|org|biz|arpa|info|name|pro|aero|coop|museum|[a-zA-Z]{2}))(:[0-9]+)*(/($|[a-zA-Z0-9.,?'\\+&amp;%$#=~_-]+))*$",
                    urlregex = new RegExp(regex);
                    return urlregex.test(val);
                  }
                },
                message: 'Remote databases must be written as urls.',
              }
            ]
          }
        }
      });
    },
    showTargetTabs: function(e){
      //Switch from local to new
      if (this.targetForm){ this.targetForm.remove();}
      this.targetForm = this.insertView("#target_form", new View.LocalRemoteTabs({
                          newDB: this.$('[name="create_target"]:checked').val()==="true",
                          type: "target"
                        }));
      this.targetForm.render();
    },
    postToReplicator: function(json){
      // Post to _replicator DB
      var that = this;
      this.newRepModel = new replication.Replicate({});
      this.newRepModel.save(json,{
        success: function(model, xhr, options){
          var notification = FauxtonAPI.addNotification({
            msg: "Replication from "+model.get('source').url+" to "+ model.get('target').url+" has been posted to the _replicator database.",
            type: "success",
            clear: true
          });
          that.updateButtonText(false);
          that.checkReplicationState(model);
        },
        error: function(model, xhr, options){
          var errorMessage = JSON.parse(xhr.responseText);
          var notification = FauxtonAPI.addNotification({
            msg: errorMessage.reason,
            type: "error",
            clear: true
          });
          that.updateButtonText(false);
        }
      });
      this.enableFields();
    },  
    checkReplicationState: function(model){
      // check if it's been triggered
      // redirect to /replication/active if the replication has been triggered
      var replicator = model,
          notification;
      replicator.fetch().done(
        function(resp){ 
          if (resp._replication_state === "error") {
            notification = FauxtonAPI.addNotification({
              msg: "Replication error. Check the _replicator database for errors.",
              type: "error",
              clear: true
            });
            FauxtonAPI.navigate('/replication/errors');
          } else if (resp._replication_state === "triggered"){
            notification = FauxtonAPI.addNotification({
              msg: "Replication has been triggered.",
              type: "success",
              clear: true
            });
            FauxtonAPI.navigate('/replication/active');
          } else {
            notification = FauxtonAPI.addNotification({
              msg: "This replication has been posted to the _replicator database but hasn't been fired yet. Check the _replicator DB to see it's state.",
              clear: true
            });
          }
        }
      );
    },
    updateButtonText: function(showWaitingText){
      var $button = this.$('#replication button[type=submit]');
      if(showWaitingText){
        $button.text('Starting replication...').attr('disabled', true);
      } else {
        $button.text('Replication').attr('disabled', false);
      }
    },

    replicateStart: function(password){
      var formData = this.scrubFormData(),
          username = app.session.get('userCtx').name;
      if ($('#source_local').is(':visible')){
        formData.source = this.setAuthHeaders(formData.source,username,password);
      } 
      if ($('#target_local').is(':visible')){
        formData.target = this.setAuthHeaders(formData.target,username,password);
      }

      this.submit(formData);
    },
    getPassword: function(e){
      e.preventDefault();
      this.updateButtonText(true);
      var formData = this.scrubFormData();
      
      if ($('#source_local').is(':visible') || $('#target_local').is(':visible')){
         this.passwordPopup();
      } else {
        this.submit(formData);
      }
    },

    submit: function(formData){
      var that = this;
      if (this.collection.getReplicatorDB().length === 0){
        FauxtonAPI.when(this.collection.createReplicatorDB()).always(function(resp){
          that.postToReplicator(formData);
          that.enableFields();
        });
      }else{
        this.postToReplicator(formData);
        this.enableFields();
      }
     
 
     
    },
    setAuthHeaders: function(source,user,pass){
      var basicHeader = new FauxtonAPI.session.createBasicAuthHeader(user,pass),
          json = {};
          json.url = window.location.origin +"/"+ app.utils.safeURLName(source);
          json.headers = {
              "Authorization": basicHeader.basicAuthHeader
          };
      return json;
    },
    passwordPopup: function(){ 
      //insert Password Modal 
      var AuthenticationView = FauxtonAPI.getExtensions('replicator:Authentication');

      if (AuthenticationView){
      var model = new this.collection.model(),
          password = this.insertView("#password_modal", new AuthenticationView({
                        model: model
                      }));
          password.render();
      } else {
        this.submit(formData);
      }
    },
    scrubFormData: function(){
      //DISABLE HIDDEN FIELDS
      this.disableFields();
      var data = {};
      _.map(this.$('#replication').serializeArray(), function(formData){
        if(formData.value !== ''){
          //clean booleans & whitespaces
          if (formData.name == "_id"){
            data[formData.name]=formData.value.replace(/\s/g, '').toLowerCase();
          } else if (formData.name == "create_target" || formData.name == "continuous"){
              data[formData.name] = (formData.value ==="true")?true:false;
          } else {
          //Lotta stuff needs to be scrubbed before it's in proper json to submit
            data[formData.name] = formData.value.trim().replace(/\s/g, '-');
          }
        }
      });
      data.user_ctx = FauxtonAPI.session.get('userCtx');
      return data;
    }
  });

  View.PasswordModal = FauxtonAPI.View.extend({
    tagName: "div",
    className: "modal",
    template: 'addons/replicator/templates/password_modal',
    events: {
      "click a.cancel-button" : "hideModal",
      "click a.continue-button": "checkValidation"
    },
    initialize:  function(){
      this.showModal();
    },
    triggerReplication: function(){
      Events.trigger('replicate:start', this.$('[name="password"]').val());
    },
    checkValidation: function(e){
      e.preventDefault();
      var password = this.$('[name="password"]').val(),
          that = this;
      FauxtonAPI.when(this.model.authValidation(password)).always(function(){
        if (that.model.passworderror){
          var notification = FauxtonAPI.addNotification({
            msg: "Your password is incorrect.",
            type: "error",
            clear: true
          });
        } else {
          that.triggerReplication();
          that.hideModal();
        }
      });
    },
    hideModal: function(e){
      if(e){e.preventDefault();}
      $(this.el).modal('hide');
    },
    showModal: function(){
      $(this.el).modal({show:true});
    }
  });

  View.AdvancedOptions = FauxtonAPI.View.extend({
    className: "authenticate",
    template: "addons/replicator/templates/options",
    events: {
      "click .options": "toggleAdvancedOptions",
    },
    toggleAdvancedOptions:  function(e){
      this.$(e.currentTarget).toggleClass("off");
      this.$('.advancedOptions').toggle("hidden").find('input').removeAttr('disabled');
    }
  });

  View.LocalRemoteTabs = FauxtonAPI.View.extend({
    template: "addons/replicator/templates/localremotetabs",
    events:  {
      "click .nav-tabs a": "tabs"
    },
    afterRender: function(){
      this.dbSearchTypeahead = new Components.DbSearchTypeahead({
        dbLimit: 30,
        el: "input.auto",
        updater: function(item){
          return item;
        }
      });
      this.dbSearchTypeahead.render();
      this.preselectedDatabase();
    },
    initialize: function(options){
      this.type = options.type;
      this.newDB = options.newDB || false;
      this.selected = options.selectedDB || "";
    },

    preselectedDatabase: function(){
      //if selected database is passed through from the _all_dbs page
      if (this.selected){
        this.$('input[type="text"]:visible').val(this.selected);
      } else if (retryReplication[this.type]) {
        this.setLocation();
      }
    },
    setLocation: function(){
      if (retryReplication[this.type].local){
        var urlArray = retryReplication[this.type].url.split('/'),
            location = urlArray[urlArray.length-1];
        this.$('input[type="text"]:visible').val(location);
      } else {
        this.$('.remote-btn').trigger("click");
        this.$('input[type="text"]:visible').val(retryReplication[this.type].url);
      }
      delete retryReplication[this.type];
    },
    showAdvancedOptions:  function(e){
      //not called for now
      if (this.$(e.currentTarget).attr('name') === "source"){
        if (this.advancedOptions){ this.advancedOptions.remove();}
          this.advancedOptions = this.insertView("#options-here", new View.AdvancedOptions({}));
          this.advancedOptions.render();
      }
    },
    tabs: function(e){
      e.preventDefault();
      var $currentTarget = this.$(e.currentTarget),
          getTabID = "#"+$currentTarget.attr('data-tab');

      $currentTarget.parents('ul').find('.active').removeClass('active');
      $currentTarget.parents('li').addClass('active');

      $(getTabID).parents('.tab-content').find('.active').removeClass('active');
      $(getTabID).addClass('active');
    },
    serialize: function(){
      return {
        newDB:  this.newDB,
        username: app.session.get('userCtx').name,
        type: this.type
      };
    }
  });


  return View;
});
