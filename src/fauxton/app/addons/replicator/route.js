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
  "addons/replicator/resources",
  "addons/replicator/views"
],
function(app, FauxtonAPI, Replication, Views) {
  var  RepRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_sidebar",
    roles: ["_admin"],
    routes: {
      "replication": "showSection",
      "replication/:section": "showSection",
      "replication/:section/:source": "showSection"
    },
    selectedHeader: "Replication",
    apiUrl: function() {
      return [this.replicate.url(), this.replicate.documentation];
    },
    crumbs: [
      {"name": "Replication", "link": "replication"}
    ],
    initialize: function(){
			this.databases = new Replication.DBList({});
      this.tasks = new Replication.Tasks({id: "ReplicationTasks"});
      this.replicate = new Replication.Replicate({});
      this.replicatorDB = new Replication.Replicator({});
      this.setView("#sidebar-content", new Views.Sidebar({}));  
    },
    showSection: function(section, source){
      var maincontent;
      $('.replication-nav-stacked').find('li').removeClass("active");
      switch (section) {
        case "new":
          maincontent = new Views.ReplicationForm({
            source: source || "",
            collection: this.databases
          });
          this.crumbs = [
            {"name": "Replication", "link": "replication"},
            {"name": "Start a new replication", "link": "#"}
          ];
        break;
        case "active":
          maincontent = new Views.ActiveReplications({
            collection: this.tasks
          });
          this.crumbs = [
            {"name": "Replication", "link": "replication"},
            {"name": "Active replications", "link": "#"}
          ];
        break;
        case "errors":
          maincontent = new Views.ReplicationErrors({
            databases: this.databases,
            collection: this.replicatorDB
          });
          this.crumbs = [
            {"name": "Replication", "link": "replication"},
            {"name": "Errors", "link": "#"}
          ];
        break;
        case "complete":
          maincontent = new Views.CompletedReplications({
            databases: this.databases,
            collection: this.replicatorDB
          });
          this.crumbs = [
            {"name": "Replication", "link": "replication"},
            {"name": "Completed", "link": "#"}
          ];
        break;
        default:
          maincontent = new Views.ReplicationForm({
            source: source || "",
            collection: this.databases
          });
        break;
      }
      $('a[data-type-select="'+section+'"]').parents('li').addClass('active');
      this.setView("#dashboard-content", maincontent); 
    }
  });


	Replication.RouteObjects = [RepRouteObject];

  return Replication;
});
