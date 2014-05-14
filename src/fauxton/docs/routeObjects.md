RouteObjects
============

RouteObjects are one of the fundemental building blocks that we use in Fauxton. They help keep a module responsive and modular. RouteObjects are used to help manage Backbone.Views that
are used for a specific group of routes and layout. A RouteObject is able to facilitate events between views, load data for views as well as keep state for a specific module. 

A basic RouteObject would look like this.

        var  ExampleRouteObject = FauxtonAPI.RouteObject.extend({
            // Set what the main layout will be for this page
            // Some standard layouts are in app/addons/templates
            layout: "with_sidebar",

            // This is specific Fauxton feature, this will automatically build a set of breadcrumbs on the layout. This is optional.
            crumbs: [
              {"name": "example", "link": "_example"}
            ],


            // Its possible to fire RouteObject events via FauxtonAPI.triggerRouteEvent, these are handle by declaring them here. This works the same way
            // that Backbone.Views works with events. Important to note that 'route:' must be always be added when declaring the event in the RouteObject
            // But don't add 'route:' when using FauxtonAPI.triggerRouteEvent
            events: {
              'route:example-event': "handleEvent"
            },

            // Setting this will mark a heading on the primary nav as active. This is optional.
            selectedHeader: "example",

            // Defining the roles is how Fauxton manages authentication and limits who can access a url. 
            // Define any couchdb role here and only users with that role assigned to them will be able to view this RouteObject's views.
            //It is also possible to define a role for a route.
            roles: ["_admin"],

            // Define all the routes for this RouteObject, it uses Backbone.Router, so review its (documentation)[http://backbonejs.org/#Router]
            // for route examples. The second route (_exampleWithAuth) uses an expanded notation so that only users with a specific role can access
            // that specific role
            routes: {
              "_example": "showExample",
              "_exampleWithAuth": {
                route: "exampleWithAuth",
                roles: ['_reader']
               }
            },

            // This will set a url for the api header. This is option. The first item in the array is the array. The second one must always be 
            // the documentation link. This function is optional.
            apiUrl: function() {
              return [this.exampleModel.url(), this.exampleModel.documentation];
            },

            // initialize like all initializes in backbone.js is called on construction of the object. This is where all views, collections and models should be declared that 
            // will be used throughout the lifespan of the RouteObject.
            initialize: function () {
              this.exampleModel = new Example();
            },

            // Establish is used to fetch all data from the server before rendering any of the views. Return any promises that are required to have been resolved before any views get rendered
            // the RouteObject will make sure they are all completed before rendering views.
            establish: {
              return this.exampleModel.fetch();
            },

            // This is an example of a method called for a specific route. Methods like these are used to wire up any views needed. Using the 'setView' method, we can set which views
            // we want rendered as well as to what dom element they should be rendered in.
            // The RouteObject will render these, as well as clean them up when they are finished being used.
            showExample: function () {
              this.setView("#dashboard-content", new Log.Views.View({collection: this.logs}));
            }
          });

General usage pattern

The general usage pattern is to define a RouteObject for a group of related Routes. Often it is just one RouteObject per addon, but some addons like the Documents addons have multiple RouteObjects. 
RouteObject makes fetching data for collections and models a lot easier. In the initialise of the RouteObject, create the required collections/models. Then in each route method bind the collections/models to the views. 
Finally call fetch on the collections/models in the establish. 

For a page with plenty of views often the views need to send "messages" or events to other views to notify them that something has happened. This can be done by using the RouteObject events.

Rendering order

The order methods are called in a RouteObject are as follows.
* The initialise is only called once when a defined route/url in a routeObject is navigated to for the first time. If the next url matches in the same RouteObject, the initialise will not be called again.
* The Routes method is then called. Here all views are created and we define what dom object we would like the view to be attached to. The view is not yet put into the dom.
* The RouteObjects `establish` method is then called and all collection's and model's data are fetched. Once all that of those promises are resolved, the views will be rendered and put into the dom.

