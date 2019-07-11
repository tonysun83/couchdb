var Search3 = (function() {

  var index_results = []; // holds temporary emitted values during index

  function handleIndexError(err, doc) {
    if (err == "fatal_error") {
      throw(["error", "map_runtime_error", "function raised 'fatal_error'"]);
    } else if (err[0] == "fatal") {
      throw(err);
    }
    var message = "function raised exception " + err.toSource();
    if (doc) message += " with doc._id " + doc._id;
    log(message);
  };

  return {
    index: function(name, value, options) {
      if (typeof name !== 'string') {
        throw({name: 'TypeError', message: 'name must be a string not ' + typeof name});
      }
      if (name.substring(0, 1) === '_') {
        throw({name: 'ReservedName', message: 'name must not start with an underscore'});
      }
      if (typeof value !== 'string' && typeof value !== 'number' && typeof value !== 'boolean') {
        throw({name: 'TypeError', message: 'value must be a string, a number or boolean not ' + typeof value});
      }
      if (options && typeof options !== 'object') {
        throw({name: 'TypeError', message: 'options must be an object not ' + typeof options});
      }
      index_results.push([name, value, options || {}]);
    },

    indexDoc: function(doc) {
      Couch.recursivelySeal(doc);
      var buf = [];
      for each (fun in State.funs) {
        index_results = [];
        try {
          fun(doc);
          buf.push(index_results);
        } catch (err) {
          handleIndexError(err, doc);
          buf.push([]);
        }
      }
      print(JSON.stringify(buf));
    }

  }
})();