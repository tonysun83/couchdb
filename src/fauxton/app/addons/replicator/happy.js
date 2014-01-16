// HAPPY JS.
// WARNING:  this has been editted to support custom error message handling. 
// $('#awesomeForm').isHappy({
//   fields: {
//     // reference the field you're talking about, probably by `id`
//     // but you could certainly do $('[name=name]') as well.
//     '#yourName': {
//       required: true,
//       message: 'Might we inquire your name'
//     },
//     '#email': {
//       required: true,
//       defaultMessage: 'How are we to reach you sans email??',
//       test:[
          // {
          //   test: happy.email, // this can be *any* function that returns true or false
          //   message: "default message here"
          // },
          // {
          //   test: happy.equals,
          //   message: "this needs to equal that"
          // }
//      ]
//   },
//   errorHandling: function(error){console.log(error.message);}
// });
//
// if you don't pass in a function for errorHandling, it behaves as designed. - Sue

(function($){
  function trim(el) {
    return (''.trim) ? el.val().trim() : $.trim(el.val());
  }
  $.fn.isHappy = function (config) {
    var happy = {
      USPhone: function (val) {
        return (/^\(?(\d{3})\)?[\- ]?\d{3}[\- ]?\d{4}$/).test(val);
      },

      // matches mm/dd/yyyy (requires leading 0's (which may be a bit silly, what do you think?)
      date: function (val) {
        return (/^(?:0[1-9]|1[0-2])\/(?:0[1-9]|[12][0-9]|3[01])\/(?:\d{4})/).test(val);
      },
      
      email: function (val) {
        return (/^(?:\w+\.?)*\w+@(?:\w+\.)+\w+$/).test(val);
      },
      
      minLength: function (val, length) {
        return val.length >= length;
      },
      
      maxLength: function (val, length) {
        return val.length <= length;
      },
      
      equal: function (val1, val2) {
        return (val1 == val2);
      }
    };

    function errorMessaging(error,selector){
      if(config.errorHandling){
        config.errorHandling(error);
      }else{
        var message = error.useRequiredMessage?error.defaultMessage:error.message;

        $(selector).parent().find("#"+error.id).remove();
        var errorEl = $('<span id="'+error.id+'" class="unhappyMessage">'+message+'</span>');
        $(selector).after(errorEl);
      }
    }

    function handleSubmit(e) {
      var errors = false, i, l,
      fields = getFields();
      for (i = 0, l = fields.length; i < l; i += 1) {
        if (!fields[i].testValid(true)) {
          errors = true;
        }
      }
      if (errors) {
        if (isFunction(config.unHappy)) config.unHappy();
        return false;
      } else if (config.testMode) {
        if (window.console) console.warn('would have submitted');
        return false;
      }
    }

    function isFunction (obj) {
      return !!(obj && obj.constructor && obj.call && obj.apply);
    }

    function getFields(){
      var fields = [];
      for ( var formField in config.fields) {
        fields.push(processField(config.fields[formField], formField));
      }
      return fields;
    }

    function processField(opts, selector) {
      var field = $(selector),
          errorM = {
            defaultMessage: opts.defaultMessage || "This field is required",
            message: " ",
            id: field.attr('name') + '_unhappy',
            useRequiredMessage: false
          };

      field.testValid = function (submit) {
        var val,
          el = $(this),
          gotFunc,
          error = false,
          temp, 
          required = opts.required,
          password = (field.attr('type') === 'password'),
          arg = isFunction(opts.arg) ? opts.arg() : opts.arg,
          tests = opts.tests || [];
          
        if ($(this).length <= 0){return true;}
        // clean it or trim it
        if (isFunction(opts.clean)) {
          val = opts.clean(el.val());
        } else if (!opts.trim && !password) {
          val = trim(el);
        } else {
          val = el.val();
        }
        
        // write it back to the field
        el.val(val);
        
        // get the value
        gotTests = ((val.length > 0 || required === 'sometimes') && tests.length !== 0);
        
        // check if we've got an error on our hands
        if (submit === true && required === true && val.length === 0) {
          error = true;
          errorM.useRequiredMessage = true;
        } else if (gotTests) {
          for (var i = 0; i < opts.tests.length; i++){
            if (isFunction(opts.tests[i].test) && !opts.tests[i].test(val, arg)){
              errorM.message = opts.tests[i].message;
              error = true;
              break;
            }
          }
        }
        
        if (error) {
          el.addClass('unhappy');
          errorMessaging(errorM, el);
          return false;
        } else {
          el.removeClass('unhappy');
          $("#"+field.attr('name') + '_unhappy').remove();
          return true;
        }
      };

      field.on(config.when || 'blur', field.testValid);
      return field;
    }
    
    if (config.submitButton) {
      $(config.submitButton).on("click", handleSubmit);
    } else {
      
      this.bind('submit', handleSubmit);
    }
    return this;
  };
})(this.jQuery || this.Zepto);

