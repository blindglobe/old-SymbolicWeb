/*
For this file to bootstrap correctly the following variables must be bound:

  * sw_viewport_id [string]

  * sw_dynamic_subdomain [string]

*/



/// swGetCurrentHash ///
////////////////////////



if($.browser.mozilla)
  swGetCurrentHash = function(){
    if((window.location.hash).length > 1)
      // https://bugzilla.mozilla.org/show_bug.cgi?id=378962 *sigh*
      return "#" + window.location.href.split("#")[1].replace(/%27/g, "'");
    else
      return "#";
  }
else
  swGetCurrentHash = function(){
    return location.hash
  };



/// swURL ///
/////////////

function swURL(){
  return [window.location.protocol, "//", sw_dynamic_subdomain, window.location.host, window.location.pathname].join('');
}



/// swAjax ///
//////////////


swAjax = (function(){
    var queue = new Array();
    var timer = false;

    function displaySpinner(){
      $("#sw-loading-spinner").css("display", "block");
    }

    function handleRestOfQueue(){
      queue.shift();
      if(queue.length != 0)
        queue[0]();
      else{
        if(timer){
          clearTimeout(timer);
          timer = false;
          $("#sw-loading-spinner").css("display", "none");
        }
      }
    }

    return function(params, callback_data, after_fn){
      if(queue.push(function(){
            var options = {
              type: "POST",
              url: [window.location.pathname, "?_sw-request-type=ajax", "&_sw-viewport-id=", sw_viewport_id, params].join(''),
              data: callback_data,
              dataType: "script",
              beforeSend: function(){ if(!timer){ timer = setTimeout(displaySpinner, 500); }}, // TODO: 500 should be configurable.
              complete: handleRestOfQueue
            };
            if(after_fn)
              options.success = after_fn;
            $.ajax(options);
          }) == 1)
        queue[0]();
    }
  })();



/// swComet ///
///////////////

sw_comet_response = false;
swComet = (function(){
    function callback(){
      if(sw_comet_response)
        sw_comet_response = false, swComet('&do=ack');
      else
        // FIXME: This stuff never happen for Webkit.
        setTimeout("swComet('');", 500);
    }

    function doIt(params){
      $.ajax({
          type: "GET",
          url: [swURL(), "?_sw-request-type=comet", "&_sw-viewport-id=", sw_viewport_id, params].join(''),
          dataType: "script",
          complete: callback});
    }

    if(!$.browser.mozilla)
      return doIt;
    else
      // NOTE: This gets rid of the "always loading" thing in FF for the mouse pointer and the tab icon/favicon.
      // FIXME: Chrome is still stuck always "loading.." though.
      return function(params){ setTimeout(function(){ doIt(params); }, 0); };
  })();



/// swHandleEvent ///
/////////////////////

function swHandleEvent(callback_id, js_before, callback_data, js_after){
  try{
    if(js_before())
      swAjax("&event=dom-event&callback-id=" + callback_id,
             callback_data,
             js_after());
  }
  catch(exception){
    swAjax("&event=event-exception&callback-id=" + callback_id,
           "&exception-str=" + encodeURIComponent(exception.toString()));
  }
}



/// swTerminateSession ///
//////////////////////////

function swTerminateSession(){
  swAjax("&event=terminate-session", "", function(){ window.location.reload(); });
}



/// swDisplaySessionInfo ///
////////////////////////////

function swDisplaySessionInfo(){
  swAjax("&event=display-session-info", "");
}



/// swReturnValue ///
/////////////////////

function swReturnValue(code_id, func){
  swAjax("&event=js-ack&code-id=" + code_id,
         "&return-value=" + encodeURIComponent(func()));
}



/// swReturnFail ///
////////////////////

function swReturnFail(code_id, exception){
  swAjax("&event=js-fail&code-id=" + code_id,
         "&exception-str=" + encodeURIComponent(exception.toString()));
}



/// swRun ///
/////////////

function swRun(code_id, async_p, func){
  try{
    if(async_p)
      func();
    else
      swReturnValue(code_id, func);
  }
  catch(exception){
    swReturnFail(code_id, exception);
  }
}



/// Handling of history stuff ///
/////////////////////////////////
/*
(function($){
  if($.browser.msie){
    // Internet Exploder
    ////////////////////
    var last_hash = swGetCurrentHash();
    if(last_hash == '#') location.replace('#');

    $("body").prepend('<iframe id="ie-history" style="display: none;"></iframe>');
    var ie_history = $("#ie-history")[0].contentWindow.document;
    ie_history.open(); ie_history.close();
    ie_history.location.hash = last_hash;

    updateHash = function(new_hash, replace_p){
      var ie_history = $("#ie-history")[0];
      ie_history = ie_history.contentDocument || ie_history.contentWindow.document; // TODO: I have no idea why I need to do this, but it seems to work.
      new_hash = "#" + new_hash;

      // So the "interval-code" below won't trigger.
      if(replace_p){
        location.replace(last_hash = new_hash);
        ie_history.location.replace(last_hash);
      }
      else{
        location.hash = last_hash = new_hash;
        ie_history.open(); ie_history.close();
        ie_history.location.hash = new_hash;
      }
    }

    // TODO: Wait for browsers to have support for onhashchanged event. Gotta love how we can't do this on our own without MS doing it "for us" first. Yes, I'm bitter.
    setInterval(function(){
        var ie_history = $("#ie-history")[0];
        ie_history = ie_history.contentDocument || ie_history.contentWindow.document; // TODO: I have no idea why I need to do this, but it seems to work.

        var curr_hash = swGetCurrentHash();
        if((ie_history.location.hash != last_hash) || (curr_hash != last_hash)){
          if(curr_hash != last_hash){
            ie_history.open(); ie_history.close();
            ie_history.location.hash = last_hash = curr_hash;
          }
          else
            location.hash = last_hash = ie_history.location.hash;
          // TODO: Only transmit diff!
          swAjax("&event=url-hash-changed",
                 "&new-url-hash=" + encodeURIComponent(encodeURIComponent(last_hash.substr(1)))); // TODO: Try to figure out why I need to do this so I can add a comment explaining why later ..
        }
      },
      250);
  }
  else if($.browser.safari){
    var last_hash = swGetCurrentHash();

    updateHash = function(new_hash, replace_p){
      new_hash = "#" + new_hash;
      // So the "interval-code" below won't trigger.
      if(replace_p){
        // FIXME: Safari seems to have the same problem Opera does when it comes to the replace function ..
        // I've reported a bug about this here: https://bugs.webkit.org/show_bug.cgi?id=20425
        window.location.replace(last_hash = new_hash);
      }
      else
        window.location.hash = last_hash = new_hash;
    }

    // TODO: Wait for browsers to have support for onhashchanged event. Gotta love how we can't do this on our own without MS doing it "for us". Yes, I'm bitter.
    setInterval(function(){
        var curr_hash = swGetCurrentHash();
        if(curr_hash != last_hash){
          // TODO: Only transmit diff!
          last_hash = curr_hash;
          swAjax("&event=url-hash-changed",
                 "&new-url-hash=" + encodeURIComponent(encodeURIComponent(last_hash.substr(1)))); // TODO: Try to figure out why I need to do this so I can add a comment explaining why later ..
          // FIXME: blah .. this seems to be neccessary for some reason .. extra load on server, waste of bandwidth .. great
          //swComet('');
        }
      },
      250);
  }
  else{
    // Firefox, Opera or other (unknown) browsers
    /////////////////////////////////////////////
    var last_hash = swGetCurrentHash();

    updateHash = function(new_hash, replace_p){
      new_hash = "#" + new_hash;
      // So the "interval-code" below won't trigger.
      if(replace_p)
        window.location.replace(last_hash = new_hash);
      else
        window.location.hash = last_hash = new_hash;
    }

    // TODO: Wait for browsers to have support for onhashchanged event. Gotta love how we can't do this on our own without MS doing it "for us". Yes, I'm bitter.
    setInterval(function(){
        var curr_hash = swGetCurrentHash();
        if(curr_hash != last_hash){
          // TODO: Only transmit diff!
          last_hash = curr_hash;
          swAjax("&event=url-hash-changed",
                 "&new-url-hash=" + encodeURIComponent(encodeURIComponent(last_hash.substr(1)))); // TODO: Try to figure out why I need to do this so I can add a comment explaining why later ..
        }
      },
      250);
  }
})(jQuery);
*/


/// Boot! ///
/////////////

swComet("&do=refresh&hash=" + encodeURIComponent(encodeURIComponent(swGetCurrentHash().substr(1))));
