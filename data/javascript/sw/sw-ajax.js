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



/// address-bar.lisp ///
////////////////////////

$.address.change(function(event){
    //alert(event.value);
    swAjax("&event=url-hash-changed",
           "&new-url-hash=" + encodeURIComponent(event.value));
  });



/// Boot! ///
/////////////

swComet("&do=refresh&hash=" + encodeURIComponent(encodeURIComponent(swGetCurrentHash().substr(1))));
