/***
 * Excerpted from "HTML5 and CSS3",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/bhh5 for more book information.
***/
WEB_SOCKET_SWF_LOCATION = "websocket_js/WebSocketMain.swf";



   //var webSocket = new WebSocket('ws://localhost:9394/');
   var webSocket = new WebSocket('ws://poddb.com:9394/');
   
   webSocket.onopen = function(event){
     $('#chatStream').append('<br>Connected to the server');
   };
   
   webSocket.onmessage = function(event){
     $('#chatStream').append(event.data);
     $('#chatStream').animate({scrollTop: $('#chatStream').height()});
   };
   
   webSocket.onclose = function(event){
     $("#chatStream").append('<br>Connection closed');
   };
   
   
   $(function(){
     $("form#chat_form").submit(function(e){
        e.preventDefault();
        var textfield = $("#message");
        webSocket.send(textfield.val());
        textfield.val("");
      });
    $("form#nick_form").submit(function(e){
      e.preventDefault();
      var textfield = $("#nickname");
      webSocket.send("/nick " + textfield.val());
    });
    
   })
   


