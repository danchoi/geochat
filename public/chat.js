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
     webSocket.send("/rooms");
   };

   
   webSocket.onmessage = function(event){
     if (event.data.length > 0) {
      if (event.data[0] == '{') {

        console.log(event.data);
        var data = JSON.parse(event.data);
        if (data.rooms) {
          for (var i in data.rooms) {
            var roomOptions;
            var room; 
            room = JSON.parse(data.rooms[i]);
            console.log("ROOM"+room);
            var center = new google.maps.LatLng(parseFloat(room.lat), parseFloat(room.lng));
            roomOptions = {
              strokeColor: "blue",
              strokeOpacity: 0.6,
              strokeWeight: 4,
              fillColor: "#FFFFFF",
              fillOpacity: 0.1,
              map: map,
              center: center,
              radius: 180
            }
            roomCircle = new google.maps.Circle(roomOptions);
            roomCircles.push(roomCircle);
            attachEventHandlerToStop(roomCircle, room);
          }
        } else if (data.room_id) {  // create a room
           var  room = data;
            var center = new google.maps.LatLng(parseFloat(room.lat), parseFloat(room.lng));
            roomOptions = {
              strokeColor: "blue",
              strokeOpacity: 0.6,
              strokeWeight: 4,
              fillColor: "#FFFFFF",
              fillOpacity: 0.1,
              map: map,
              center: center,
              radius: 180
            }
            roomCircle = new google.maps.Circle(roomOptions);
            roomCircles.push(roomCircle);
            attachEventHandlerToStop(roomCircle, room);

        }

      } else {
         $('#chatStream').append(event.data);
         liveRoom = $('#chatStream .message');

         $('#chatStream').animate({scrollTop: $('#chatStream').height()});
      }
     }
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

    $("#create_stream").click(function(e) {
      if (initialLocation) {
        var msg = "/create new_room "+initialLocation.lat()+" "+initialLocation.lng();
        console.log(msg);
        webSocket.send(msg);
      }
    });
    
   })
   


