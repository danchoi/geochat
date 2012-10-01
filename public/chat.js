WEB_SOCKET_SWF_LOCATION = "websocket_js/WebSocketMain.swf";

var webSocket;  // A global

// see beget: From p. 22 of JavaScript the Good Parts

var geoRoomPrototype = {
  toString: function() {
    console.log("TEST toString");
  }
}
var messagePrototype  = {
  toString: function() {
    console.log("Message: TEST toString");
  }
}

function getURLParameter(name) {
  return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null;
}

$(document).ready(function() {
  var webSocketURL = getURLParameter('dev') ? 'ws://localhost:9394' : 'ws://poddb.com:9394';
  webSocket = new WebSocket(webSocketURL); 
  webSocket.onopen = function(event){
    $('#chatStream').append('<br>Connected to the server');
    webSocket.send("/rooms");
  };

  // test ich template
  // console.log( ich.user({name: 'test name', twitter: 'danchoi'}) );

 
  // TODO move this stuff into a dispatcher class 

  webSocket.onmessage = function(event){
    if (event.data.length > 0) {
     if (event.data[0] == '{') {
       var x;
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

     } else {  // receive a chat message 
        
        var message = event.data;
        message.prototype = messagePrototype;
        console.log("message received");
        console.log(message.toString());
        $('#chatStream').append(message);
        var liveRoomId = $('#chatStream .message').last().attr("room_id");
        if (liveRoomId ) {
          console.log("live room id: "+liveRoomId);
          x = roomCircles[liveRoomId];
          console.log(x);
          x.setOptions({strokeColor: "red", radius: 220, strokeWeight: 7});
          setTimeout(
           function() {
            x.setOptions({strokeColor: "blue", radius: 180, strokeWeight: 4});

           }, 100
          );
        }
        $('#chatStream').animate({scrollTop: $('#chatStream').height()});
     }
    }
  };
  
  webSocket.onclose = function(event){
    $("#chatStream").append('<br>Connection closed');
  };
 
 
  
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
});

