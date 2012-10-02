WEB_SOCKET_SWF_LOCATION = "websocket_js/WebSocketMain.swf";

var rooms = [{"room_id": 3, "lat": 42.36, "lng": -71.07}, {"room_id": 4, "lat": 42.36, "lng": -71.08}, {"lat": 42.32, "lng": -71.08, "room_id": 5}
];

var map;
var roomCircles = [];

var initialLocation;

function randomlyPulse() {

};

function attachEventHandlerToStop(roomCircle, room) {
  google.maps.event.addListener(roomCircle, 'mouseover', function() {
    roomCircle.setOptions({strokeColor: "red", radius: 220, strokeWeight: 7});
  });
  google.maps.event.addListener(roomCircle, 'mouseout', function() {
    roomCircle.setOptions({strokeColor: "blue", radius: 180, strokeWeight: 4});
  });
  google.maps.event.addListener(roomCircle, 'click', function() {
    console.log("Room clicked: "+ room.room_id); 
    GeoGossip.ws.send("/enter "+room.room_id );
  });
}



var GeoGossip = {
  createRoom: function(pos)  {
    var msg = {clientEvent: 'roomCreated', lat: pos.lat(), lng: pos.lng()}
    GeoGossip.ws.send(JSON.stringify(msg));
  }
};


function createMap() {
  var center;
  var latLng = new google.maps.LatLng(42.36, -71.08);
  var myOptions = {
    center: latLng,
    zoom: 12,
    mapTypeId: google.maps.MapTypeId.ROADMAP
  };

  $("#create_chat").hide();

  map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);

  google.maps.event.addListener(map, 'click', function(event) {
    var pos = event.latLng;
    GeoGossip.createRoom(pos);
  });

  randomlyPulse();

  if(navigator.geolocation) {
    browserSupportFlag = true;
    navigator.geolocation.getCurrentPosition(function(position) {
      initialLocation = new google.maps.LatLng(position.coords.latitude,position.coords.longitude);
      $("#create_chat").show();
    }, function() {
      // console.log("No geolocation");
    });
  }
}


// see beget: From p. 22 of JavaScript the Good Parts

var geoRoomPrototype = {
  toString: function() {
    console.log("Room ID: "+this.room_id);
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
  createMap();
  GeoGossip.ws = new WebSocket(webSocketURL); 
  GeoGossip.ws.onopen = function(event){
    $('#chatStream').append('<br>Connected to the server');
    GeoGossip.ws.send("/rooms");
  };


  GeoGossip.ws.onmessage = function(event){
    if (event.data.length > 0) {
     if (event.data[0] == '{') {  // We have a JSON payload and should parse it
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
          room.prototype = geoRoomPrototype;
          console.log("create room: "+ room.lat);

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
        console.log(message.toString());
        $('#chatStream').append(ich.message(message));
        var liveRoomId = message.room_id;
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
  
  GeoGossip.ws.onclose = function(event){
    $("#chatStream").append('<br>Connection closed');
  };
 
 
  
  $("form#chat_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#message");
    GeoGossip.ws.send(textfield.val());
    textfield.val("");
  });

  $("form#nick_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#nickname");
    GeoGossip.ws.send("/nick " + textfield.val());
  });

  $("#create_stream").click(function(e) {
    if (initialLocation) {
      var msg = "/create_room  new_room "+initialLocation.lat()+" "+initialLocation.lng();
      console.log(msg);
      GeoGossip.ws.send(msg);
    }
  });
});

