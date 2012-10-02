WEB_SOCKET_SWF_LOCATION = "websocket_js/WebSocketMain.swf";

var map;

var GeoGos = {
  ws: null,
  currentLocation: null,
  rooms: {},
  Room: function (circle, roomInfo) { // a constructor
    this.circle =  circle;
    this.info = roomInfo;
    this.baseOptions =  {strokeColor: "blue", radius: 180, strokeWeight: 4};
    this.selectedOptions = {strokeColor: "green", radius: 180, strokeWeight: 4};
    this.hoverOptions = {strokeColor: "red", radius: 220, strokeWeight: 7};
    this.chatActivityOptions = {strokeColor: "red", radius: 220, strokeWeight: 7};
    this.selected = false;  
    this.mouseoverOpts = function() { return (this.hoverOptions); };
    this.mouseoutOpts = function() { return (this.selected ? this.selectedOptions : this.baseOptions); };
    this.flashOpts = function() { return (this.chatActivityOptions); };
    google.maps.event.addListener(circle, 'mouseover', function(x) {
      return function() {
        return x.circle.setOptions(x.mouseoverOpts());
      }
    }(this));
    google.maps.event.addListener(circle, 'mouseout', function(x) {
       return function() {
        x.circle.setOptions(x.mouseoutOpts());
      }
    }(this));
    google.maps.event.addListener(circle, 'click', function(x) {
      return function() {
        console.log("Room clicked: "+ x.info.roomId); 
        // GeoGos.ws.send( )
        //  "/enter "+roomInfo.room_id );
      };
    }(this));
  },
  map: {
    circle: {
      drawOptions: function(center) {
        return {strokeColor: "blue", strokeOpacity: 0.6, strokeWeight: 4, fillColor: "#FFFFFF", fillOpacity: 0.1, map: map, center: center, radius: 180};
      }
    },
    createMap: function() {
      var center;
      var latLng = new google.maps.LatLng(42.36, -71.08);
      var myOptions = { center: latLng, zoom: 12, mapTypeId: google.maps.MapTypeId.ROADMAP };
      $("#create_chat").hide();
      map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
      google.maps.event.addListener(map, 'click', function(event) { 
        var msg = {clientEvent: 'roomCreated', lat: event.latLng.lat(), lng: event.latLng.lng()}
        GeoGos.ws.send(JSON.stringify(msg));
      });
      if(navigator.geolocation) {
        browserSupportFlag = true;
        navigator.geolocation.getCurrentPosition(function(pos) {
          GeoGos.currentLocation = new google.maps.LatLng(pos.coords.latitude,pos.coords.longitude);
          $("#create_chat").show();
        }, function() { /* No geolocation */ });
      }
    },
  },
  serverEvents: {
    roomCreated: function(roomInfo) {
      console.log("creating room: "+ JSON.stringify(roomInfo));
      var center = new google.maps.LatLng(roomInfo.lat, roomInfo.lng);
      var circle = new google.maps.Circle(GeoGos.map.circle.drawOptions(center));
      var room = new GeoGos.Room(circle, roomInfo);
      GeoGos.rooms[roomInfo.roomId] = room;
    }

  }
};





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
  GeoGos.map.createMap();
  GeoGos.ws = new WebSocket(webSocketURL); 
  GeoGos.ws.onopen = function(event){
    $('#chatStream').append('<br>Connected to the server');
    GeoGos.ws.send("/rooms");
  };

  GeoGos.ws.onmessage = function(event){
    if (event.data.length > 0) {
      var x = JSON.parse(event.data);
      var serverEvent = GeoGos.serverEvents[x.serverEvent];
      if (typeof serverEvent == 'function') {
        // dynamically dispatch to one of the GeoGos.serverEvents
        serverEvent(x);
      }


      if (event.data[0] == '{') {  // We have a JSON payload and should parse it
       var x;
       console.log(event.data);
       var data = JSON.parse(event.data);
       // rooms list
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
           // TODO CHANGE THIS
           roomCircles.push(roomCircle);
           attachEventsToCircles(roomCircle, room);
         }
       } 
     } else {  // receive a chat message 
        
        var message = event.data;
        message.prototype = messagePrototype;
        console.log(message.toString());
        $('#chatStream').append(ich.message(message));
        var liveRoomId = message.room_id;
        if (liveRoomId ) {
          console.log("live room id: "+liveRoomId);
          // TODO CHANGE
          x = roomCircles[liveRoomId];
          console.log(x);
          x.setOptions(GeoGos.map.circle.chatActivityOptions);
          setTimeout( function() { x.setOptions(GeoGos.map.circle.baseOptions); }, 100); 
        }
        $('#chatStream').animate({scrollTop: $('#chatStream').height()});
     }
    }
  };
  
  GeoGos.ws.onclose = function(event){
    $("#chatStream").append('<br>Connection closed');
  };
 


  // chat room widget 

  $("form#chat_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#message");
    GeoGos.ws.send(textfield.val());
    textfield.val("");
  });

  $("form#nick_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#nickname");
    GeoGos.ws.send("/nick " + textfield.val());
  });

  $("#create_stream").click(function(e) {
    if (GeoGos.currentLocation) {
      var msg = "/create_room  new_room "+GeoGos.currentLocation.lat()+" "+GeoGos.currentLocation.lng();
      console.log(msg);
      GeoGos.ws.send(msg);
    }
  });
});

