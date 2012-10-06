WEB_SOCKET_SWF_LOCATION = "websocket_js/WebSocketMain.swf";

var map;

var geoGossip = {
  ws: null,
  currentLocation: null,
  rooms: {
    selected: null
  },
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
    this.select = function() {
      if (geoGossip.rooms.selected) 
        geoGossip.rooms.selected.unselect();
      this.selected = true;
      geoGossip.rooms.selected = this;
    },
    this.unselect = function() {
      this.selected = false;
      console.log("This Unselecting");
      this.circle.setOptions(this.baseOptions);
    }
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
        x.select();
        // geoGossip.ws.send( )
        // /enter "+roomInfo.room_id );
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
      var latLng = new google.maps.LatLng(42.36, -71.08);
      var myOptions = { center: latLng, zoom: 12, mapTypeId: google.maps.MapTypeId.ROADMAP };
      $("#create_chat").hide();
      map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
      google.maps.event.addListener(map, 'click', function(event) { 
        var msg = {clientEvent: 'roomCreated', lat: event.latLng.lat(), lng: event.latLng.lng()}
        geoGossip.ws.send(JSON.stringify(msg));
      });
      if(navigator.geolocation) {
        // var browserSupportFlag = true;
        navigator.geolocation.getCurrentPosition(function(pos) {
          geoGossip.currentLocation = new google.maps.LatLng(pos.coords.latitude,pos.coords.longitude);
          $("#create_chat").show();
        }, function() { /* No geolocation */ });
      }
    },
  },
  serverEvents: {
    roomCreated: function(roomInfo) {
      console.log("creating room: "+ JSON.stringify(roomInfo));
      var center = new google.maps.LatLng(roomInfo.lat, roomInfo.lng);
      var circle = new google.maps.Circle(geoGossip.map.circle.drawOptions(center));
      var room = new geoGossip.Room(circle, roomInfo);
      geoGossip.rooms[roomInfo.roomId] = room;
    },
    roomsList: function(rooms) {
      drawRooms(rooms);
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
  var webSocketURL = 'ws://localhost:9160'; // : 'ws://poddb.com:9394';
  geoGossip.map.createMap();
  geoGossip.ws = new WebSocket(webSocketURL); 
  geoGossip.ws.onopen = function(event){
    $('#chatStream').append('<br>Connected to the server');
    var msg = {clientEvent: 'sesssionStarted'};
  };

  geoGossip.ws.onmessage = function(event){
    if (event.data.length > 0) {
      var x = JSON.parse(event.data);
      var serverEvent = geoGossip.serverEvents[x.serverEvent];
      if (typeof serverEvent == 'function') {
        // dynamically dispatch to one of the geoGossip.serverEvents
        serverEvent(x);
      }
      return;


      if (event.data[0] == '{') {  // We have a JSON payload and should parse it
       var x;
       console.log(event.data);
       var data = JSON.parse(event.data);
       // rooms list
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
          x.setOptions(geoGossip.map.circle.chatActivityOptions);
          setTimeout( function() { x.setOptions(geoGossip.map.circle.baseOptions); }, 100); 
        }
        $('#chatStream').animate({scrollTop: $('#chatStream').height()});
     }
    }
  };
  
  geoGossip.ws.onclose = function(event){
    $("#chatStream").append('<br>Connection closed');
  };
 


  // chat room widget 

  $("form#chat_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#message");
    geoGossip.ws.send(textfield.val());
    textfield.val("");
  });

  $("form#nick_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#nickname");
    geoGossip.ws.send("/nick " + textfield.val());
  });

  $("#create_stream").click(function(e) {
    if (geoGossip.currentLocation) {
      var msg = "/create_room  new_room "+geoGossip.currentLocation.lat()+" "+geoGossip.currentLocation.lng();
      console.log(msg);
      geoGossip.ws.send(msg);
    }
  });


});




// d3 test
//


function drawRooms(data) {
  console.log("drawRooms");
  var overlay = new google.maps.OverlayView();

  // Add the container when the overlay is added to the map.
  overlay.onAdd = function() {
    var layer = d3.select(this.getPanes().overlayLayer).append("div")
        .attr("class", "rooms");

    // Draw each marker as a separate SVG element.
    // We could use a single SVG, but what size would it have?
    overlay.draw = function() {
      var projection = this.getProjection();

      console.log("projection "+JSON.stringify(data.rooms));
      var marker = layer.selectAll("svg")
          .data(d3.entries(data.rooms))
          .each(transform) // update existing markers
        .enter().append("svg:svg")
          .each(transform)
          .attr("class", "marker");


      // Add a circle.
      marker.append("svg:circle")
          .attr("r", 4.5)
          .attr("cx", 25)
          .attr("cy", 25);

      // Add a label.
      marker.append("svg:text")
          .attr("x", 33)
          .attr("dy", 28)
          .text(function(d) { return d.key; });

      function transform(d) {
        console.log(JSON.stringify(d));
        d = new google.maps.LatLng(d.value.lat, d.value.lng);
        d = projection.fromLatLngToDivPixel(d);
        return d3.select(this)
            .style("left", (d.x-25) + "px")
            .style("top", (d.y-25) + "px");
      }
    };
  };

  // Bind our overlay to the map…
  overlay.setMap(map);
};

