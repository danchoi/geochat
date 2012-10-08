WEB_SOCKET_SWF_LOCATION = "websocket_js/WebSocketMain.swf";

var map;
var rooms = [];
var clients = [];

var geogossip = {
  ws: null,
  currentLocation: null,
  tellServer: function (data) {
    geogossip.ws.send(JSON.stringify(data));
  },
  serverEvents: {
    UpdatedClient: function(data) {
      return; // STUB Geolocation is not accurate enough
      var c = data.client;
      var j = -1;
      for (var i = 0; i < clients.length; i++) {
        if (clients[i].roomId == c.roomId) {
            j = i; 
            break;
        }
      }
      if (j > -1) {
          clients.splice(j, 1); 
          layer.selectAll("svg.client").data(clients).exit().remove();
          clients.push(c); 
          layer.selectAll("svg.client").data(clients).enter();
      } else {
        clients.push(c);
      }
      window.overlay.draw();
    },
    UpdatedRoom: function(data) {
      var r = data.room;
      var j = -1;
      for (var i = 0; i < rooms.length; i++) {
        if (rooms[i].roomId === r.roomId) {
            j = i; // room exists at j
            break;
        }
      }
      if (j > -1 && r.numParticipants == 0) {
          rooms.splice(j,1); 
          layer.selectAll("svg.marker").data(rooms).exit().remove();
      } else if (j > -1 && rooms[j].numParticipants != r.numParticipants) {
          rooms.splice(j,1); 
          layer.selectAll("svg.marker").data(rooms).exit().remove();
          rooms.push(r);
          layer.selectAll("svg.marker").data(rooms).enter();
      } else if (j > -1  && rooms[j].numParticipants == r.numParticipants) {
          return;
      } else {
          rooms.push(r);
      }
      window.overlay.draw();
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
  createMap();
  geogossip.ws = new WebSocket(webSocketURL); 
  geogossip.ws.onopen = function(event){
    console.log("Connected to server");
  };

  geogossip.ws.onmessage = function(event){
    if (event.data.length > 0 && event.data[0] == '[') {
      var x  = JSON.parse(event.data);
      var messages = JSON.parse(event.data);
      for (var i = 0; i < messages.length; i++) {
        var message = messages[i];
        var f = geogossip.serverEvents[message.type];
        console.log("server: " + JSON.stringify(message));
        if (typeof f == 'function') {
          f(message);
        }
      }

      return;

    } else {  // receive a chat message 
        var message = event.data;
        message.prototype = messagePrototype;
        console.log(message.toString());
        $('#chatStream').append(ich.message(message));
        var liveRoomId = message.room_id;
        if (liveRoomId ) {
          x = roomCircles[liveRoomId];
          x.setOptions(geogossip.map.circle.chatActivityOptions);
          setTimeout( function() { x.setOptions(geogossip.map.circle.baseOptions); }, 100); 
        }
        $('#chatStream').animate({scrollTop: $('#chatStream').height()});

    }
  };
  
  geogossip.ws.onclose = function(event){
    $("#chatStream").append('<br>Connection closed');
  };
 


  // chat room widget 

  $("form#chat_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#message");
    textfield.val("");
  });

  $("form#nick_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#nickname");
    geogossip.tellServer({type: 'ChangeNickname', nickname: textfield.val()});
  });

  $("#create_stream").click(function(e) {
    if (geogossip.currentLocation) {
      var msg = "/create_room  new_room "+geogossip.currentLocation.lat()+" "+geoGossip.currentLocation.lng();
      console.log(msg);
      geogossip.ws.send(msg);
    }
  });


});




// d3

// layer.selectAll("svg.marker").data([]).exit().remove()

var overlay;
var layer;

function createMap() {
  
  var latLng = new google.maps.LatLng(42.36, -71.08);
  var myOptions = { center: latLng, zoom: 13, mapTypeId: google.maps.MapTypeId.ROADMAP };
  map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
  google.maps.event.addListener(map, 'click', function(event) { 
    geogossip.tellServer( {type: 'CreateRoom', lat: event.latLng.lat(), lng: event.latLng.lng()} );
  });

  overlay = new google.maps.OverlayView();
  overlay.setMap(map);
  overlay.onAdd = function() {

    layer = d3.select(this.getPanes().overlayMouseTarget).append("div").attr("class", "rooms");
    
    overlay.draw = function() {
      var projection = this.getProjection();

      var marker = layer.selectAll(".rooms .marker")
          .data(d3.entries(rooms))
          .on("mouseover", function(d, i) { 
            // console.log("mouseover on " + d.value.roomId) ;
            // geogossip.tellServer({type: 'JoinRoom', roomId: d.value.roomId});
          })
          .each(transform) // update existing markers
        .enter().append("svg:svg")
          .each(transform)
          .attr("class", "marker");

      // Add a circle.
      marker.append("svg:circle")
          .attr("r", 8.5)
          .attr("cx", 25)
          .attr("cy", 25);

      // Add a label.
      marker.append("svg:text")
          .attr("x", 33)
          .attr("dy", 28)
          .text(function(d) { return d.value.numParticipants; });

      function transform(d) {
        d1 = new google.maps.LatLng(d.value.latLng[0], d.value.latLng[1]);
        d2 = projection.fromLatLngToDivPixel(d1);
        return d3.select(this)
            .style("left", (d2.x-25) + "px")
            .style("top", (d2.y-25) + "px");
      }

      console.log("updating clients "+JSON.stringify(clients));
      var client = layer.selectAll(".rooms .client")
          .data(d3.entries(clients))
          .each(transform2) 
        .enter().append("svg:svg")
          .each(transform2)
          .attr("class", "client");
      client.append("svg:circle")
          .attr("r", 4.5)
          .attr("cx", 25)
          .attr("cy", 25);
      client.append("svg:text")
          .attr("x", 33)
          .attr("dy", 28)
          .text(function(d) { return d.value.nickName; });

      function transform2(d) {
        d1 = new google.maps.LatLng(d.value.clientLatLng[0], d.value.clientLatLng[1]);
        d2 = projection.fromLatLngToDivPixel(d1);
        return d3.select(this)
            .style("left", (d2.x-25) + "px")
            .style("top", (d2.y-25) + "px");
      }


    };
  }

  if(navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(function(pos) {
      geogossip.currentLocation = new google.maps.LatLng(pos.coords.latitude,pos.coords.longitude);
      geogossip.tellServer({type: 'LocationUpdated', lat: pos.coords.latitude, lng: pos.coords.longitude}); 
      $("#create_chat").show();
    }, function() { /* No geolocation */ });
  }
}

