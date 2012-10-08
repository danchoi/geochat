WEB_SOCKET_SWF_LOCATION = "websocket_js/WebSocketMain.swf";

var map;
var rooms = [];
var roomsMap = {}
var clients = [];
var clientsMap = {}

var geogossip = {
  ws: null,
  currentLocation: null,
  tellServer: function (data) {
    geogossip.ws.send(JSON.stringify(data));
  },
  serverEvents: {
    UpdatedRoom: function(data) {
      var r = data.room;
      if (r.numParticipants === 0) {
          console.log("Remove room "+r.roomId);
          for (var i = 0, j = rooms.length; i < j; i++) {
            if (rooms[i].roomId === r.roomId) {
              rooms.splice(i,1);
              break;
            }
          }
          $("#room-"+r.roomId).remove();
          delete roomsMap[r.roomId];
      } else if (roomsMap[r.roomId] && (roomsMap[r.roomId].numParticipants != r.numParticipants)) {
          roomsMap[r.roomId] = r;
          var x = d3.select("#room-"+r.roomId).__data__.value.numParticipants;
          console.log("current "+x);
      } else {
          rooms.push(r);
          roomsMap[r.roomId] == r;
          window.overlay.draw();
      }
    },
    Broadcast: function(data) {
      var roomId = data.roomId;
      d3.select(".rooms #room-"+roomId+" circle").style("fill", "blue").transition().style("fill", "red");
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
    geogossip.tellServer({type: 'PostMessage', content: textfield.val()});
    textfield.val("");
  });

  $("form#nick_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#nickname");
    geogossip.tellServer({type: 'ChangeNickname', nickname: textfield.val()});
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
          .each(transform) // update existing markers
        .enter().append("svg:svg")
          .each(transform)
          .attr("class", "marker")
          .attr("id", function(d,i) { return ("room-" + d.value.roomId) });

      // Add a circle.
      marker.append("svg:circle")
          .attr("r", 14.5)
          .attr("cx", 25)
          .attr("cy", 25)
          //.on("mouseover", function(d, i) { d3.select(this).style("fill", "yellow")})
          //.on("mouseout", function(d, i) { d3.select(this).style("fill", "red")})
          .on("click", function(d, i) { 
            console.log("click "+d.value.roomId);
            //d3.select(this).style("fill", "yellow");
            d3.select("#room-"+d.value.roomId).attr("class", "selected");
            

          });


      // Add a label.
      marker.append("svg:text")
          .attr("x", 22)
          .attr("dy", 28)
          .text(function(d) { return d.value.numParticipants; });

      function transform(d) {
        d1 = new google.maps.LatLng(d.value.latLng[0], d.value.latLng[1]);
        d2 = projection.fromLatLngToDivPixel(d1);
        return d3.select(this)
            .style("left", (d2.x-25) + "px")
            .style("top", (d2.y-25) + "px");
      }
    };
  }

  if(navigator.geolocation) {
    return; // STUB THIS OUT
    navigator.geolocation.getCurrentPosition(function(pos) {
      geogossip.currentLocation = new google.maps.LatLng(pos.coords.latitude,pos.coords.longitude);
      geogossip.tellServer({type: 'LocationUpdated', lat: pos.coords.latitude, lng: pos.coords.longitude}); 
      $("#create_chat").show();
    }, function() { /* No geolocation */ });
  }
}

