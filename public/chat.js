WEB_SOCKET_SWF_LOCATION = "websocket_js/WebSocketMain.swf";

var map;
var myMapBounds;
var myClientId;
var rooms = [];
var roomsMap = {}
var clients = [];
var clientsMap = {}
var websocket;
var tellServer = function (data) {
  websocket.send(JSON.stringify(data));
};

var ServerEvents = {
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
    } else if (roomsMap[r.roomId] && (roomsMap[r.roomId].numParticipants !== r.numParticipants)) {
        roomsMap[r.roomId] = r;
        $("#room-"+r.roomId+" text.numParticipants").text(r.numParticipants);
    } else {
        console.log("Adding room node "+r.roomId);
        rooms.push(r);
        roomsMap[r.roomId] = r;
        window.overlay.draw();
    }

    if (data.change.client && data.change.client[0] === myClientId && data.change.type === "EnterRoom") {
        d3.selectAll(".marker").attr("class", "marker");
        d3.select("#room-"+r.roomId).attr("class", "marker selected");
    }
  },
  Broadcast: function(data) {
    console.log(data);
    var roomId = data.roomId;
    d3.select(".rooms #room-"+roomId+" circle").style("stroke", "white").transition().style("stroke", "black");
    d3.select(".rooms #room-"+roomId+" text.chat").text(data.client[1] + ": " + data.text);
  }
}

function tellservermybounds() {
  if (websocket.readyState == 1) {
    var bounds = map.getBounds();
    var myMapBounds = {type: "MapBoundsUpdated", 
                latSW:  bounds.getSouthWest().lat(),
                lngSW:  bounds.getSouthWest().lng(),
                latNE:  bounds.getNorthEast().lat(),
                lngNE:  bounds.getNorthEast().lng() }
    tellServer(myMapBounds);
  }
}


$(document).ready(createMap);

function startWS() {
  var webSocketURL = 'ws://localhost:9160/ws'; 
  websocket = new WebSocket(webSocketURL); 
  websocket.onopen = function(event){
    console.log("Connected to server");
    tellservermybounds(myMapBounds);
  };
  websocket.onmessage = function(event){
    var x = JSON.parse(event.data);
    if (Object.prototype.toString.call( x) === '[object Array]') {
      for (var i = 0, j = x.length; i < j; i++) {
        var message = x[i];
        var f = ServerEvents[message.type];
        console.log("ServerEvent: " + event.data);
        if (typeof f == 'function') { 
          f(message) 
        } else { 
          console.log("Unrecognized event: "+event.data);
        }
      }
    } else if (x['type'] === 'Handshake') {
        console.log("Handshake: " + event.data);
        myClientId = x['clientId'];
    } else {  // receive a chat message 
      console.log("Unparseable message: "+event.data);
    }
  };
  
  websocket.onclose = function(event){
    $("#chatStream").append('<br>Connection closed');
  };
 
  $("form#chat_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#message");
    tellServer({type: 'PostMessage', content: textfield.val()});
    textfield.val("");
  });

  $("form#nick_form").submit(function(e){
    e.preventDefault();
    var textfield = $("#nickname");
    tellServer({type: 'ChangeNickname', nickname: textfield.val()});
  });

};




// d3

// layer.selectAll("svg.marker").data([]).exit().remove()

var overlay;
var layer;

function createMap() {
  
  var latLng = new google.maps.LatLng(42.36, -71.08);
  var myOptions = { center: latLng, zoom: 13, mapTypeId: google.maps.MapTypeId.ROADMAP };
  map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
  google.maps.event.addListener(map, 'click', function(event) { 
    tellServer( {type: 'CreateRoom', lat: event.latLng.lat(), lng: event.latLng.lng()} );
  });

  overlay = new google.maps.OverlayView();
  overlay.setMap(map);
  google.maps.event.addListener(map, "bounds_changed", tellservermybounds); 
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

      marker.append("svg:circle")
          .attr("r", 14.5)
          .attr("cx", 25)
          .attr("cy", 25);
          //.on("mouseover", function(d, i) { d3.select(this).style("fill", "yellow")})
          //.on("mouseout", function(d, i) { d3.select(this).style("fill", "red")})

      marker.append("svg:text")
          .attr("class", "numParticipants")
          .attr("x", 22)
          .attr("dy", 29)
          .text(function(d) { return d.value.numParticipants; });

      marker.append("svg:text")
          .attr("class", "chat")
          .attr("x", 22)
          .attr("dy", 60)
          .text("chat");

      function transform(d) {
        d1 = new google.maps.LatLng(d.value.latLng[0], d.value.latLng[1]);
        d2 = projection.fromLatLngToDivPixel(d1);
        return d3.select(this)
            .style("left", (d2.x-25) + "px")
            .style("top", (d2.y-25) + "px");
      }
    };
    startWS();
  }

}

