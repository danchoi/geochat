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
    webSocket.send("/enter "+room.room_id );
  });
}

$(document).ready(function() {
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
    var msg = "/create new_room "+pos.lat()+" "+pos.lng();
    webSocket.send(msg);

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


});

