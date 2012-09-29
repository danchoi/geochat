var stops = [{"lat": 42.36, "lng": -71.07}, {"lat": 42.36, "lng": -71.08}, {"lat": 42.32, "lng": -71.08}
];

var stopCircles = [];

var initialLocation;

function randomlyPulse() {

};

$(document).ready(function() {
  var center;

  var latLng = new google.maps.LatLng(42.36, -71.08);
  var myOptions = {
    center: latLng,
    zoom: 12,
    mapTypeId: google.maps.MapTypeId.ROADMAP
  };

  var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);

  for (var i in stops) {
    var stopOptions;
    var stop; 
    stop = stops[i];
    console.log("stop: " + stop.lat);
    center = new google.maps.LatLng(stop.lat, stop.lng);
    stopOptions = {
      strokeColor: "blue",
      strokeOpacity: 0.6,
      strokeWeight: 4,
      fillColor: "#FFFFFF",
      fillOpacity: 0.1,
      map: map,
      center: center,
      radius: 180
    }
    stopCircle = new google.maps.Circle(stopOptions);
    stopCircles.push(stopCircle);
    attachEventHandlerToStop(stopCircle, stop);
  }

  function attachEventHandlerToStop(stopCircle, stop) {
    google.maps.event.addListener(stopCircle, 'mouseover', function() {
      console.log("Stop "+stop.stop_name+" touched");
      stopCircle.setOptions({strokeColor: "red", radius: 220, strokeWeight: 7});
    });
    google.maps.event.addListener(stopCircle, 'mouseout', function() {
      stopCircle.setOptions({strokeColor: "blue", radius: 180, strokeWeight: 4});
    });
  }


  randomlyPulse();


  if(navigator.geolocation) {
    browserSupportFlag = true;
    navigator.geolocation.getCurrentPosition(function(position) {
      initialLocation = new google.maps.LatLng(position.coords.latitude,position.coords.longitude);
      console.log("initial location!");
    }, function() {
      // console.log("No geolocation");
    });
  }


});

