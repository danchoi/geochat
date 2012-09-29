var stops = [{"lat": 42.36, "lng": -71.07}, {"lat": 42.36, "lng": -71.08}, {"lat": 42.32, "lng": -71.08}
];

$(document).ready(function() {
  var stopOptions;
  var center;
  var stop;


  var latLng = new google.maps.LatLng(42.36, -71.08);
  var myOptions = {
    center: latLng,
    zoom: 12,
    mapTypeId: google.maps.MapTypeId.ROADMAP
  };

  var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);

  for (var i in stops) {
    stop = stops[i];
    console.log("stop: " + stop.lat);
    center = new google.maps.LatLng(stop.lat, stop.lng);
    stopOptions = {
      strokeColor: (stop.selected ? "blue" : "#FF0000"),
      strokeOpacity: 0.6,
      strokeWeight: 4,
      fillColor: "#FFFFFF",
      fillOpacity: 0.1,
      map: map,
      center: center,
      radius: 180
    }
    stopCircle = new google.maps.Circle(stopOptions);
  }

});

