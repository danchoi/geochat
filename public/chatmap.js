$(document).ready(function() {


  var latLng = new google.maps.LatLng(42.36, -71.08);
  var myOptions = {
    center: latLng,
    zoom: 12,
    mapTypeId: google.maps.MapTypeId.ROADMAP
  };

  var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);

  /*   / stops
  for (var i in stops) {
    var stop = stops[i];
    var center = new google.maps.LatLng(stop.stop_lat, stop.stop_lon);
    var stopOptions = {
      strokeColor: (stop.selected ? "blue" : "#FF0000"),
      strokeOpacity: 0.8,
      strokeWeight: 2,
      fillColor: "#FFFFFF",
      fillOpacity: 1,
      map: map,
      center: center,
      radius: (stop.selected ? 50 : 20)
    }
    stopCircle = new google.maps.Circle(stopOptions);
  }

  */
});

