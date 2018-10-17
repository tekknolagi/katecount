'use strict';

var color_cache = {};

var chart = new Chart('chart', {
  type: 'pie',
  data: {
    labels: [],
    datasets: [{
      backgroundColor: [],
      data: [],
    }],
  },
  options: {
    title: {
      display: false,
      text: 'Current breakdown of editors',
    }
  },
});

function color_of_editor (editor, tracked) {
  if (editor in color_cache) {
    return color_cache[editor];
  }

  // var eds = ["vi","vim","kate","gedit","atom","ed","emacs","subl","nano"];
  // var piyg = d3.scaleSequential(d3.interpolatePiYG);
  // var scale = d3.scaleSequential(function(t) {
  //   return d3.hsl(t * 360, 1, 0.5) + "";
  // });
  var scale = d3.scaleSequential(d3.interpolateRainbow);
  var color = scale(tracked.indexOf(editor)/tracked.length);
  color_cache[editor] = color;
  return color;

  var random_pastel =
    "hsl(" + 360 * Math.random() + ',' +
             (50 + 50 * Math.random()) + '%,' + 
             (60 + 10 * Math.random()) + '%)';

  color_cache[editor] = random_pastel;
  return random_pastel;
}

function make_pie_chart (alist, tracked) {
  chart.data.labels = [];
  chart.data.datasets[0].backgroundColor = [];
  chart.data.datasets[0].data = [];

  for (var i = 0; i < alist.length; i++) {
    var editor_name = alist[i][0];
    var editor_count = alist[i][1];
    var slice_color = color_of_editor(editor_name, tracked);

    chart.data.labels.push(editor_name);
    chart.data.datasets[0].backgroundColor.push(slice_color);
    chart.data.datasets[0].data.push(editor_count);
  }

  chart.update();
}

function get_data() {
  d3.request("/~mberns01/editors.json")
    .mimeType("application/json")
    .response(function (xhr) {
      return JSON.parse(xhr.responseText);
    })
    .get(function (editors) {
      // Handle weird errors like network errors
      if (editors == null) { return; }
      var tracked = editors.available;
      var $alist = Object.keys(editors.summary).map(function (key) {
        return [key, editors.summary[key]];
      });
      $alist.sort(function (a, b) {
        // Descending order
        return b[1] - a[1];
      });
      var $list = $("<ol>", {id: "editorRankings"});
      for (var i = 0; i < $alist.length; i++) {
        var kv = $alist[i];
        var editor = kv[0];
        var num_sessions = kv[1];
        var $li = $("<li>").text(editor + " (" + num_sessions.toString() + ")");
        $list.append($li);
      }
      make_pie_chart($alist, tracked);
      $("#rankings").html($list);
    });
}

var seconds = 1000;
get_data();
window.setInterval(get_data, 5*seconds);
