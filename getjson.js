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

function date_from_unix(unix_timestamp) {
    var date = new Date(unix_timestamp*1000);
    // Hours part from the timestamp
    var hours = date.getHours();
    // Minutes part from the timestamp
    var minutes = "0" + date.getMinutes();
    // Seconds part from the timestamp
    var seconds = "0" + date.getSeconds();

    // Will display time in 10:30:23 format
    return hours + ':' + minutes.substr(-2) + ':' + seconds.substr(-2);
}

// function sortByUsage(editors, summary) {
//     // l.sort(function (a, b) { return b[1] - a[1]; });
//     var eds = editors;
//     eds.sort(function(a, b) {
//         return summary[a] - summary[b] });
// }

function v(i) {
return i === undefined ? 0 : i;
}

function make_line_chart(history, tracked, summary) {
    var table = [];
    tracked.sort(function(a,b) {
        return v(summary[a]) - v(summary[b]);
    });
    table.push(["Time"].concat(tracked));
    var intv = 1;
    var timeSpanSeconds = history[history.length-1].timestamp - history[0].timestamp;
    for (var i = 0; i < history.length; i+=intv) {
        var dict_row = history[i];
        // var row = [date_from_unix(dict_row.timestamp)];
        var row = [''];
        for (var j = 0; j < tracked.length; j++) {
            var ed = tracked[j];
            var res = dict_row.summary[ed];
            row.push(res === undefined ? 0 : res);
        }
        table.push(row);
    }

    var data = google.visualization.arrayToDataTable(table);

    var options = {
      title: 'Editor Usage Over Time (' + timeSpanSeconds/60.0/60.0 + ' hrs)',
      hAxis: {title: 'Time',  titleTextStyle: {color: '#333'}},
      vAxis: {minValue: 0, direction: 1},
      isStacked: true,
      aggregationTarget: 'category',
    };

    var chart = new google.visualization.AreaChart(document.getElementById('areaChart'));
    chart.draw(data, options);
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
      // Descending order
      $alist.sort(function (a, b) { return b[1] - a[1]; });
      var $list = $("<ol>", {id: "editorRankings"});
      for (var i = 0; i < $alist.length; i++) {
        var kv = $alist[i];
        var editor = kv[0];
        var num_sessions = kv[1];
        var $li = $("<li>").text(editor + " (" + num_sessions.toString() + ")");
        $list.append($li);
      }
      make_pie_chart($alist, tracked);
      make_line_chart(editors.history, tracked, editors.summary);
      $("#rankings").html($list);
      $("#num-machines").html("(" + editors.num_nodes + ")");
    });
}

var seconds = 1000;
get_data();
window.setInterval(get_data, 5*seconds);
