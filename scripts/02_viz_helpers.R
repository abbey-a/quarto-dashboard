# JavaScript tooltip formatters for echarts4r

tooltip_weekly_events <- htmlwidgets::JS("
  function(params) {
    function getY(p) {
      var v = p.value;
      if (Array.isArray(v)) return Number(v[v.length - 1]);
      return Number(v);
    }

    var weekStart = params[0].axisValue;
    var start = new Date(weekStart + 'T00:00:00');
    var end = new Date(start);
    end.setDate(start.getDate() + 6);
    var fmt = {month: 'short', day: 'numeric', year: 'numeric'};
    var header = '<strong>' + start.toLocaleDateString('en-US', fmt) +
      ' - ' + end.toLocaleDateString('en-US', fmt) + '</strong><br/>';
    var body = '';
    var hasIncompleteCases = params.some(function(p) {
      return p.seriesName === 'Cases | incomplete week' && getY(p) > 0;
    });

    params.forEach(function(p) {
      if (p.seriesName === 'Cases | incomplete week' && !hasIncompleteCases) return;
      if (p.seriesName === 'Cases | specimen date' && hasIncompleteCases) return;
      body += p.marker + ' ' + p.seriesName + ': <strong>' +
        getY(p).toLocaleString() + '</strong><br/>';
    });

    return header + body;
  }
")

tooltip_age_severity <- htmlwidgets::JS("
  function(params) {
    function getY(p) {
      var v = p.value;
      if (Array.isArray(v)) return Number(v[v.length - 1]);
      return Number(v);
    }

    var header = '<strong>Age ' + params[0].axisValue + '</strong><br/>';
    var body = '';

    params.forEach(function(p) {
      var detail = p.name || '';
      body += p.marker + ' ' + p.seriesName + ': <strong>' +
        getY(p).toFixed(1) + '%</strong><br/>' +
        '<span style=\"color:#5f6b76\">' + detail + '</span><br/>';
    });

    return header + body;
  }
")

axis_date_formatter <- htmlwidgets::JS("
  function(value) {
    var d = new Date(value + 'T00:00:00');
    return d.toLocaleDateString('en-US', {month: 'short', year: '2-digit'});
  }
")

percent_axis_formatter <- htmlwidgets::JS("
  function(value) { return value + '%'; }
")

dashboard_colors <- list(
  ink = "#243442",
  muted = "#667580",
  grid = "#DCE3E8",
  cases = "#2878A5",
  admissions = "#D28B21",
  deaths = "#B64B4B",
  provisional = "rgba(102, 117, 128, 0.12)"
)
