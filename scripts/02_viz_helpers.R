# JavaScript customizations for echarts4r graphs

tooltip_weekly_cases <- htmlwidgets::JS("
      function(params){
        return('<strong>' + params.name + 
                '</strong><br />Week Starting: ' + params.value[0] + 
                '<br />Number of Cases: ' + params.value[1]) 
                }
    ")

tooltip_race_pie <- htmlwidgets::JS("
      function(params) {
          return '<strong>' + params.name + '</strong><br/>' +
               'Cases: ' + params.value.toLocaleString() + '<br/>' +
               'Percentage: ' + params.percent.toFixed(2) + '%';
      }
    ")

tooltip_hospitalizations <- htmlwidgets::JS("
  function(params){
    var p = params[0];

    // --- Parse date ---
    var d = new Date(p.axisValue);
    var dateLabel = d.toISOString().slice(0,10); // YYYY-MM-DD

    // --- CDC epi week calculation ---
    // CDC week: week containing Jan 4 is week 1
    var year = d.getUTCFullYear();
    var jan4 = new Date(Date.UTC(year, 0, 4));
    var firstWeekStart = new Date(jan4);
    firstWeekStart.setUTCDate(jan4.getUTCDate() - jan4.getUTCDay() + 1);

    var diff = d - firstWeekStart;
    var cdcWeek = Math.floor(diff / (7 * 24 * 60 * 60 * 1000)) + 1;
    if (cdcWeek < 1) cdcWeek = 1;

    // --- Extract y value ---
    var y = Array.isArray(p.value) ? p.value[1] : p.value;
    y = Number(y);

    return '<strong>CDC Week ' + cdcWeek + '</strong><br/>' +
           'Week Starting: ' + dateLabel + '<br/>' +
           'Hospitalization Rate: ' + y.toFixed(1) + '%';
  }
")

tooltip_deaths <- htmlwidgets::JS("
    function(params){

      function getY(p){
        var v = p.value;
        if (Array.isArray(v)) return v[v.length - 1]; // y
        return v;
      }

      var bar  = params.find(p => p.seriesType === 'bar');
      var line = params.find(p => p.seriesType === 'line');

      // CDC Week label comes from bind -> params[i].name
      var cdcWeekLabel = params[0].name; // e.g., 'CDC Week 31'

      // Week start date from axis value or from value[0]
      var weekStart =
        (params[0].axisValueLabel || params[0].axisValue) ||
        (Array.isArray(params[0].value) ? params[0].value[0] : params[0].name);

      var deaths = bar  ? getY(bar)  : null;
      var cfr    = line ? getY(line) : null;

      var deathsTxt = (deaths === null || deaths === undefined) ? 'NA' : deaths;

      var cfrTxt = (cfr === null || cfr === undefined || isNaN(cfr))
        ? 'NA'
        : (Number(cfr) * 100).toFixed(2) + '%';

      return '<strong>' + cdcWeekLabel + '</strong>'
        + '<br/>Week Starting: ' + weekStart
        + '<br/>Number of Deaths: ' + deathsTxt
        + '<br/>Case Fatality Rate: ' + cfrTxt;
    }
  ")

# color palette for pie chart
flatly_pie <- c(
  "#4E79A7",  # muted blue (anchor)
  "#76B7B2",  # teal
  "#59A14F",  # green
  "#EDC948",  # soft gold (NOT bright yellow)
  "#AF7AA1",  # muted purple
  "#F28E2B",  # softened orange
  "#9C755F",  # warm gray-brown
  "#BAB0AC"   # neutral gray
)