
function view_sentiments() {
  
  var checked = document.getElementById('sentiments')
  var wordcloud = document.getElementById('wordcloudplot')
  var sentimentplots = document.getElementById('sentimentplots')
  
  
  if (checked == true) {
    document.getElementById('comparisoncloud').style.display = "none"
  }
}