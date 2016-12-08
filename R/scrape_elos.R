
library(rvest)

url<-read_html("http://projects.fivethirtyeight.com/2016-nfl-predictions/")
selector_name<-".num.elo"
fnames<-html_nodes(x = url, css = selector_name) %>%
  html_text()

trs = test[grepl("num elo", test)]
teams = gsub('.*data-team="|">.*', "", trs)[2:NROW(test2)]
elos = data.frame(teams= teams, elo = fnames[2:NROW(fnames)])



