library(data.table)
library(streamgraph)
library(htmlwidgets)

jobloss <- fread("stateunemployment_longrate.csv")

head(jobloss$month)
jobloss$month <- as.Date(jobloss$month,format='%m/%d/%y') #had to format csv to work with asDate for some oddreason.


pp <- streamgraph(jobloss, "state", "rate", "month", offset ="zero", interpolate ="cardinal", height="300px", width="1000px") %>%
    sg_legend(show=TRUE, label="states: ")
pp 
saveWidget(pp, file=paste0(getwd(), "/streamgraphBasic.html"))
