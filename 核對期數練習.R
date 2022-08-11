library(dplyr)
set.seed(123L)
dates <- seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by = "1 day")
ptnamesMID <- data.frame(ID = 1:10, Measure_date = sample(dates, 10L))
CTDB <- data.frame(ID = sample.int(10, 30L, TRUE), VISIT_DATE = sample(dates, 30L, TRUE))

library(data.table)
# coerce to data.table and append join columns to preserve the original columns 
setDT(CTDB)[, join_date := VISIT_DATE]
setDT(ptnamesMID)[, join_date := Measure_date]
# rolling join
CTDB[ptnamesMID, on = .(ID, join_date), roll = "nearest"]


#left_join(CTDB , ptnamesMID , by = c("ID","join_date") , roll = "nearest")

CTDB[ptnamesMID, on = .(ID), roll = "nearest"]

