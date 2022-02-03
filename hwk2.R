library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")

siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

peaceH <- streamH[streamH$siteID == 2295637, ]

#plot date vs height with both lines and filled in points
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

floods <- full_join(streamH, # left table
                    siteInfo, # right table
                    by="siteID")
head(floods)

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarize(min(datetime))
head(max.cat)
  
  
