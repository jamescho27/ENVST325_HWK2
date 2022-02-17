library(dplyr)
library(lubridate)
library(ggplot2)


# initialize data
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


# QUESTION 1plot all rivers height vs time
floods %>% 
  ggplot(aes(dateF, gheight.ft, color = names)) + geom_point()

# QUESTION 2create data frames with the first data of each flood level
first.action <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= action.ft) %>%
  summarize(min(dateF))
colnames(first.action) <- c('name', "first_action")

first.flood <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarize(min(dateF))
colnames(first.flood) <- c('name', "first_flood")

first.moderate <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= moderate.ft) %>%
  summarize(min(dateF))
colnames(first.moderate)<- c('name', "first_moderate")

first.major <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarize(min(dateF))
colnames(first.major)<- c('name', "first_major")
# merge all said data frame

flood_dates <- first.action %>% 
  full_join(first.flood, by = 'name') %>%
  full_join(first.moderate, by = 'name') %>%
  full_join(first.major, by = 'name')

# find the time difference from flood level to flood level
flood_dates <- mutate(flood_dates, action_to_flood_time = difftime(first_flood, first_action, units = "hours"),
                      flood_to_moderate_time = difftime(first_moderate, first_flood, units = "hours"),
                      moderate_to_major_time = difftime(first_major, first_moderate, units = "hours"))
# QUESTION 3
# find the exceedance of the major flood level
floods <- mutate(floods, major.exceedance = gheight.ft - major.ft)

max.flood <- floods %>%
  group_by(names) %>%
  summarize(max(exceedance))

