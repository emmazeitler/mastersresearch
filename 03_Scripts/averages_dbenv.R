library(tidyverse)
library(lubridate)

db_data <- read_csv("02_Clean_data/dbenv_use.csv")
db_data$date <- mdy(db_data$date)
rm(list=setdiff(ls(), "db_data"))


spring_avgs <- db_data %>% 
  filter(burn_season == "Spring") %>% 
  group_by(burn_season, env_type) %>% 
  summarize(rem_no = mean(rem_no, na.rm=TRUE),
            rem_event = mean(rem_event, na.rm=TRUE),
            latency = mean(latency2, na.rm=TRUE))

summer_avgs <- db_data %>% 
  filter(burn_season == "Summer")%>% 
  group_by(burn_season, env_type) %>% 
  summarize(rem_no = mean(rem_no, na.rm=TRUE),
            rem_event = mean(rem_event, na.rm=TRUE),
            latency = mean(latency2, na.rm=TRUE))

fall_avgs <- db_data %>% 
  filter(burn_season == "Fall")%>% 
  group_by(burn_season, env_type) %>% 
  summarize(rem_no = mean(rem_no, na.rm=TRUE),
            rem_event = mean(rem_event, na.rm=TRUE),
            latency = mean(latency2, na.rm=TRUE))

winter_avgs <- db_data %>% 
  filter(burn_season == "Winter") %>%
  group_by(burn_season, env_type) %>% 
  summarize(rem_no = mean(rem_no, na.rm=TRUE),
            rem_event = mean(rem_event, na.rm=TRUE),
            latency = mean(latency2, na.rm=TRUE))


averages <- rbind(spring_avgs, summer_avgs)
averages <- rbind(averages, fall_avgs)
averages <- rbind(averages, winter_avgs)

ggplot(averages) +
  geom_point(aes(x=env_type, y=rem_no, color = env_type, size = 3)) +
  scale_color_manual(values=c("#d2601a", "#1d3c45"))+
  xlab("Environment Type") +
  ylab("Amount of Dung Removed") +
  facet_wrap(~burn_season)

ggplot(averages) +
  geom_point(aes(x=env_type, y=rem_event, color = env_type, size = 3)) +
  scale_color_manual(values=c("#d2601a", "#1d3c45"))+
  xlab("Environment Type") +
  ylab("Probability of Removal") +
  facet_wrap(~burn_season)

ggplot(averages) +
  geom_point(aes(x=env_type, y=latency, color = env_type, size = 3)) +
  scale_color_manual(values=c("#d2601a", "#1d3c45"))+
  xlab("Environment Type") +
  ylab("Latency Until Removal") +
  facet_wrap(~burn_season)

  

