rm(list = ls())
library(tidyverse)
library(janitor)
library(readr)
library(dplyr)
library(skimr)
library(DataExplorer)
library(lubridate)
library(ggplot2)
library(geofacet)
library(gganimate)
library(ggthemes)
library(stringr)
library(fpp2)
library(patchwork)
library(viridis)
library(gghighlight)
library(PerformanceAnalytics)
# Loading csv file
ldh_m <- read_csv("../input/housing-in-london/housing_in_london_monthly_variables.csv")
head(ldh_m)
colnames(ldh_m)
n_unique(ldh_m$code)
min(ldh_m$date)
max(ldh_m$date)
# Find unique areas
unique(ldh_m$area)
# Count observations in each area
data.frame(table(ldh_m$area))
# Find duplicates
sum(duplicated(ldh_m))
arrange(unique(ldh_m[c("code", "area")]))
ldh_m %>% 
  filter(area=="enfield") %>% 
  arrange(desc(code, date)) # replicate with wrong code: delete entire row
ldh_m %>% 
  filter(area=="hackney") %>% 
  arrange(desc(code, date)) # replicate with wrong code: delete entire row
ldh_m %>% 
  filter(area=="south east") %>% 
  arrange(code, date) # replicate with wrong code: delete entire row
ldh_m %>% 
  filter(area=="tower hamlets") %>% 
  arrange(code, date)  # replicate with wrong code: delete entire row
str(ldh_m)
introduce(ldh_m)
# View missing value distribution 
plot_missing(ldh_m)
# Remove replicates with wrong codes
m_new <- ldh_m[!(ldh_m$date == "1998-04-01" & ldh_m$area =="south east" & ldh_m$code == "E09000012") & !(ldh_m$date == "1998-04-01" & ldh_m$area =="hackney" & ldh_m$code == "E12000008") & !(ldh_m$date == "1996-02-01" & ldh_m$area =="enfield" & ldh_m$code == "E09000030") & !(ldh_m$date == "1996-02-01" & ldh_m$area =="tower hamlets" & ldh_m$code == "E09000010"),]

# Exclude data date 2020-01-01 as the year is incomplete
m_new <- m_new %>% filter(date != "2020-01-01") %>% select(-"borough_flag")

# Filling missing values (imputation method)
hsold_median = median(ldh_m$houses_sold, na.rm=TRUE)
ldh_m$houses_sold[is.na(ldh_m$houses_sold)] = hsold_median
# Find unique codes (post-cleaning & transforming)
unique(m_new$code)
# Count how many boroughs are available, code starts with "E09"
boroughs <- m_new %>% filter(str_detect(m_new$code, "E09")) 
n_unique(boroughs$code)
unique(boroughs$code)

# Count how many regions are available, code starts with "E12"
regions <- m_new %>% filter(str_detect(m_new$code, "E12")) 
n_unique(regions$code)
unique(regions$code)

# Check if there are inner and outer London data available, code starts with "E13"
greater_ld <- m_new %>% filter(str_detect(m_new$code, "E13")) 
n_unique(greater_ld$code)
unique(greater_ld$code)
# Add a new column defining inner / outer boroughs
zoneld <- m_new %>% 
  mutate(zone = case_when(
    area %in% c("camden", "greenwich", "hackney", "hammersmith and fulham", "islington",
                "kensington and chelsea", "lambeth", "lewisham", "southwark", "tower hamlets", "wandsworth", "westminster") ~ "inner", 
    area %in% c("barking and dagenham", "barnet", "bexley","brent","bromley", "croydon", "ealing", "enfield", "haringey", "harrow", "havering", "hillingdon", "hounslow", "kingston upon thames", "merton", "newham", "redbridge", "richmond upon thames","sutton", "waltham forest") ~ "outer",
    TRUE ~ "other"))

# Add new column with extract year value from date 
zoneld <-  zoneld %>% 
  mutate(date = ymd(date), year=as.numeric(substr(date, 1, 4)))
#Plot histogram of houses price over years
ggplot(m_new,aes(x=average_price))+ 
  geom_histogram(colour = "darkgray", fill = "white", bins = 40)+
  labs(xlab = "Steps", ylab = "Count", title = "Histogram of Houses Price Over Years")+
  geom_vline(xintercept = mean(m_new$average_price, na.rm = TRUE), 
             color = "red", show.legend = TRUE, size = 1.5)+
  geom_vline(xintercept = median(m_new$average_price, na.rm = TRUE), 
             color = "blue", show.legend = TRUE, size = 1.5)+
  scale_color_manual(name = "statistics", values = c(mean = "red", median = "blue"))+
  scale_x_continuous(labels = scales::comma)
options(repr.plot.width = 14, repr.plot.height = 7)
# Correlation among variables
corr <- m_new[, -c(1,2,4)]
chart.Correlation(corr, histogram=TRUE, pch=19)
options(repr.plot.width = 10, repr.plot.height = 6)
# Plot inner vs outer London 
m_new <- m_new %>% 
  mutate(date = ymd(date),
         year=as.numeric(substr(date, 1, 4)))
m_new %>% 
  filter(str_detect(code, c("E13", "E92"))) %>% 
  group_by(area, date) %>% 
  summarise(mean_price = mean(average_price), .groups = "drop") %>% 
  ggplot(aes(x=date, y=mean_price, group=area, colour=area)) +
  geom_line(lwd=.8)+
  theme_set(theme_minimal())+
  labs(x= NULL, y="Average price (£)", title = "Average Houses Price London Vs England")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# Plot number of houses sold across regions
p8 <- m_new %>% 
  filter(str_detect(code, "E12")) %>% 
  group_by(area, date) %>% 
  summarise(sum_sold = sum(houses_sold, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x=reorder(area, sum_sold), y=sum_sold, group=area, fill=area)) +
  geom_boxplot(show.legend = FALSE)+
  theme_set(theme_minimal())+
  labs(x=NULL, y="Number of houses sold", title = "Number of Houses Sold Across Regions")+
  scale_y_continuous(labels = scales::comma)