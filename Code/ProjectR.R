#Installing Packages
install.packages('DT')
install.packages('ggthemes')
install.packages("wesanderson")
install.packages("ggprel")
install.packages("kableExtra")

#Importing the libraries 
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(wesanderson)
library(stringr)
library(RColorBrewer)
library(ggfortify)
library(ggrepel)
library(kableExtra)
library(gridExtra)

#CLEANING AND WRANGLING

#Getting the data in r
apr <- read.csv("uber-raw-data-apr.csv")
may <- read.csv("uber-raw-data-may.csv")
jun <- read.csv("uber-raw-data-jun.csv")
jul <- read.csv("uber-raw-data-jul.csv")
aug<- read.csv("uber-raw-data-aug.csv")
sep <- read.csv("uber-raw-data-sep.csv")

#Using R bind to combine it into one big df
df_uber <- rbind(apr,may, jun, jul, aug, sep)

#Formatting the df_Uber
df_uber$Date.Time <- as.POSIXct(df_uber$Date.Time, format = "%m/%d/%Y %H:%M:%S")
df_uber$Date.Time <- ymd_hms(df_uber$Date.Time)

#Using lubridate package and further massaging the data and time column and creating factors of time to prepare data for analyis
df_uber <- df_uber %>% mutate(year = year(Date.Time), 
                    month = month(Date.Time), 
                    day = day(Date.Time),
                    hour = hour(Date.Time),
                    minute = minute(Date.Time),
                    second = second(Date.Time))

#Dropping column 10, since it only has zeroes
df_uber <-select(df_uber,-c(10))

#Making sure we dont get scientic numbers in visulas
options(scipen =999)

#Getting the count of trips by month in nyc
month_uber <- df_uber %>%
  group_by(month) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

#Digging further deep and now analyzing the pickuptrips at Hour level
hour_uber <- df_uber %>%
  group_by(hour) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

#Making Visual for hours
h1<- ggplot(hour_uber, aes(hour, Count)) + geom_point(color='red') +geom_line()+ggtitle("Trip Count by Hours Uber") +
  geom_smooth(color='darkgreen')+ geom_text(aes(label=hour),hjust=0.5, vjust=2)
h1


#Getting data insights from both month and hour perspective
hourmonth_uber <- df_uber %>%
  group_by(hour,month) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))

#Using case when to change the month numbers to month names
hourmonth_uber <- hourmonth_uber %>%
  mutate(`month` = str_trim(`month`)) %>% 
  mutate(month = case_when(`month` == "4" ~ 'April',
                           `month` == "5" ~ 'May',
                           `month` == "6" ~ 'June',
                           `month` == "7" ~ 'July',`month` == "8" ~ 'August',`month` == "9" ~ 'September'))

#Making some visulas and using colors learnt in lecture to improve aesthics
ggplot(hourmonth_uber, aes(x=hour, y=Total, fill = month)) + 
  geom_bar( stat = "identity", position = 'dodge', width=0.78) + scale_fill_brewer(palette="Spectral")+
  ggtitle("Trip_Count by Hour and Month") 

#Using Lubridate to get day names and then mutating them.
df_uber <- df_uber %>% mutate(wday = wday(Date.Time, label = TRUE))


#Getting trip count by weekday and month
dm_group <- df_uber %>%
  group_by(month, wday) %>%
  summarize(Count = n())%>%
  arrange(desc(Count))

head(dm_group)

ggplot(dm_group, aes(month, Count, fill = wday)) +
  geom_bar( stat = "identity") +
  ggtitle("Trip Count by Days and Months")

#Getting trip count by bases
b_group <- df_uber %>%
  group_by(Base) %>%
  summarize(Count = n())%>%
  arrange(desc(Count))

head(b_group)

ggplot(df_uber, aes(Base)) + 
  geom_bar(fill = "orange") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")+ 
  coord_flip()


#Comparing UBER PERFORMANCE with its Competitor LYFT PERFORMANCE

lyft <- read.csv("lyft.csv")
lyft <-select(lyft,-c(4))

head(lyft)

#Cleaning,Wrangling and Formatting the data
lyft$time_of_trip <- as.POSIXct(lyft$time_of_trip, format = "%m/%d/%Y %H:%M")
lyft$time_of_trip<- ymd_hms(lyft$time_of_trip)

lyft <- lyft %>% mutate(year = year(time_of_trip), 
                              month = month(time_of_trip), 
                              day = day(time_of_trip),
                              hour = hour(time_of_trip),
                              minute = minute(time_of_trip))
#Getting data insights from both month and hour perspective
month_lyft <- lyft %>%
  group_by(month) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

#Using case when to change the month numbers to month names
month_lyft <- month_lyft %>%
  mutate(`month` = str_trim(`month`)) %>% 
  mutate(month = case_when(`month` == "4" ~ 'April',
                           `month` == "5" ~ 'May',
                           `month` == "6" ~ 'June',
                           `month` == "7" ~ 'July',`month` == "8" ~ 'August',`month` == "9" ~ 'September'))
month_uber <- month_uber %>%
  mutate(`month` = str_trim(`month`)) %>% 
  mutate(month = case_when(`month` == "4" ~ 'April',
                           `month` == "5" ~ 'May',
                           `month` == "6" ~ 'June',
                           `month` == "7" ~ 'July',`month` == "8" ~ 'August',`month` == "9" ~ 'September'))
#Making a column in LYFT
month_lyft <- month_lyft %>%
  mutate(Type = "Lyft")

#Making a column in Uber

month_uber <- month_uber %>%
  mutate(Type = "Uber")

month_uber <- filter(month_uber, month == "September"| month == "August"|month == "July")

month_uber_lyft <- rbind(month_lyft,month_uber)


ggplot(data = month_uber_lyft, aes(x = Type, y = Count, group = month)) +
  geom_line(aes(color = month), size = 1.5) +
  geom_point(aes(color = month), size = 4) + scale_x_discrete(position = "top") +
  geom_text(data = month_uber_lyft, 
            aes(label = paste0(Count)) , 
            hjust = -.45, 
            fontface = "bold", 
            size = 3.5) +
  # moving x labels
  scale_x_discrete(position = "top")  +
  # Removing border
  theme(panel.border=element_blank()) +
  # Removing  the y axis
  theme(axis.title.y=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank()) +
  # Cleaning up x axis
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top = element_text(size=15)) +
  # cleaning and removing x & y tick marks
  theme(axis.ticks=element_blank()) +
  theme(plot.title=element_text(size=8, hjust = 0.8)) +
  theme(plot.subtitle=element_text(hjust = 0.8))+ggtitle("Trip Count by Months for Uber and Lyft")

#Digging further deep and now analyzing the pickuptrips at Hour level
hour_lyft <- lyft %>%
  group_by(hour) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

#Making Visual for hours
h2<- ggplot(hour_lyft, aes(hour, Count)) + geom_point(color='mediumvioletred') +geom_line()+ggtitle("Trip Count by Hours Lyft") +
  geom_smooth(color='darkgreen')+ geom_text(aes(label=hour),hjust=0.5, vjust=2)
h2

grid <- grid.arrange(h1,h2,ncol=2, nrow=1)

grid

