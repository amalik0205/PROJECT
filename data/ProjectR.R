#Installing Packages
install.packages('DT')
install.packages('ggthemes')
install.packages("wesanderson")
install.packages("ggprel")
install.packages("kableExtra")
install.packages("ghit")


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
library(pdftools)
library(tidyverse)


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
df_uber <- df_uber %>% mutate(Date = date(Date.Time), year = year(Date.Time),
                    month = month(Date.Time), 
                    day = day(Date.Time),
                    hour = hour(Date.Time),
                    minute = minute(Date.Time),
                    second = second(Date.Time))

#Dropping column 10, since it only has zeroes
df_uber <-select(df_uber,-c(11))

#Making sure we dont get scientic numbers in visulas
options(scipen =999)



#Getting the count of trips by month in nyc
month_uber <- df_uber %>%
  group_by(month) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))
head(month_uber)

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

head(hourmonth_uber)
tail(hourmonth_uber)
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
df_uber <- df_uber %>% mutate(wday = weekdays(Date.Time))


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
head(month_lyft)

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

head(month_uber)
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

#Wrangling of weather data
listofdfs <- list()
for (i in c <- list("aprilw.pdf","mayw.pdf","junew.pdf","julyw.pdf","augustw.pdf","septemberw.pdf")) {
  
  PDF <- pdf_text(i) %>%
    readr::read_lines()
  PDF

  PDF.weather <-PDF[-c(1:3,36:46)] # remove lines
  PDF.weather

  all_stat_lines <- PDF.weather[3:32] %>%
    str_squish() %>%
    strsplit(split = " ")# remove empty spaces

  col_lines <- c("Date", "Maximum", "Minimum", "Average", "Departure","HDD", "CDD","Precipitation", "NewSnow", "SnowDepth") # create your variable names

  df <- plyr::ldply(all_stat_lines) #create a data frame

  colnames(df) <- col_lines
  final_df <- as_tibble(df)
  listofdfs[[i]] <- final_df 
}

weatherdf <- bind_rows(listofdfs)
weatherdf$Date <- as.Date(weatherdf$Date)
weatherdf$Maximum <- as.numeric(weatherdf$Maximum)
weatherdf$Minimum <- as.numeric(weatherdf$Minimum)
weatherdf$Average <- as.numeric(weatherdf$Average)
weatherdf$Departure <- as.numeric(weatherdf$Departure)
weatherdf$HDD <- as.numeric(weatherdf$HDD)
weatherdf$CDD <- as.numeric(weatherdf$CDD)
weatherdf$Precipitation <- as.numeric(weatherdf$Precipitation)
weatherdf$NewSnow <- as.numeric(weatherdf$NewSnow)
weatherdf$SnowDepth  <- as.numeric(weatherdf$SnowDepth )

#Removing NAS
weatherdf[is.na(weatherdf)] <- 0

weatherdf <- weatherdf %>% mutate(Date = date(Date), year = year(Date),
                              month = month(Date), 
                              day = day(Date))

#Getting the count of trips by day in nyc
daymonth_uber <- df_uber %>%
  group_by(day,month) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))


bigdf3 <- left_join(daymonth_uber, weatherdf, by = c("month" = "month","day" = "day"))

bigdf3  <- bigdf3  %>%
  mutate(`month` = str_trim(`month`)) %>% 
  mutate(month = case_when(`month` == "4" ~ 'April',
                           `month` == "5" ~ 'May',
                           `month` == "6" ~ 'June',
                           `month` == "7" ~ 'July',`month` == "8" ~ 'August',`month` == "9" ~ 'September'))


ggplot(bigdf3, aes(Average, Count, color=month))+geom_line()+geom_point()+ggtitle("Trip Count Weather Effect") +
  geom_smooth(color='black')


ggplot(data = bigdf3, aes(x = Maximum, y =Count, color = month)) + 
  geom_point()+geom_boxplot()




sepdfw  <- filter(bigdf3, month == "September")

ggplot(sepdfw, aes(Average, Count))+geom_line()+geom_point(color='orange')+ggtitle("September:Trip Count Daily Average Temperature Effect") +
  geom_smooth(color='red')

ggplot(sepdfw, aes(Precipitation, Count))+geom_line(color="blue")+geom_point(color="orange")+ggtitle("September:Trip Count Precitipation Effect")+
  geom_text(aes(label=day),hjust=0.03, vjust=0.7)


aprildfw  <- filter(bigdf3, month == "April")
ggplot(aprildfw, aes(Precipitation, Count))+geom_line(color="red")+geom_point(color="yellow")+ggtitle("April:Trip Count Precitipation Effect")+
  geom_text(aes(label=day),hjust=0.03, vjust=0.7)

ggplot(aprildfw, aes(Average, Count))+geom_line()+geom_point(color='orange')+ggtitle("April:Trip Count Daily Average Temperature Effect") +
  geom_smooth(color='red')


