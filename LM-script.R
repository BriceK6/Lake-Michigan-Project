library(readr)
GLOS_data_export_45161 <- read_csv("GLOS_data_export_45161.csv", skip = 1)
lake <- GLOS_data_export_45161
library(tidyverse)
library(dplyr)
library(purrr)
library(lubridate)
library(zoo)




#change to shorter column names 

 lake <- lake %>%
   rename(
     date_time = `Date/Time (UTC)`,
     wind_speed = `Wind_Speed (kts)`,
     wind_gust = `Wind_Gust (kts)`,
     wind_direction = `Wind_from_Direction (degrees_true)`,
     water_temp = `Water_Temperature_at_Surface (fahrenheit)`,
     wave_height = `Significant_Wave_Height (ft)`,
     wave_period = `Significant_Wave_Period (s)`,
     wave_direction = `significant_wave_from_direction (degrees_true)`,
     air_temp = `Air_Temperature (fahrenheit)`,
     air_pressure = `Air_Pressure (mb)`,
     therm_1m = `Thermistor_String_at_1m (fahrenheit)`,
     therm_22m = `Thermistor_String_at_22m (fahrenheit)`
   )
 
 

 # add month and year to table ****fix 
   
 lake$date_time <- mdy_hm(lake$date_time)
 
 lake <- lake %>%
    mutate(as_datetime(date_time))
 
 lake <- lake %>%
    mutate(month(date_time, label = TRUE))
lake <- lake %>%
    rename(month_date = `month(date_time, label = TRUE)`)

lake <- lake %>%
   mutate(year(date_time)) 
lake <- lake %>%
  rename(year_date = `year(date_time)`)

lake %>%
  select(date_time, month_date, year_date)%>%
  filter(is.na(month_date), is.na(year_date))

#finding missing and incorrect data
lake %>%
  summarise(meanWind = mean(wind_speed), minWind = min(wind_speed), maxWind = max(wind_speed))
lake %>%
  filter(wind_speed >=0 & wind_speed < 194)%>%
  summarise(meanWind = mean(wind_speed), minWind = min(wind_speed), maxWind = max(wind_speed))

lake %>%
  summarise(meanWave = mean(wave_height), minWave = min(wave_height), maxWave = max(wave_height))
lake %>%
  filter(wave_height >=0 & wave_height < 30)%>%
  summarise(meanWave = mean(wave_height), minWave = min(wave_height), maxWave = max(wave_height))
  
 
#create tibble of wind speed and direction 
wind <- tibble(lake[,c(1:4,14,15)]) %>%
  filter(wind_speed >= 0 & wind_speed < 194)
wind_clean <- wind %>% 
  filter(wind_gust != -19436.45616, wind_direction >= 0 & wind_direction <= 360)

wind_clean %>%
  summarise(min = min(wind_gust), mean = mean(wind_gust), max = max(wind_gust))

  #add mph to wind             
wind <- wind %>%
  mutate(mph = wind_speed*1.15078)


#create wave tibble
wave <- tibble(lake[,c(1,6:8, 14:15)])
#removing missing wave height and wave period
wave <- subset(wave, wave$wave_height!= -32805.11916 )
wave2 <- subset(wave, wave$wave_period != -9999 & wave$wave_direction != 0)

wave2 <- wave2 %>%
  filter(wave_direction >= 0 & wave_direction <= 360)

wave2 %>%
  summarize(mean(wave_period), min(wave_period), max(wave_period))
wave2 %>%
  summarize(mean(wave_direction), min(wave_direction), max(wave_direction))

#surfable waves
surfWave <- subset(wave2, wave2$wave_height >= 2.5)
surfWave %>%
  ggplot(mapping = aes(x = month_date, y = wave_height))+
  geom_point(size = .1)+
  geom_smooth()

surfWave %>%
  count(month_date)
wave2 %>%
  count(month_date)
wave %>%
  count(month_date)

#ideal surfing conditions
wavewind <- lake %>%
  select(date_time, wind_speed, wave_height, wind_direction, wave_direction, month_date, year_date ) %>%
  filter(wind_speed >= 0 & wind_speed < 150, wave_height >= 0 & wave_height < 30, wind_direction >= 0 & wind_direction <= 360, wave_direction >=0 & wave_direction <= 360) %>%
  arrange(date_time)

byMonthYear <- lake %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  filter(wave_height >=0 & wave_height < 30, wind_speed >= 0 & wind_speed < 100)%>%
  group_by(month_date, year_date)%>%
  summarise(waveH = mean(wave_height), windS = mean(wind_speed))

surfIdeal <- wavewind %>%
  filter(wind_speed > 0 & wind_speed < 10, wave_height >= 2.5 & wave_height < 300)%>%
  arrange(desc(wave_height))
  
surfIdeal %>%  
  summarize(meanWave = mean(wave_height), minWave = min(wave_height),
            maxWave = max(wave_height), meanWind = mean(wind_speed),
            minWind = min(wind_speed), maxWind = min(wind_speed), count = count(surfIdeal))
  
surfIdeal %>%
  ggplot(mapping = aes(x = wind_speed, y = wave_height))+
  geom_point(size = .5)+
  labs(title = "Ideal surf conditions Wave vs Wind")

surfIdeal %>%
  ggplot(mapping = aes(x = wind_speed, y = wave_height))+
  geom_bin2d()+
  labs(title = "Density Plot of Ideal Surf Conditions")

surfIdeal %>%
  ggplot(mapping = aes(x = wind_direction, y = wave_height))+
  geom_point(size = .5)+
  labs(title = "Ideal surf conditions Wave vs Wind Direction")

surfIdeal %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  group_by(month_date)%>%
  summarise(waveH = mean(wave_height), windS = mean(wind_speed))%>%
  ggplot(mapping = aes(x = month_date, y = waveH))+
  geom_bar(stat = 'identity')+
  labs(title = "Monthly Average Wave Height For Ideal Surf Conditions")
  
surfIdeal %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  group_by(month_date)%>%
  summarise(waveH = mean(wave_height), windS = mean(wind_speed))%>%
  ggplot(mapping = aes(x = month_date, y = windS))+
  geom_bar(stat = 'identity')+
  labs(title = "Monthly Average Wind Speed For Ideal Surf Conditions")

surfIdeal %>%
  select(month_date)%>%
  group_by(month_date)%>%
  count(month_date)%>%
  ggplot(mapping = aes(x = month_date, y = n))+
  geom_bar(stat = 'identity')+
  labs(title = "Monthly Number of Oberservations for Ideal Surfing")
  

#plot relationship between wind speed and wave size
ggplot(data = wavewind, mapping = aes(x = wind_speed, y = wave_height))+
  geom_point(size = .1)+
  geom_smooth()+
  labs(title = "Wind Speed vs. Wave Height")

# plot wave height based on wind direction , 245 is parallel
wavewind %>%
  select(wind_direction)%>%
  summarise(min = min(wind_direction), mean = mean(wind_direction), max = max(wind_direction))

ggplot(data = wavewind, mapping = aes(x = wind_direction, y = wave_height))+
  geom_point(size = .1)+
  geom_smooth()+
  labs(title = "Wind Direction vs. Wave Height")

#Graph monthly aveages 
monthly <- function(var_y) {}
  
lake %>%
    select(date_time, month_date, year_date, wave_height, wind_speed)%>%
    filter(wave_height >=0 & wave_height < 30, wind_speed >= 0 & wind_speed < 100)%>%
    group_by(month_date)%>%
    summarise(waveH = mean(wave_height), windS = mean(wind_speed))

    # Doing the same thing with already filtered subset 
wavewind %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  group_by(month_date)%>%
  summarise(waveH = mean(wave_height), windS = mean(wind_speed))%>%
  ggplot(mapping = aes(x = month_date, y = waveH))+
    geom_bar(stat = 'identity')+
    labs(title = "Monthly Average Wave Height")

    #Summarize by month and year *notice missing years
wavewind %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  group_by(month_date, year_date)%>%
  summarise(waveH = mean(wave_height), windS = mean(wind_speed))
    
    #Same thing but with the original dataset only filtering out wind and wave
    # notice only one more year was added
byMonthYear <- lake %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  filter(wave_height >=0 & wave_height < 30, wind_speed >= 0 & wind_speed < 100)%>%
  group_by(month_date, year_date)%>%
  summarise(waveH = mean(wave_height), windS = mean(wind_speed))

yearMonth <- lake %>%
  mutate(month_year = yearmon(wave_height))

# graph monthly 
lake %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  filter(wave_height >=0 & wave_height < 30, wind_speed >= 0 & wind_speed < 100)%>%
  group_by(month_date, year_date) %>%
  ggplot(mapping = aes(month(date_time, label=TRUE, abbr=TRUE), 
     group=factor(year(date_time)), y = wave_height, colour=factor(year(date_time)))) +
  geom_line() +
  geom_point() +
  labs(x="Month", colour="Year") +
  theme_classic()
 
      #FIX
lake %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  filter(wave_height >=0 & wave_height < 30, wind_speed >= 0 & wind_speed < 100)%>%
  group_by(month_date, year_date) %>%
  summarize(waveH = mean(wave_height))%>%
  ggplot(mapping = aes(month(date_time, label=TRUE, abbr=TRUE), 
                       group=factor(year(date_time)), y = wave_ave, colour=factor(year(date_time)))) +
  geom_line() +
  geom_point() +
  labs(x="Month", colour="Year") +
  theme_classic()

    #scratch 
lake %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  filter(wave_height >=0 & wave_height < 30, wind_speed >= 0 & wind_speed < 100)%>%
  group_by(month_date, year_date) %>%
  summarize(waveH = mean(wave_height))
        #FIX
lake %>%
  select(month_date, year_date, wave_height, wind_speed)%>%
  filter(wave_height >=0 & wave_height < 30, wind_speed >= 0 & wind_speed < 100)%>%
  group_by(month_date)%>%
  summarize(waveH = mean(wave_height), month_date, year_date)%>%
  ggplot(mapping = aes(x = month_date, y = waveH, colour = factor(year_date)))+ 
                     #  group=factor(year_date, y = waveH, colour=factor(year_date))) +
  geom_line() +
  geom_point() +
  labs(x="Month", y = "Wave Height", colour="Year") +
  theme_classic()

#show variance in yearly monthly averages
byMonthYear %>%
  ggplot(mapping = aes(x = month_date,label=TRUE, abbr=TRUE, y = waveH, colour = factor(year_date)),
         group=factor(year_date))+
  geom_line() +
  geom_point() +
  labs(title = "Monthly Average Wave Height by Year", x="Month", y = "Wave Height", color = "Year")+
  theme_classic()

# group factor by month
byMonthYear %>%
  ggplot(mapping = aes(x = month_date,label=TRUE, abbr=TRUE, y = windS, colour = year_date),
         group=factor(month_date))+
  geom_line() +
  geom_point() +
  labs(title = "Yearly Monthly Wind Speed Averages", x="Month", y = "Wind Speed")+
  theme_classic()

   # fix
byMonthYear %>%
  ggplot(mapping = aes(x = month_date,label=TRUE, abbr=TRUE, y = waveH, colour = year_date),
         group=factor(waveH))+
  geom_line() +
  geom_point() +
  labs(x="Month", y = "Wave Height")+
  theme_classic()



lake %>%
  select(date_time, month_date, wave_height, wind_speed)%>%
  filter(wave_height >=0 & wave_height < 30, wind_speed >= 0 & wind_speed < 100, )

#ideal kite conditions for beginners 
kiteIdeal <- wavewind %>%
  filter(wind_speed > 12 & wind_speed <16 & wave_height < 2.5 & wind_direction > 140 & wind_direction < 340)
     #graph wind speeds for ideal kite conditions for beginners
kiteIdeal %>%
  ggplot(mapping = aes(x = wave_height, y = wind_speed))+
  geom_point(size = .5)+
  geom_smooth()+
  labs(title = "Ideal Kite Conditions Wave vs Wind")

wind %>%
  filter(wind_speed > 12 & wind_speed <16 & wind_direction > 140 & wind_direction < 340)%>%
  ggplot(mapping = aes(x = wind_direction, y = wind_speed))+
  geom_point()+
  labs(title = "Wind Direction of Ideal Kite Conditions")

kiteIdeal %>%
  select(date_time, month_date, year_date, wave_height, wind_speed)%>%
  group_by(month_date)%>%
  count(month_date)%>%
  ggplot(data = kideal, mapping = aes(x = month_date, y = n))+
  geom_bar(stat = 'identity')+
  labs(title = "Number of Observations for Ideal Kiting")

cor(wavewind$wind_speed, wavewind$wave_height)
l_reg <- lm(data = wavewind, wave_height ~ wind_speed)
print(summary(l_reg))
