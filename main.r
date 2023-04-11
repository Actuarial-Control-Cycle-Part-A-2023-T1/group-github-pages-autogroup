### import packages ### 
library(tidyverse)
library(readxl)
library(forecast)

### SECTION 1 - DATA PREP & CLEANING ###

### 1.0 - load data ###
setwd("/Users/michael/Downloads")
econ_data <- as.data.frame(read_excel("2023-student-research-eco-dem-data.xlsx", sheet = 3, range = "B8:F68"))
dem_data <- as.data.frame(read_excel("2023-student-research-eco-dem-data.xlsx", sheet = "Demographic-Economic", range = "B8:H51"))
hazard_data <- as.data.frame(read_excel("2023-student-research-hazard-event-data.xlsx", range = "B13:I3379"))

### 1.1 - macroeconomic data ###
econ_data <- econ_data %>% fill(colnames(econ_data)) #forward filling missing values
ggplot(econ_data) + aes(x = Year, y = Inflation) + geom_line() #check plot, weird 990% inflation in 2003
econ_data[econ_data$Year == 2003, ]$Inflation <- econ_data[econ_data$Year == 2002, ]$Inflation #impute 2003 with last known value
ggplot(econ_data) + aes(x = Year, y = Inflation) + geom_line() #check plot again, looks much better 

### 1.2 - demographic data ###
dem_data <- as.data.frame(t(dem_data)) #organising rows and columns, converting formats
colnames(dem_data) <- dem_data[1,]
dem_data <- dem_data[-1,]
dem_data[] <- lapply(dem_data, function(x) gsub("Ꝕ ", "", x))
dem_data[] <- lapply(dem_data, function(x) as.numeric(gsub(",", "", x)))

### 1.3 - hazard data ###
hazard_data[,c(1:3)] <- lapply(hazard_data[,c(1:3)], as.factor) #coverting formats, fix typo in event type
hazard_data$`Hazard Event` <- as.character(hazard_data$`Hazard Event`)
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% c("Severe Storm/Thunder Storm - Wind")] <- "Severe Storm/Thunder Storm/ Wind"

#reduce cardinality by grouping multi-hazard events into most severe contributing hazard
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Hurricane/Tropical Storm",
                               "Coastal/ Hurricane/Tropical Storm/ Wind",
                               "Coastal/ Hurricane/Tropical Storm/ Severe Storm/Thunder Storm/ Wind",
                               "Hurricane/Tropical Storm/ Severe Storm/Thunder Storm")] <- "Hurricane"
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Drought/ Heat")] <- "Drought"
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Hail/ Wind")] <- "Hail"
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Wind/ Winter Weather")] <- "Winter Weather"
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Coastal/ Flooding",
                               "Coastal/ Wind")] <- "Coastal"
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Flooding/ Hail",
                               "Flooding/ Hail/ Wind",
                               "Flooding/ Lightning",
                               "Flooding/ Lightning/ Wind",
                               "Flooding/ Wind")] <- "Flooding"
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Hail/ Tornado",
                               "Hail/ Tornado/ Wind",
                               "Lightning/ Tornado/ Wind",
                               "Tornado/ Wind")] <- "Tornado"
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Hail/ Lightning",
                               "Hail/ Lightning/ Wind",
                               "Lightning/ Wind")] <- "Tornado"
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Coastal/ Severe Storm/Thunder Storm/ Wind",
                               "Coastal/ Severe Storm/Thunder Storm",
                               "Lightning/ Severe Storm/Thunder Storm",
                               "Flooding/ Lightning/ Severe Storm/Thunder Storm",
                               "Hail/ Lightning/ Severe Storm/Thunder Storm/ Wind",
                               "Hail/ Severe Storm/Thunder Storm/ Wind",
                               "Severe Storm/Thunder Storm/ Wind",
                               "Severe Storm/Thunder Storm",
                               "Flooding/ Severe Storm/Thunder Storm",
                               "Severe Storm/Thunder Storm/ Winter Weather",
                               "Coastal/ Flooding/ Severe Storm/Thunder Storm/ Wind",
                               "Severe Storm/Thunder Storm/ Wind/ Winter Weather",
                               "Hail/ Severe Storm/Thunder Storm",
                               "Lightning/ Severe Storm/Thunder Storm/ Wind",
                               "Flooding/ Severe Storm/Thunder Storm/ Wind",
                               "Hail/ Lightning/ Severe Storm/Thunder Storm",
                               "Hail/ Severe Storm/Thunder Storm/ Wind/ Winter Weather"
                             )] <- "Severe Storm"

### Section 2 - EDA OF HAZARDS ###

### 2.1 - national data ###
hazard_count <- hazard_data %>% #total count and average damage of each hazard type
  group_by(`Hazard Event`) %>%
  summarise(Count = n(), AverageCost = mean(`Property Damage`), AverageFatalities = mean(`Fatalities`), AverageInjuries = mean(`Injuries`), AverageDuration = mean(`Duration`))

hr_gross_max <- hazard_data %>% #examine most catastrophic single events
  arrange(desc(`Property Damage`)) 

head(hr_gross_max, 10) #looking at the data, we would need to potentially remove some outliers, but also since so many outliers are for 2, may indicate that there are more expensive regions? does not match up with the demographic data however 

ggplot() + 
  aes(hr_gross_max$`Property Damage`) + 
  geom_density()

ggplot() +  
  aes(hr_gross_max$`Property Damage`[300:nrow(hr_gross_max)]) + 
  geom_density() #significantly grouped towards lower values

#region 2 appears to have the most catastrophic events. examine region 2 event history (potentially remove outliers?)
hr_gross_max_r2 <- hr_gross_max %>% filter(Region == 2) 

ggplot() + 
  aes(hr_gross_max_r2$`Property Damage`[70:nrow(hr_gross_max_r2)]) + 
  geom_density() #cutting the top 10% of values will yield a similar distribution to overall, but those 10% may not be ignorable if they are consistently from region 2
rm(hr_gross_max_r2)

#further reduce cardinality by categorising events into 3 severities, as per SSP scenarios
hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Hurricane",
                               "Wildfire")] <- "Major"

hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Drought",
                               "Flooding",
                               "Tornado",
                               "Winter Weather")] <- "Medium"

hazard_data$`Hazard Event`[hazard_data$`Hazard Event` %in% 
                             c("Lightning",
                               "Wind",
                               "Severe Storm",
                               "Coastal",
                               "Fog",
                               "Hail",
                               "Heat",
                               "Landslide")] <- "Minor"

### 2.2 - regional data ###
#break down overall gross hazard counts by region
hr1 <- hazard_data %>% filter(Region == 1) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 1")
hr2 <- hazard_data %>% filter(Region == 2) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 2")
hr3 <- hazard_data %>% filter(Region == 3) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 3")
hr4 <- hazard_data %>% filter(Region == 4) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 4")
hr5 <- hazard_data %>% filter(Region == 5) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 5")
hr6 <- hazard_data %>% filter(Region == 6) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 6")
hr_count_bytype <- list(hr1,hr2,hr3,hr4,hr5,hr6) %>% reduce(full_join, by = "Hazard Event")
rm(hr1,hr2,hr3,hr4,hr5,hr6)

#break down overall average hazard costs by region
hr1 <- hazard_data %>% filter(Region == 1)
hr1 <- aggregate(hr1$`Property Damage`, by = list("Hazard Event" = hr1$`Hazard Event`), FUN = mean) %>% rename_at(2,~"Region 1")
hr2 <- hazard_data %>% filter(Region == 2)
hr2 <- aggregate(hr2$`Property Damage`, by = list("Hazard Event" = hr2$`Hazard Event`), FUN = mean) %>% rename_at(2,~"Region 2")
hr3 <- hazard_data %>% filter(Region == 3)
hr3 <- aggregate(hr3$`Property Damage`, by = list("Hazard Event" = hr3$`Hazard Event`), FUN = mean) %>% rename_at(2,~"Region 3")
hr4 <- hazard_data %>% filter(Region == 4)
hr4 <- aggregate(hr4$`Property Damage`, by = list("Hazard Event" = hr4$`Hazard Event`), FUN = mean) %>% rename_at(2,~"Region 4")
hr5 <- hazard_data %>% filter(Region == 5)
hr5 <- aggregate(hr5$`Property Damage`, by = list("Hazard Event" = hr5$`Hazard Event`), FUN = mean) %>% rename_at(2,~"Region 5")
hr6 <- hazard_data %>% filter(Region == 6)
hr6 <- aggregate(hr6$`Property Damage`, by = list("Hazard Event" = hr6$`Hazard Event`), FUN = mean) %>% rename_at(2,~"Region 6")
hr_avgcost_bytype <- list(hr1,hr2,hr3,hr4,hr5,hr6) %>% reduce(full_join, by = "Hazard Event")
rm(hr1,hr2,hr3,hr4,hr5,hr6)

#break down annual hazard costs by region
hr1 <- hazard_data %>% filter(Region == 1) %>% select(`Hazard Event`, Year, `Property Damage`) %>% mutate("Region" = "1", "Households" = dem_data[1,7])
hr2 <- hazard_data %>% filter(Region == 2) %>% select(`Hazard Event`, Year, `Property Damage`) %>% mutate("Region" = "2", "Households" = dem_data[2,7])
hr3 <- hazard_data %>% filter(Region == 3) %>% select(`Hazard Event`, Year, `Property Damage`) %>% mutate("Region" = "3", "Households" = dem_data[3,7])
hr4 <- hazard_data %>% filter(Region == 4) %>% select(`Hazard Event`, Year, `Property Damage`) %>% mutate("Region" = "4", "Households" = dem_data[4,7])
hr5 <- hazard_data %>% filter(Region == 5) %>% select(`Hazard Event`, Year, `Property Damage`) %>% mutate("Region" = "5", "Households" = dem_data[5,7])
hr6 <- hazard_data %>% filter(Region == 6) %>% select(`Hazard Event`, Year, `Property Damage`) %>% mutate("Region" = "6", "Households" = dem_data[6,7])
hr_cost <- bind_rows(hr1,hr2,hr3,hr4,hr5,hr6)
hr_cost <- replace(hr_cost, hr_cost == 0,3) #replace zero values with 3, such that log will still produce non-negative values and binomial distribution can still be used without having significant effect on data
rm(hr1,hr2,hr3,hr4,hr5,hr6)


#break down annual hazard counts by region
year_index <- data.frame(Year  = as.numeric(1960:2020))
hr1min <- hazard_data %>% filter(Region == 1) %>% filter(`Hazard Event` == "Minor")
hr1min <- aggregate(hr1min$`Property Damage`, by = list(Year = hr1min$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "1", "Hazard Event" = "Minor", "Households" = dem_data[1,7])

hr1med <- hazard_data %>% filter(Region == 1) %>% filter(`Hazard Event` == "Medium")
hr1med <- aggregate(hr1med$`Property Damage`, by = list(Year = hr1med$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "1", "Hazard Event" = "Medium", "Households" = dem_data[1,7])


hr1maj <- hazard_data %>% filter(Region == 1) %>% filter(`Hazard Event` == "Major")
hr1maj <- aggregate(hr1maj$`Property Damage`, by = list(Year = hr1maj$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "1", "Hazard Event" = "Major", "Households" = dem_data[1,7])


hr2min <- hazard_data %>% filter(Region == 2) %>% filter(`Hazard Event` == "Minor")
hr2min <- aggregate(hr2min$`Property Damage`, by = list(Year = hr2min$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "2", "Hazard Event" = "Minor", "Households" = dem_data[2,7])


hr2med <- hazard_data %>% filter(Region == 2) %>% filter(`Hazard Event` == "Medium")
hr2med <- aggregate(hr2med$`Property Damage`, by = list(Year = hr2med$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "2", "Hazard Event" = "Medium", "Households" = dem_data[2,7])


hr2maj <- hazard_data %>% filter(Region == 2) %>% filter(`Hazard Event` == "Major")
hr2maj <- aggregate(hr2maj$`Property Damage`, by = list(Year = hr2maj$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "2", "Hazard Event" = "Major", "Households" = dem_data[2,7])


hr3min <- hazard_data %>% filter(Region == 3) %>% filter(`Hazard Event` == "Minor")
hr3min <- aggregate(hr3min$`Property Damage`, by = list(Year = hr3min$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "3", "Hazard Event" = "Minor", "Households" = dem_data[3,7])


hr3med <- hazard_data %>% filter(Region == 3) %>% filter(`Hazard Event` == "Medium")
hr3med <- aggregate(hr3med$`Property Damage`, by = list(Year = hr3med$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "3", "Hazard Event" = "Medium", "Households" = dem_data[3,7])


hr3maj <- hazard_data %>% filter(Region == 3) %>% filter(`Hazard Event` == "Major")
hr3maj <- aggregate(hr3maj$`Property Damage`, by = list(Year = hr3maj$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "3", "Hazard Event" = "Major", "Households" = dem_data[3,7])


hr4min <- hazard_data %>% filter(Region == 4) %>% filter(`Hazard Event` == "Minor")
hr4min <- aggregate(hr4min$`Property Damage`, by = list(Year = hr4min$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "4", "Hazard Event" = "Minor", "Households" = dem_data[4,7])


hr4med <- hazard_data %>% filter(Region == 4) %>% filter(`Hazard Event` == "Medium")
hr4med <- aggregate(hr4med$`Property Damage`, by = list(Year = hr4med$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "4", "Hazard Event" = "Medium", "Households" = dem_data[4,7])


hr4maj <- hazard_data %>% filter(Region == 4) %>% filter(`Hazard Event` == "Major")
hr4maj <- aggregate(hr4maj$`Property Damage`, by = list(Year = hr4maj$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "4", "Hazard Event" = "Major", "Households" = dem_data[4,7])


hr5min <- hazard_data %>% filter(Region == 5) %>% filter(`Hazard Event` == "Minor")
hr5min <- aggregate(hr5min$`Property Damage`, by = list(Year = hr5min$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "5", "Hazard Event" = "Minor", "Households" = dem_data[5,7])


hr5med <- hazard_data %>% filter(Region == 5) %>% filter(`Hazard Event` == "Medium")
hr5med <- aggregate(hr5med$`Property Damage`, by = list(Year = hr5med$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "5", "Hazard Event" = "Medium", "Households" = dem_data[5,7])


hr5maj <- hazard_data %>% filter(Region == 5) %>% filter(`Hazard Event` == "Major")
hr5maj <- aggregate(hr5maj$`Property Damage`, by = list(Year = hr5maj$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "5", "Hazard Event" = "Major", "Households" = dem_data[5,7])


hr6min <- hazard_data %>% filter(Region == 6) %>% filter(`Hazard Event` == "Minor")
hr6min <- aggregate(hr6min$`Property Damage`, by = list(Year = hr6min$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "6", "Hazard Event" = "Minor", "Households" = dem_data[6,7])


hr6med <- hazard_data %>% filter(Region == 6) %>% filter(`Hazard Event` == "Medium")
hr6med <- aggregate(hr6med$`Property Damage`, by = list(Year = hr6med$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "6", "Hazard Event" = "Medium", "Households" = dem_data[6,7])


hr6maj <- hazard_data %>% filter(Region == 6) %>% filter(`Hazard Event` == "Major")
hr6maj <- aggregate(hr6maj$`Property Damage`, by = list(Year = hr6maj$Year), FUN = length) %>% 
  rename("Event Count" = x) %>%
  full_join(year_index, by = "Year") %>%
  replace(is.na(.), 0) %>%
  mutate("Region" = "6", "Hazard Event" = "Major", "Households" = dem_data[6,7])

hr_count <- bind_rows(hr1min,hr2min,hr3min,hr4min,hr5min,hr6min,hr1med,hr2med,hr3med,hr4med,hr5med,hr6med,hr1maj,hr2maj,hr3maj,hr4maj,hr5maj,hr6maj) 
rm(hr1min,hr2min,hr3min,hr4min,hr5min,hr6min,hr1med,hr2med,hr3med,hr4med,hr5med,hr6med,hr1maj,hr2maj,hr3maj,hr4maj,hr5maj,hr6maj,year_index)

#break down overall gross and average damages by region
hr_gross <- hazard_data %>%
  group_by(Region) %>%
  summarise(Count = n(), GrossCost = sum(`Property Damage`), GrossFatalities = sum(Fatalities), GrossInjuries = sum(Injuries))

hr_avg <- hazard_data %>%
  group_by(Region) %>%
  summarise(Count = n(), 
            AverageCost = mean(`Property Damage`), 
            AverageFatalities = mean(Fatalities), 
            AverageInjuries = mean(Injuries))

ggplot(hr_avg) + 
  aes(x = Region, y = Count) + 
  geom_col() + 
  ylab("Count of Disastrous Events")

#correcting for population to find what area has most hazards per population to get better idea of impact on people?
hr_avg$PopCount <- hr_avg$Count/dem_data$`Census, July 1, 2021`

ggplot(hr_avg) + 
  aes(x = Region, y = PopCount) + 
  geom_col() + 
  ylab("Count of Disastrous Events Corrected For Population Size")

#average cost of disasters per region
ggplot(hr_avg) + 
  aes(x = Region, y = AverageCost) + 
  geom_col() + 
  ylab("Average Cost of Property Damage From Disastrous Events in Region") 

ggplot(hr_avg) + 
  aes(x = Region, y = AverageFatalities) + 
  geom_col() + 
  ylab("Average Fatalities From Disastrous Events in Region") #much more even compared to below graph, outliers are nowhere near the 3831 injuries in one event 

ggplot(hr_avg) + 
  aes(x = Region, y = AverageInjuries) + 
  geom_col() + 
  ylab("Average Injuries From Disastrous Events in Region") #massive outlier in region 5

head(hazard_data %>%
       filter(Region == 5) %>%
       arrange(desc(Injuries))) #again a massive outlier event skewing results for region 5 in the above comparison, 3831 injuries compared to 117, remove from dataset? 

ggplot(hazard_data %>%
         filter(Injuries < 3000) %>%
         group_by(Region) %>%
         summarise(AverageInjuries = mean(Injuries))) +
  aes(x = Region, y = AverageInjuries) + 
  geom_col() + 
  ylab("Average Injuries From Disastrous Events in Region (Excluding One Outlier)")

### 2.3 - seasonality and long-term trends ###
yearquarter_count  <- hazard_data %>%
  unite(YQ, c(Year, Quarter), sep="") %>%
  group_by(YQ) %>%
  summarise(Count = n()) 

year_count <- hazard_data %>%
  group_by(Year) %>%
  summarise(Count = n(), AverageSeverity = mean(`Property Damage`))

quarter_count <- hazard_data %>%
  group_by(Quarter) %>%
  summarise(Count = n(), AverageSeverity = mean(`Property Damage`))

ggplot(yearquarter_count) + 
  aes(x = YQ, y = Count, group = 1) + 
  geom_line() #data too granular, cant see any clear trend 

ggplot(year_count) + 
  aes(x = Year, y = Count, group = 1) + 
  geom_line() #still not much of a clear trend 

ggplot(quarter_count) + 
  aes(x = Quarter, y = Count, group = 1) + 
  geom_line() #very significant differences, quarter 2 is 3 times as many disasters as quarter 4, seasonality of accidents occurring 

ggplot(year_count) + 
  aes(x = Year, y = AverageSeverity, group = 1) + 
  geom_line() #definitely subject to outliers, but it does seem that disaster costs have risen recently, can be due to many factors e.g. inflation, more expensive property, not just more devastating disasters

ggplot(quarter_count) + 
  aes(x = Quarter, y = AverageSeverity, group = 1) + 
  geom_line() #would need to remove outliers before seeing this properly due to not having as many quarters as years 

rm(quarter_count,year_count,yearquarter_count)

### SECTION 3 - MODELING ###

### 3.1 - macroeconomic indicies ###
#generate an ewma (exponentially weighted moving average, reduces noise in data)
ewma <- function(data, column, decay) { 
  arr <- data[, column]
  for (n in 2:length(data[, column])) {
    old <- arr[n - 1] *
      (1 - exp(-(n - 1) * decay)) / (1 - exp(-decay))
    arr[n] <- (arr[n] + old * exp(-decay)) /
      (1 - exp(-n * decay)) * (1 - exp(-decay))
  }
  arr
}

#define forecasting function, o is order of arima model, and n is number of forecasted times to be made
tsforecast <- function(data, column, decay, o, n = 20) {
  ewma_arr <- ewma(data, column, decay)
  
  print(
    ggplot(data) +
      aes(x = Year, group = 1) + 
      geom_line(aes(y = data[, column]), colour = "red") +
      geom_line(aes(y = ewma_arr), colour = "blue")
  )
  
  ewma_ts <- ts(ewma_arr, start = data$Year[1])
  ewma_model <- arima(ewma_ts, order = o)
  ewma_fc <- forecast(ewma_model, h = n)
  ewma_fc
}

#define an arbitrary decay constant, 0.1 seems to fit reasonably well
decay <- 0.1

inflation_data <- tsforecast(econ_data, colnames(econ_data)[2], decay, c(1, 2, 2))
plot(inflation_data)
inflation_data <- data.frame(inflation_data) %>%
  select(Point.Forecast) %>% 
  rownames_to_column(var = "Year") %>%
  add_row(Year = as.character(2021), Point.Forecast = 0.0384, .before = 1) %>%
  head(-1)

lr_data <- tsforecast(econ_data, colnames(econ_data)[3], decay, c(1, 1, 2))
plot(lr_data)

y1_data <- tsforecast(econ_data, colnames(econ_data)[4], decay, c(1, 1, 2))
plot(y1_data)

y10_data <- tsforecast(econ_data, colnames(econ_data)[5], decay, c(1, 1, 2))
plot(y10_data)

### 3.2 - hazard data ###
#forecasting
decay <- 0.1
order <- c(5, 1, 1)

hr1 <- aggregate(hr_cost$`Property Damage`, by = list(Year = hr_cost$Year, Region = hr_cost$Region), FUN = sum) %>% 
  rename("Event Cost" = x) %>%
  replace(is.na(.), 0) %>%
  filter(Region == "1") %>% 
  select(Year,`Event Cost`)
forecast_r1 <- tsforecast(hr1, colnames(hr1)[2], decay, order)
plot(forecast_r1)

hr2 <- aggregate(hr_cost$`Property Damage`, by = list(Year = hr_cost$Year, Region = hr_cost$Region), FUN = sum) %>% 
  rename("Event Cost" = x) %>%
  replace(is.na(.), 0) %>%
  filter(Region == "2") %>% 
  select(Year,`Event Cost`)
forecast_r2 <- tsforecast(hr2, colnames(hr2)[2], decay, order)
plot(forecast_r2)

hr3 <- aggregate(hr_cost$`Property Damage`, by = list(Year = hr_cost$Year, Region = hr_cost$Region), FUN = sum) %>% 
  rename("Event Cost" = x) %>%
  replace(is.na(.), 0) %>%
  filter(Region == "3") %>% 
  select(Year,`Event Cost`)
forecast_r3 <- tsforecast(hr3, colnames(hr3)[2], decay, order)
plot(forecast_r3)

hr4 <- aggregate(hr_cost$`Property Damage`, by = list(Year = hr_cost$Year, Region = hr_cost$Region), FUN = sum) %>% 
  rename("Event Cost" = x) %>%
  replace(is.na(.), 0) %>%
  filter(Region == "4") %>% 
  select(Year,`Event Cost`)
forecast_r4 <- tsforecast(hr4, colnames(hr4)[2], decay, order)
plot(forecast_r4)

hr5 <- aggregate(hr_cost$`Property Damage`, by = list(Year = hr_cost$Year, Region = hr_cost$Region), FUN = sum) %>% 
  rename("Event Cost" = x) %>%
  replace(is.na(.), 0) %>%
  filter(Region == "5") %>% 
  select(Year,`Event Cost`)
forecast_r5 <- tsforecast(hr5, colnames(hr5)[2], decay, order)
plot(forecast_r5)

hr6 <- aggregate(hr_cost$`Property Damage`, by = list(Year = hr_cost$Year, Region = hr_cost$Region), FUN = sum) %>% 
  rename("Event Cost" = x) %>%
  replace(is.na(.), 0) %>%
  filter(Region == "6") %>% 
  select(Year,`Event Cost`)
forecast_r6 <- tsforecast(hr6, colnames(hr6)[2], decay, order)
plot(forecast_r6)

rm(hr1,hr2,hr3,hr4,hr5,hr6)

forecast_r1 <- data.frame(forecast_r1) %>% rownames_to_column(var = "Year") %>% select(Year, Point.Forecast) %>% rename(r1 = Point.Forecast)
forecast_r2 <- data.frame(forecast_r2) %>% rownames_to_column(var = "Year") %>% select(Year, Point.Forecast) %>% rename(r2 = Point.Forecast)
forecast_r3 <- data.frame(forecast_r3) %>% rownames_to_column(var = "Year") %>% select(Year, Point.Forecast) %>% rename(r3 = Point.Forecast)
forecast_r4 <- data.frame(forecast_r4) %>% rownames_to_column(var = "Year") %>% select(Year, Point.Forecast) %>% rename(r4 = Point.Forecast)
forecast_r5 <- data.frame(forecast_r5) %>% rownames_to_column(var = "Year") %>% select(Year, Point.Forecast) %>% rename(r5 = Point.Forecast)
forecast_r6 <- data.frame(forecast_r6) %>% rownames_to_column(var = "Year") %>% select(Year, Point.Forecast) %>% rename(r6 = Point.Forecast)
forecast_all <- list(forecast_r1,forecast_r2,forecast_r3,forecast_r4,forecast_r5,forecast_r6) %>%
  reduce(full_join, by = "Year") %>%
  mutate(Damage = r1+r2+r3+r4+r5+r6) %>%
  select(Year,Damage)

### 3.3 -  main model ###
#annual model, treating region 2 and 3 as high-risk for now
gdp20 <- sum(dem_data$`GDP, 2020 (Ꝕ1,000 )`)*1000
gdp_data <- inflation_data %>% 
  mutate("Forecast GDP" = gdp20*cumprod(1+inflation_data$Point.Forecast)) %>% 
  rename("Forecast Inflation" = Point.Forecast) %>%
  format(scientific = FALSE) %>%
  mutate("Budget" = as.numeric(`Forecast GDP`)*0.02) #set budget as 2% of GDP for now

#cost per proactive relocation for region 2, 3. Using median home value for now.  NEED TO MODEL 
dem_data_r2 <- dem_data["Region 2",]
dem_data_r3 <- dem_data["Region 3",]
pc2 <- dem_data_r2$`Median Value of Owner-Occupied Housing Units`[1] 
pc3 <- dem_data_r3$`Median Value of Owner-Occupied Housing Units`[1]

#number of households
h2 <- dem_data_r2$`Housing Units`[1] 
h3 <- dem_data_r3$`Housing Units`[1] 

#number of owner-occupied units/households
oh2 <- dem_data_r2$`Owner-Occupied Housing Units`[1]
oh3 <- dem_data_r3$`Owner-Occupied Housing Units`[1] 
#to do: model distribution, moving allowance?

#distribute budget by relative claim costs
forecast_r2_scheme <- forecast_r2 %>% 
  mutate(RelBudget = r2/forecast_all$Damage) %>% 
  mutate(Budget = RelBudget*gdp_data$Budget[1:20]) %>%
  mutate(PC = Budget - r2) %>%
  rename(EC = r2)

forecast_r3_scheme <- forecast_r3 %>% 
  mutate(RelBudget = r3/forecast_all$Damage) %>% 
  mutate(Budget = RelBudget*gdp_data$Budget[1:20]) %>%
  mutate(PC = Budget - r3) %>%
  rename(EC = r3)

#derive number of relocation based on estimated cost of EC and PC
ec2 <- pc2*1.6*1.25 + 6*
  dem_data_r2$`Temporary housing cost with disaster (per person per month)`*
  dem_data_r2$`Persons per Household, 2016-2020` #6 month temporary housing allowance, middle estimate for increased material cost and replacing lost items

forecast_r2_scheme <- forecast_r2_scheme %>%
  mutate(nEC = floor(EC/ec2)) %>%
  mutate(nPC = floor(PC/pc2)) %>%
  mutate(nOH = oh2-cumsum(nPC)) %>%
  mutate(nH = h2-cumsum(nPC))

ec3 <- pc3*1.6*1.25 + 6*
  dem_data_r3$`Temporary housing cost with disaster (per person per month)`*
  dem_data_r3$`Persons per Household, 2016-2020`

forecast_r3_scheme <- forecast_r3_scheme %>%
  mutate(nEC = floor(EC/ec3)) %>%
  mutate(nPC = floor(PC/pc3)) %>%
  mutate(nOH = oh3-cumsum(nPC)) %>%
  mutate(nH = h3-cumsum(nPC))

#account for decreasing EC due to PC relocation: assume risk exposure is proportional to number of households
for (i in c(2:20)){
  forecast_r2_scheme$EC[i] <- forecast_r2_scheme$EC[i]*forecast_r2_scheme$nH[i-1]/h2
  forecast_r2_scheme$PC[i] <- forecast_r2_scheme$Budget[i]-forecast_r2_scheme$EC[i]
  forecast_r2_scheme$nEC[i] <- floor(forecast_r2_scheme$EC[i]/ec2)
  forecast_r2_scheme$nPC[i] <- floor(forecast_r2_scheme$PC[i]/pc2)
  forecast_r2_scheme$nH[i] <- forecast_r2_scheme$nH[i-1] - forecast_r2_scheme$nPC[i]
}

for (i in c(2:20)){
  forecast_r3_scheme$EC[i] <- forecast_r3_scheme$EC[i]*forecast_r3_scheme$nH[i-1]/h3
  forecast_r3_scheme$PC[i] <- forecast_r3_scheme$Budget[i]-forecast_r3_scheme$EC[i]
  forecast_r3_scheme$nEC[i] <- floor(forecast_r3_scheme$EC[i]/ec3)
  forecast_r3_scheme$nPC[i] <- floor(forecast_r3_scheme$PC[i]/pc3)
  forecast_r3_scheme$nH[i] <- forecast_r3_scheme$nH[i-1] - forecast_r3_scheme$nPC[i]
}

#calculate NPV of costs for region 2 and 3
npv <- function(x, r){sum(as.numeric(x)/cumprod(1+as.numeric(r)))}
forecast_r2_scheme <- forecast_r2_scheme %>% mutate(Damage = EC + PC)
forecast_r3_scheme <- forecast_r3_scheme %>% mutate(Damage = EC + PC)

#NPV without scheme
npv_noscheme <- npv(forecast_all$Damage, gdp_data$`Forecast Inflation`)

#NPV with scheme
npv_scheme <- npv(forecast_r1$r1, gdp_data$`Forecast Inflation`) + 
  npv(forecast_r2_scheme$r2, gdp_data$`Forecast Inflation`) + 
  npv(forecast_r3_scheme$r3, gdp_data$`Forecast Inflation`) + 
  npv(forecast_r4$r4, gdp_data$`Forecast Inflation`) +
  npv(forecast_r5$r5, gdp_data$`Forecast Inflation`) +
  npv(forecast_r6$r6, gdp_data$`Forecast Inflation`)

npv_noscheme
npv_scheme

#climate scenario multipliers

#calculate NPV of costs
npv <- function(x, r){
  sum(x/cumprod(1+as.numeric(r)))
}

npv(forecast_all$Damage, gdp_data$`Forecast Inflation`)
npv(forecast_r2$EC, gdp_data$`Forecast Inflation`)


### GLM ###

### Splitting Data Set into Validation, Train and Test ###
# Training is first 45 years, 15 of which are validation, last 15 years are test set

### training set ###
hrmin_cost_train <- hr_cost %>% filter(Year %in% c(1960:2005), `Hazard Event` == "Minor") %>% mutate_at(vars(Region), as.factor)
hrmed_cost_train <- hr_cost %>% filter(Year %in% c(1960:2005), `Hazard Event` == "Medium") %>% mutate_at(vars(Region), as.factor)
hrmaj_cost_train <- hr_cost %>% filter(Year %in% c(1960:2005), `Hazard Event` == "Major") %>% mutate_at(vars(Region), as.factor)
hrmin_count_train <- hr_count %>% filter(Year %in% c(1960:2005), `Hazard Event` == "Minor") %>% mutate_at(vars(Region), as.factor)
hrmed_count_train <- hr_count %>% filter(Year %in% c(1960:2005), `Hazard Event` == "Medium") %>% mutate_at(vars(Region), as.factor)
hrmaj_count_train <- hr_count %>% filter(Year %in% c(1960:2005), `Hazard Event` == "Major") %>% mutate_at(vars(Region), as.factor)

### test set ###
hrmin_cost_test <- hr_cost %>% filter(Year %in% c(2006:2020), `Hazard Event` == "Minor") %>% mutate_at(vars(Region), as.factor)
hrmed_cost_test <- hr_cost %>% filter(Year %in% c(2006:2020), `Hazard Event` == "Medium") %>% mutate_at(vars(Region), as.factor)
hrmaj_cost_test <- hr_cost %>% filter(Year %in% c(2006:2020), `Hazard Event` == "Major") %>% mutate_at(vars(Region), as.factor)
hrmin_count_test <- hr_count %>% filter(Year %in% c(2006:2020), `Hazard Event` == "Minor") %>% mutate_at(vars(Region), as.factor)
hrmed_count_test <- hr_count %>% filter(Year %in% c(2006:2020), `Hazard Event` == "Medium") %>% mutate_at(vars(Region), as.factor)
hrmaj_count_test <- hr_count %>% filter(Year %in% c(2006:2020), `Hazard Event` == "Major") %>% mutate_at(vars(Region), as.factor)

### Severity ###
library(statmod)
library(caret)
library(glmnet)
library(pROC)

#define function to output model evalution statistics
error_severity <- function(model, pred, res, x_train, y_test) {
  acc <- colMeans((exp(pred) - exp(y_test))^2)
  AIC <- nrow(x_train)*log(t(res)%*%as.matrix(res)) + 2*(model$df.residual)
  BIC <- nrow(x_train)*log(t(res)%*%as.matrix(res)) + (model$df.residual)*log(nrow(x_train))
  result <- as.table(t(c(acc, AIC, BIC)))
  colnames(result) <- c("Validation", "AIC", "BIC")
  return(result)
}

#fit GLM
glm_severity <- function (traindata, testdata) {
  #fit and output
  glmgamma <- glm(log(`Property Damage`) ~ Year + Region, data=traindata, family=Gamma(link="log"), maxit=100)
  predgamma <- exp(predict(glmgamma, newdata = testdata))
  
  #evaluate fit
  resgamma <-  traindata %>% select(`Property Damage`) %>% mutate(`Property Damage` = log(`Property Damage`)) - (predict(glmgamma, newdata = traindata))
  resgamma <- exp(resgamma$`Property Damage`) - exp(traindata$`Property Damage`)
  accgamma <- error_severity(glmgamma, predgamma, resgamma, exp(traindata %>% select(`Property Damage`)), exp(testdata %>% select(`Property Damage`)))
  rownames(accgamma) <- "glm - gamma"
  return(list(accuracy = accgamma, modelGamma = glmgamma))
}

glm_severity_output_min <- glm_severity(hrmin_cost_train, hrmin_cost_test)
glm_severity_output_med <- glm_severity(hrmed_cost_train, hrmed_cost_test)
glm_severity_output_maj <- glm_severity(hrmaj_cost_train, hrmaj_cost_test)

### Frequency ###
#Frequency Modeling
error_frequency <- function(pred,test) {
  # Calculate the deviance and Pearson chi-squared statistic
  dev <- sum(2 * (test$`Event Count` * log(pred) - pred))
  chisq <- sum((test$`Event Count` - pred)^2 / pred)
  
  # Create a table of the confusion matrix
  confmatrix <- matrix(0, nrow = 2, ncol = 2)
  predclass <- ifelse(pred > 0.5, 1, 0)
  trueclass <- ifelse(test$`Event Count` > 0, 1, 0)
  confmatrix[1,1] <- sum(trueclass == 0 & predclass == 0)
  confmatrix[1,2] <- sum(trueclass == 0 & predclass == 1)
  confmatrix[2,1] <- sum(trueclass == 1 & predclass == 0)
  confmatrix[2,2] <- sum(trueclass == 1 & predclass == 1)
  
  # Calculate the overall accuracy and other metrics
  accuracy <- (confmatrix[1,1] + confmatrix[2,2]) / sum(confmatrix)
  sensitivity <- confmatrix[2,2] / sum(confmatrix[2,])
  specificity <- confmatrix[1,1] / sum(confmatrix[1,])
  
  # Create a table of the results
  result <- matrix(c(dev, chisq, accuracy, sensitivity, specificity), nrow = 1, ncol = 5)
  colnames(result) <- c("Deviance", "Chi-Squared", "Accuracy", "Sensitivity", "Specificity")
  rownames(result) <- paste("glm", " - Poisson", sep = "")
  
  # Return the results as a list
  list(matrix = confmatrix, accuracy = result)
}

# GLM (Poisson)
glm_frequency <- function(traindata, testdata) {
  glm_poisson <- glm(`Event Count` ~ Year + Region, data = traindata, family = "poisson")
  pred_poisson <- predict(glm_poisson, newdata = testdata, type = "response")
  
  error <- error_frequency(pred_poisson, testdata)
  rownames(error$accuracy) <- "glm - poisson"
  
  list(matrix = error$matrix, accuracy = error$accuracy, modelPoisson = glm_poisson)
}

glm_frequency_output_min <- glm_frequency(hrmin_count_train, hrmin_count_test)
glm_frequency_output_med <- glm_frequency(hrmed_count_train, hrmed_count_test)
glm_frequency_output_maj <- glm_frequency(hrmaj_count_train, hrmaj_count_test)

### final model outputs ###
pred_severity <- function(traindata) {
  newdata <- expand.grid(Year = 2021:2040, Region = as.factor(1:6))
  glmgamma <- glm(`Property Damage` ~ Year + Region, data=traindata, family=Gamma(link="log"))
  predgamma <- predict(glmgamma, newdata = newdata)
  data.frame(year = newdata$Year, region = newdata$Region, predgamma = predgamma)
}
pred_severity(hrmin_cost_train)
pred_severity(hrmaj_cost_train)
