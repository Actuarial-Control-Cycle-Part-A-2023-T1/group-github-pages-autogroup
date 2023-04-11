###setwd("~/GitHub/ACTL4001-Assignment")

### import packages ### 
library(tidyverse)
library(readxl)

### data prep and cleaning ###
#load data
econ_dem <- as.data.frame(read_excel("2023-student-research-eco-dem-data.xlsx", sheet = "Demographic-Economic", range = "B8:H51"))
hazard <- as.data.frame(read_excel("2023-student-research-hazard-event-data.xlsx", range = "B13:I3379"))

#econ data prep
#organise rows and columns, convert formats
econ_dem <- as.data.frame(t(econ_dem))
colnames(econ_dem) <- econ_dem[1,]
econ_dem <- econ_dem[-1,]
econ_dem[] <- lapply(econ_dem, function(x) gsub("Ꝕ ", "", x))
econ_dem[] <- lapply(econ_dem, function(x) as.numeric(gsub(",", "", x)))

hazard[,c(1:3)] <- lapply(hazard[,c(1:3)], as.factor)
hazard$`Hazard Event`[hazard$`Hazard Event` %in% c("Severe Storm/Thunder Storm - Wind")] <- "Severe Storm/Thunder Storm/ Wind"

#hazard data prep
#reduce cardinality by grouping multi-hazard events into most severe contributing hazard
hazard$`Hazard Event` <- as.character(hazard$`Hazard Event`)
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                            c("Hurricane/Tropical Storm",
                              "Coastal/ Hurricane/Tropical Storm/ Wind",
                              "Coastal/ Hurricane/Tropical Storm/ Severe Storm/Thunder Storm/ Wind",
                              "Hurricane/Tropical Storm/ Severe Storm/Thunder Storm")] <- "Hurricane"
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                            c("Drought/ Heat")] <- "Drought"
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                            c("Hail/ Wind")] <- "Hail"
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                            c("Wind/ Winter Weather")] <- "Winter Weather"
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                            c("Coastal/ Flooding",
                              "Coastal/ Wind")] <- "Coastal"
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                            c("Flooding/ Hail",
                              "Flooding/ Hail/ Wind",
                              "Flooding/ Lightning",
                              "Flooding/ Lightning/ Wind",
                              "Flooding/ Wind")] <- "Flooding"
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                            c("Hail/ Tornado",
                              "Hail/ Tornado/ Wind",
                              "Lightning/ Tornado/ Wind",
                              "Tornado/ Wind")] <- "Tornado"
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                            c("Hail/ Lightning",
                              "Hail/ Lightning/ Wind",
                              "Lightning/ Wind")] <- "Tornado"
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
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

### EDA ###
#analysis of hazards
hazard_count <- hazard %>%
  group_by(`Hazard Event`) %>%
  summarise(Count = n(), AverageCost = mean(`Property Damage`), AverageFatalities = mean(`Fatalities`), AverageInjuries = mean(`Injuries`), AverageDuration = mean(`Duration`))

#examine most catastrohpic events
hr_gross_max <- hazard %>% arrange(desc(`Property Damage`))
head(hr_gross_max, 10) #looking at the data, we would need to potentially remove some outliers, but also since so many outliers are for 2, may indicate that there are more expensive regions? does not match up with the demographic data however 
ggplot() + 
  aes(hr_gross_max$`Property Damage`) + 
  geom_density()

ggplot() + 
  aes(hr_gross_max$`Property Damage`[300:nrow(hr_gross_max)]) + 
  geom_density() #significantly grouped towards lower values

hr_gross_max_r2 <- hr_gross_max %>%
  filter(Region == 2)

ggplot() + 
  aes(hr_gross_max_r2$`Property Damage`[70:nrow(hr_gross_max_r2)]) + 
  geom_density() #cutting the top 10% of values will yield a similar distribution to overall, but those 10% may not be ignorable if they are consistently from region 2
rm(hr_gross_max_r2)

#further reduce cardinality by categorising events into 3 severities, as per SSP scenarios
hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                        c("Hurricane",
                          "Wildfire")] <- "Major"

hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                        c("Drought",
                          "Flooding",
                          "Tornado",
                          "Winter Weather")] <- "Medium"

hazard$`Hazard Event`[hazard$`Hazard Event` %in% 
                        c("Lightning",
                          "Wind",
                          "Severe Storm",
                          "Coastal",
                          "Fog",
                          "Hail",
                          "Heat",
                          "Landslide")] <- "Minor"

#matrix of regions vs hazards (number of each event per region)
hr1 <- hazard %>% filter(Region == 1) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 1")
hr2 <- hazard %>% filter(Region == 2) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 2")
hr3 <- hazard %>% filter(Region == 3) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 3")
hr4 <- hazard %>% filter(Region == 4) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 4")
hr5 <- hazard %>% filter(Region == 5) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 5")
hr6 <- hazard %>% filter(Region == 6) %>% count(`Hazard Event`) %>% rename_at(2,~"Region 6")

hr_count_byevent <- list(hr1,hr2,hr3,hr4,hr5,hr6) %>%
  reduce(full_join, by = "Hazard Event")
rm(hr1,hr2,hr3,hr4,hr5,hr6)

# summarise by regions, sum damages by year
year_index <- data.frame(Year  = as.numeric(1960:2020))
hr1 <- hazard %>% filter(Region == 1)
hr1 <- aggregate(hr1$`Property Damage`, by = list(Year = hr1$Year), FUN = sum) %>% 
  rename("Region 1" = x) %>%
  full_join(year_index, by = "Year") %>%
  arrange(Year) %>%
  replace(is.na(.), 0)

hr2 <- hazard %>% filter(Region == 2)
hr2 <- aggregate(hr2$`Property Damage`, by = list(Year = hr2$Year), FUN = sum) %>% 
  rename("Region 2" = x) %>%
  full_join(year_index, by = "Year") %>%
  arrange(Year) %>%
  replace(is.na(.), 0)

hr3 <- hazard %>% filter(Region == 3)
hr3 <- aggregate(hr3$`Property Damage`, by = list(Year = hr3$Year), FUN = sum) %>% 
  rename("Region 3" = x) %>%
  full_join(year_index, by = "Year") %>%
  arrange(Year) %>%
  replace(is.na(.), 0)

hr4 <- hazard %>% filter(Region == 4)
hr4 <- aggregate(hr4$`Property Damage`, by = list(Year = hr4$Year), FUN = sum) %>% 
  rename("Region 4" = x) %>%
  full_join(year_index, by = "Year") %>%
  arrange(Year) %>%
  replace(is.na(.), 0)

hr5 <- hazard %>% filter(Region == 5)
hr5 <- aggregate(hr5$`Property Damage`, by = list(Year = hr5$Year), FUN = sum) %>% 
  rename("Region 5" = x) %>%
  full_join(year_index, by = "Year") %>%
  arrange(Year) %>%
  replace(is.na(.), 0)

hr6 <- hazard %>% filter(Region == 6)
hr6 <- aggregate(hr6$`Property Damage`, by = list(Year = hr6$Year), FUN = sum) %>% 
  rename("Region 6" = x) %>%
  full_join(year_index, by = "Year") %>%
  arrange(Year) %>%
  replace(is.na(.), 0)

hr_cost <- list(hr1,hr2,hr3,hr4,hr5,hr6) %>%
  reduce(full_join, by = "Year")
rm(hr1,hr2,hr3,hr4,hr5,hr6,year_index)

#gross data (sum to date)
hr_gross <- hazard %>%
  group_by(Region) %>%
  summarise(Count = n(), GrossCost = sum(`Property Damage`), GrossFatalities = sum(Fatalities), GrossInjuries = sum(Injuries))

#count of disasters per region
hr_count <- hazard %>%
  group_by(Region) %>%
  summarise(Count = n(), 
            AverageCost = mean(`Property Damage`), 
            AverageFatalities = mean(Fatalities), 
            AverageInjuries = mean(Injuries))

ggplot(hr_count) + 
  aes(x = Region, y = Count) + 
  geom_col() + 
  ylab("Count of Disastrous Events")

#correcting for population to find what area has most hazards per population to get better idea of impact on people?
hr_count$PopCount <- hr_count$Count/econ_dem$`Census, July 1, 2021`

ggplot(hr_count) + 
  aes(x = Region, y = PopCount) + 
  geom_col() + 
  ylab("Count of Disastrous Events Corrected For Population Size")

#average cost of disasters per region
ggplot(hr_count) + 
  aes(x = Region, y = AverageCost) + 
  geom_col() + 
  ylab("Average Cost of Property Damage From Disastrous Events in Region") 



ggplot(hr_count) + 
  aes(x = Region, y = AverageFatalities) + 
  geom_col() + 
  ylab("Average Fatalities From Disastrous Events in Region") #much more even compared to below graph, outliers are nowhere near the 3831 injuries in one event 

ggplot(hr_count) + 
  aes(x = Region, y = AverageInjuries) + 
  geom_col() + 
  ylab("Average Injuries From Disastrous Events in Region") #massive outlier in region 5

head(hazard %>%
  filter(Region == 5) %>%
  arrange(desc(Injuries))) #again a massive outlier event skewing results for region 5 in the above comparison, 3831 injuries compared to 117, remove from dataset? 

ggplot(hazard %>%
         filter(Injuries < 3000) %>%
         group_by(Region) %>%
         summarise(AverageInjuries = mean(Injuries))) +
  aes(x = Region, y = AverageInjuries) + 
  geom_col() + 
  ylab("Average Injuries From Disastrous Events in Region (Excluding One Outlier)")

#other things to look at in EDA yet to do: change in disaster counts/disaster severity over time, seasonality
yearquarter_count  <- hazard %>%
  unite(YQ, c(Year, Quarter), sep="") %>%
  group_by(YQ) %>%
  summarise(Count = n()) 

year_count <- hazard %>%
  group_by(Year) %>%
  summarise(Count = n(), AverageSeverity = mean(`Property Damage`))

quarter_count <- hazard %>%
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

### Modeling ###
# import packages 
library(forecast)

# economic indicies
# load data
econ_data <- as.data.frame(read_excel("2023-student-research-eco-dem-data.xlsx",
                                      sheet = 3, range = "B8:F68"))

# forward fill missing values
econ_data <- econ_data %>% fill(colnames(econ_data))

# weird -990% inflation in 2003, impute with last known value
econ_data[econ_data$Year == 2003, ]$Inflation <- econ_data[econ_data$Year == 2002, ]$Inflation

ggplot(econ_data) +
  aes(x = Year, y = Inflation) +
  geom_line()

# generate an ewma (exponentially weighted moving average, reduces noise in data)
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

# define forecasting function, o is order of arima model, and n is number of forecasted times to be made
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

# define an arbitrary decay constant, 0.1 seems to fit reasonably well
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

# hazard data
# forecasting
decay <- 0.1
order <- c(5, 1, 1)

hr1 <- select(hr_cost,Year,`Region 1`)
forecast_r1 <- tsforecast(hr1, colnames(hr1)[2], decay, order)
plot(forecast_r1)


hr2 <- select(hr_cost,Year,`Region 2`)
forecast_r2 <- tsforecast(hr2, colnames(hr2)[2], decay, order)
plot(forecast_r2)

hr3 <- select(hr_cost,Year,`Region 3`)
forecast_r3 <- tsforecast(hr3, colnames(hr3)[2], decay, order)
plot(forecast_r3)

hr4 <- select(hr_cost,Year,`Region 4`)
forecast_r4 <- tsforecast(hr4, colnames(hr4)[2], decay, order)
plot(forecast_r4)

hr5 <- select(hr_cost,Year,`Region 5`)
forecast_r5 <- tsforecast(hr5, colnames(hr5)[2], decay, order)
plot(forecast_r5)

hr6 <- select(hr_cost,Year,`Region 6`)
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

#main model
#treating region 2 and 3 as high-risk for now
#starting with annual model for now

gdp20 <- sum(econ_dem$`GDP, 2020 (Ꝕ1,000 )`)*1000
gdp_data <- inflation_data %>% 
  mutate("Forecast GDP" = gdp20*cumprod(1+inflation_data$Point.Forecast)) %>% 
  rename("Forecast Inflation" = Point.Forecast) %>%
  format(scientific = FALSE) %>%
  mutate("Budget" = as.numeric(`Forecast GDP`)*0.02) #2% of GDP budget for now

#cost per proactive relocation for region 2, 3
#start with median home value.  NEED TO MODEL 
econ_dem_r2 <- econ_dem["Region 2",]
econ_dem_r3 <- econ_dem["Region 3",]
pc2 <- econ_dem_r2$`Median Value of Owner-Occupied Housing Units`[1] 
pc3 <- econ_dem_r3$`Median Value of Owner-Occupied Housing Units`[1]
#number of households
h2 <- econ_dem_r2$`Housing Units`[1] 
h3 <- econ_dem_r3$`Housing Units`[1] 
#number of owner-occupied units/households
oh2 <- econ_dem_r2$`Owner-Occupied Housing Units`[1]
oh3 <- econ_dem_r3$`Owner-Occupied Housing Units`[1] 
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
  econ_dem_r2$`Temporary housing cost with disaster (per person per month)`*
  econ_dem_r2$`Persons per Household, 2016-2020` #6 month temporary housing allowance, middle estimate for increased material cost and replacing lost items

forecast_r2_scheme <- forecast_r2_scheme %>%
  mutate(nEC = floor(EC/ec2)) %>%
  mutate(nPC = floor(PC/pc2)) %>%
  mutate(nOH = oh2-cumsum(nPC)) %>%
  mutate(nH = h2-cumsum(nPC))

ec3 <- pc3*1.6*1.25 + 6*
  econ_dem_r3$`Temporary housing cost with disaster (per person per month)`*
  econ_dem_r3$`Persons per Household, 2016-2020`

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




