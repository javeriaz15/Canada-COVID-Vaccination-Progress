getwd()

library(ggplot2) #for elegant data visualization 
library(hrbrthemes)
library(scales)
library(ggpubr) # creating and customizing 'ggplot2'
library(cowplot) # for plotgrid()
library(dplyr) # provides a set of tools for efficiently manipulating datasets in R 
options(scipen=1000) # print numeric values in fixed or exponential notation

setwd("/Users/jz_ar/javworkspace/R/scifaa/test/Final 3")

# world <- read.csv("share-people-fully-vaccinated-covid_0101_1507.csv")
# Bring in data
world <- read.csv("share-people-vaccinated_1dose_covid_cleaneddata_1512_1507.csv")

# remove X145610.annotations
world <- subset(world, select = -c(X145610.annotations) )

colnames(world)[1] <- "Country"
colnames(world)[3] <- "Date"

world$Date <- as.Date(world$Date, format = "%Y-%m-%d")
world_dim <- dim(world)

# All Countries that got vaccinated
str(world)
head(world, 10)
summary(world)#Data Summary Table

world_dim <- dim(world)

# Adding serial number
world$Number <- 1:world_dim[1]
# relocating serial number column
world <- world %>% relocate(Number, .before = Country)

# indexing each country to latest date
world_date <- which(grepl("2021-07-14", world$Date))
countries <- world[c(world_date),]

countries_dim <- dim(countries)
# serial number
countries$Number <- 1:countries_dim[1]
# relocate
countries <- countries %>% relocate(Number, .before = Country)

# Descriptive Statistical Analysis
str(countries)

head(countries, 10)

summary(countries)#Data Summary Table

mean(countries$people_vaccinated_per_hundred, na.rn= FALSE)

median(countries$people_vaccinated_per_hundred, na.rn= FALSE)

min(countries$people_vaccinated_per_hundred, na.rn= FALSE)

max(countries$people_vaccinated_per_hundred, na.rn= FALSE)

quantile(countries$people_vaccinated_per_hundred, na.rn= FALSE, 0.25)

quantile(countries$people_vaccinated_per_hundred, na.rn= FALSE, 0.75)

quantile(countries$people_vaccinated_per_hundred, na.rn= FALSE, probs = c(0.05, 0.95))

quantile(countries$people_vaccinated_per_hundred, na.rn= FALSE)

IQR(countries$people_vaccinated_per_hundred)

var(countries$people_vaccinated_per_hundred) # variance

sd(countries$people_vaccinated_per_hundred)

standardError = sd(countries$people_vaccinated_per_hundred) / 
  sqrt(length(countries$people_vaccinated_per_hundred))


# Data Visualization on World Data
highest_vaccine_per_hundred <- countries %>% slice_max(people_vaccinated_per_hundred)
highest_vaccine_per_hundred

lowest_vaccine_per_hundred <- countries %>% slice_min(people_vaccinated_per_hundred)
lowest_vaccine_per_hundred

countries_desc <- countries %>%arrange(desc(people_vaccinated_per_hundred))

countries_top5 <- head(countries_desc,5)
ggplot(countries_top5, aes(x= reorder(Country, -people_vaccinated_per_hundred), y=people_vaccinated_per_hundred)) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_text(aes(label = round(people_vaccinated_per_hundred, 1)), vjust=-0.5, color="black", size=3) +
  theme_minimal() + 
  labs(title="Countries with Highest Vaccinations", subtitle = "(15/Dec/2020 to 15/Jul/2021)", x="Country", y = "People Vaccinated per Hundred")

countries_low5 <- tail(countries_desc, 5) 
ggplot(countries_low5, aes(x= reorder(Country, people_vaccinated_per_hundred), y=people_vaccinated_per_hundred)) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_text(aes(label = round(people_vaccinated_per_hundred, 1)), vjust=-0.5, color="black", size=3) +
  theme_minimal() + 
  labs(title="Countries with Lowest Vaccinations", subtitle = "(15/Dec/2020 to 15/Jul/2021)", x="Country", y = "People Vaccinated per Hundred")

# CANADA
CAN <- subset(world, Code == "CAN")
dim(CAN)
canada <- CAN[which(grepl("2020-12-15", CAN$Date)):which(grepl("2021-07-15", CAN$Date)), ]

canada_dim <- dim(canada)

# serial number
canada$Number <- 1:canada_dim[1]
# relocate
canada <- canada %>% relocate(Number, .before = Country)
canada <- canada %>% relocate(people_vaccinated_per_hundred, .before = Date)

canada$Dt <- format(as.Date(canada$Date), "%d")
canada$Dt <- as.numeric(canada$Dt)
canada$Mn <- format(as.Date(canada$Date), "%m")
canada$Mn <- as.numeric(canada$Mn)
canada$Month <- format(as.Date(canada$Date), "%m")
canada$Month <- as.numeric(canada$Month)
canada$Month <- month.name[canada$Month]
canada$Yr <- format(as.Date(canada$Date), "%Y")
canada$Yr <- as.numeric(canada$Yr)

# Descriptive Statistical Analysis
str(canada)
head(canada, 10)
summary(canada)#Data Summary Table
mean(canada$people_vaccinated_per_hundred, na.rn= FALSE)
median(canada$people_vaccinated_per_hundred, na.rn= FALSE)
min(canada$people_vaccinated_per_hundred, na.rn= FALSE)
max(canada$people_vaccinated_per_hundred, na.rn= FALSE)
quantile(canada$people_vaccinated_per_hundred, na.rn= FALSE, 0.25)
quantile(canada$people_vaccinated_per_hundred, na.rn= FALSE, 0.75)
quantile(canada$people_vaccinated_per_hundred, na.rn= FALSE, probs = c(0.05, 0.95))
quantile(canada$people_vaccinated_per_hundred, na.rn= FALSE)
IQR(canada$people_vaccinated_per_hundred)
var(canada$people_vaccinated_per_hundred) # variance
sd(canada$people_vaccinated_per_hundred)
standardError <- sd(canada$people_vaccinated_per_hundred) / 
  sqrt(length(canada$people_vaccinated_per_hundred))
round(standardError,2)

# compute the CORRELATION coefficient between -1 and +1
round(cor(canada$people_vaccinated_per_hundred, canada$Number),2)

# Data Visualization

# Date vs vaccinations
ggplotd1 <- ggplot(canada, aes(Date,people_vaccinated_per_hundred)) 
ggplotd2 <- ggplotd1 + theme_minimal() + 
  labs(title="Vaccination Progress in Canada", subtitle = "(15/Dec/2020 to 15/Jul/2021)", x="Days of Vaccinations", y = "People Vaccinated per Hundred") 
dataplotd <- ggplotd2 + geom_point(colour = "steelblue", size = 2)
dataplotd  

# MAIN: Number vs vaccinations 
dataplotn <- ggplot(canada, aes(Number,people_vaccinated_per_hundred)) + theme_minimal() + 
  geom_point(colour = "steelblue", size = 2) + 
  labs(title="Vaccination Progress in Canada",subtitle = "(15/Dec/2020 to 15/Jul/2021)", 
       x="Days of Vaccinations", y = "People Vaccinated per Hundred", 
       caption = "Days of vaccinations on x-axis starts from 15 December 2021 taken as Day 1,  and progress till 15 July, 2021 taken as Day 213.")

dataplotnd <- dataplotn + scale_x_continuous(sec.axis = sec_axis(~., breaks=c(0,50,100,150,200), labels=c('Dec/2020','Feb/2021','Mar/2021','May/2021','Jul/2021')))
dataplotnd

# Year vs vaccination
dataploty <- ggplot(canada, aes(Yr,people_vaccinated_per_hundred)) + theme_minimal() + 
  labs(title="Vaccination Progress in Canada", subtitle = "(15/Dec/2020 to 15/Jul/2021)", x="Days of Vaccinations", y = "People Vaccinated per Hundred") + 
  geom_point(colour = "steelblue", size = 2)
dataploty
# result: linearly increasing relationship

################################################################################

# COMPUTATION
# Logistic regression
# dependent variable (trying to predict) ~ independent variable
# [target variable] ~ [predictor variables]
# [Number] ~ [people_vaccinated_per_hundred]

# A - predicting day (y) from people vaccinated per hundred (x)

model <- lm(Number ~ people_vaccinated_per_hundred, data = canada)
summary(model) #Review the results

AugNormyNormx <- broom::augment_columns(model, data = canada)

# MODEL ACCURACY
sigma(model)*100/mean(canada$people_vaccinated_per_hundred)

################################################################################
# fit a linear model to a scatter plot of our data
reg_plotn <- dataplotnd + 
  labs(title = "Linear Model Fitted to Vaccination Progress in Canada") + 
  stat_smooth(method = "lm", col = "darkred") 
reg_plotn

###############################################################################
# to create data for share of people = 100
canada_test <- subset(canada_t, select = -c(date_t) )

Range <- 382 #12/12/22
Number_t <- 1:Range
date_t <- seq.Date(as.Date("2020-12-15"), length.out = Range, by = "day")
canada_test <- data.frame(Number_t, date_t)
canada_test['people_vaccinated_per_hundred_t'] <- 0

# working on date
canada_test$date_t <- as.Date(canada_test$date_t, format = "%Y-%m-%d")

canada_test$Dt <- format(as.Date(canada_test$date_t), "%d")
canada_test$Dt <- as.numeric(canada_test$Dt)

canada_test$Mn <- format(as.Date(canada_test$date_t), "%m")
canada_test$Mn <- as.numeric(canada_test$Mn)

canada_test$Month <- format(as.Date(canada_test$date_t), "%m")
canada_test$Month <- as.numeric(canada_test$Month)
canada_test$Month <- month.name[canada_test$Month]

canada_test$Yr <- format(as.Date(canada_test$date_t), "%Y")
canada_test$Yr <- as.numeric(canada_test$Yr)

# PREDICTION
# A - the predicted dayth is Y(Day) when people vaccinated per hundred in 100.
# single instance of population

test_ppl <- c(100)
Dayth1 <- predict(model, data.frame(people_vaccinated_per_hundred = test_ppl))
Dayth <- round(Dayth1, digits = 0)
# Dayth
Day <- canada_test$Dt[Dayth]
Month <- canada_test$Month[Dayth]
Year <- canada_test$Yr[Dayth] 
sprintf("Hopefully %s people out of 100 people group will be vaccinated by %s %s, %s", test_ppl, Day, Month, Year)

# Model Summary
modelSummary <- summary(model)  # capture model summary as an object

modelCoeffs <- modelSummary$coefficients  # model coefficients
# a change in one unit in people vaccinated per hundred will bring 2.249 units 
# to change in day

beta.estimate <- modelCoeffs["people_vaccinated_per_hundred", "Estimate"]  # get beta estimate for people vaccinated

std.error <- modelCoeffs["people_vaccinated_per_hundred", "Std. Error"]  # get std.error for people vaccinated

t_value <- beta.estimate/std.error  # calc t statistic
t_value <- round(t_value, digits = 2)
print(paste0("t Value: ", t_value))

p_value <- 2*pt(-abs(t_value), df=nrow(canada)-ncol(canada))  # calc p Value
#p_value <- round(p_value, digits = 2)
print(paste0("p Value: ", p_value))

f_statistic <- modelSummary$fstatistic[1]  # fstatistic
f_statistic <- round(f_statistic, digits = 0)
print(paste0("Model F Statistic: ", f_statistic))

f <- modelSummary$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
print(paste0("Model P Value: ", model_p))

AIC(model)
BIC(model)

# Standard errors and confidence intervals
confint(model)

# MODEL ACCURACY
sigma(model)*100/mean(canada$people_vaccinated_per_hundred)

####
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
