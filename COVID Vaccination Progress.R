getwd()

library(ggplot2) #for elegant data visualization 
library(hrbrthemes)
library(scales)
library(ggpubr) # creating and customizing 'ggplot2'
library(cowplot) # for plotgrid()
library(dplyr) # provides a set of tools for efficiently manipulating datasets in R 
options(scipen=1000) # print numeric values in fixed or exponential notation

setwd("/Users/jz_ar/javworkspace/R/scifaa/test")

# Bring in data
share <- read.csv("share-people-vaccinated_1dose_covid_cleaneddata_1512_1507.csv")

# remove X145610.annotations
share <- subset(share, select = -c(X145610.annotations) )

share$Day <- as.Date(share$Day, format = "%Y-%m-%d")
share_dim <- dim(share)

####################################################################################
# All Countries that got vaccinated
str(share)
head(share, 10)
summary(share)#Data Summary Table

share_dim <- dim(share)

# Adding serial number
share$Number <- 1:share_dim[1]
# relocating serial number column
share <- share %>% relocate(Number, .before = Entity)

# indexing each country to latest date
share_day <- which(grepl("2021-07-14", share$Day))
country <- share[c(share_day),]

country_dim <- dim(country)
# serial number
country$Number <- 1:country_dim[1]
# relocate
country <- country %>% relocate(Number, .before = Entity)

# Descriptive Statistical Analysis
str(country)

head(country, 10)

summary(country)#Data Summary Table

mean(country$people_vaccinated_per_hundred, na.rn= FALSE)

median(country$people_vaccinated_per_hundred, na.rn= FALSE)

min(country$people_vaccinated_per_hundred, na.rn= FALSE)

max(country$people_vaccinated_per_hundred, na.rn= FALSE)

quantile(country$people_vaccinated_per_hundred, na.rn= FALSE, 0.25)

quantile(country$people_vaccinated_per_hundred, na.rn= FALSE, 0.75)

quantile(country$people_vaccinated_per_hundred, na.rn= FALSE, probs = c(0.05, 0.95))

quantile(country$people_vaccinated_per_hundred, na.rn= FALSE)

IQR(country$people_vaccinated_per_hundred)

var(country$people_vaccinated_per_hundred) # variance

sd(country$people_vaccinated_per_hundred)

standardError = sd(country$people_vaccinated_per_hundred) / 
  sqrt(length(country$people_vaccinated_per_hundred))

# Graphical display of distributions

# A - Bar chart with barplot(height = quantitative_variable)
par(mfrow=c(2,2))
barplot(country[c(1:30),]$people_vaccinated_per_hundred,
        main = "",
        xlab = "Country",
        ylab = "Vaccinated",
        names.arg = country[c(1:30),]$Entity,
        col = "steelblue", border="lightblue", 
        cex.names=0.5, las=2 
        )

barplot(country[c(31:60),]$people_vaccinated_per_hundred,
        main = "",
        xlab = "Country",
        ylab = "Vaccinated",
        names.arg = country[c(31:60),]$Entity,
        col = "steelblue", border="lightblue", 
        cex.names=0.5, las=2,
)

barplot(country[c(61:90),]$people_vaccinated_per_hundred,
        main = "",
        xlab = "Country",
        ylab = "Vaccinated",
        names.arg = country[c(61:90),]$Entity,
        col = "steelblue", border="lightblue", 
        cex.names=0.5, las=2 
)

barplot(country[c(91:118),]$people_vaccinated_per_hundred,
        main = "",
        xlab = "Country",
        ylab = "Vaccinated",
        names.arg = country[c(91:118),]$Entity,
        col = "steelblue", border="lightblue", 
        cex.names=0.5, las=2 
)


# B - geom_bar in order of country names
# theme grey is default
# plotted bars in alphabetical order
g1 <- ggplot(country[c(1:30),], aes(x=Code, y=people_vaccinated_per_hundred)) + 
  geom_bar(stat = "identity", fill="steelblue")+ theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

g2 <- ggplot(country[c(31:60),], aes(x=Code, y=people_vaccinated_per_hundred)) + 
  geom_bar(stat = "identity", fill="steelblue")+ theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3 <- ggplot(country[c(61:90),], aes(x=Code, y=people_vaccinated_per_hundred)) + 
  geom_bar(stat = "identity", fill="steelblue")+ theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g4 <- ggplot(country[c(91:118),], aes(x=Code, y=people_vaccinated_per_hundred)) + 
  geom_bar(stat = "identity", fill="steelblue")+ theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+ 
  #geom_text(aes(label = signif(people_vaccinated_per_hundred, digits = 3)))

plot_grid(g1, g2, g3, g4 , ncol = 2, nrow = 2)


# stacked bar graph with men and women and kids 
# https://www.learnbyexample.org/r-bar-plot-base-graph/

####################################################################################
# HIGHEST
highest_vaccine_per_hundred <- country %>% slice_max(people_vaccinated_per_hundred)
highest_vaccine_per_hundred

# LOWEST
lowest_vaccine_per_hundred <- country %>% slice_min(people_vaccinated_per_hundred)
lowest_vaccine_per_hundred

# TOP AND LOW 5
country_desc <- country %>%arrange(desc(people_vaccinated_per_hundred))

# top 5
# country_top5 <- country_desc %>% slice(1:5)
country_top5 <- head(country_desc,5)
# ggplot(country_top5, aes(x=Entity, y=people_vaccinated_per_hundred)) + 
#   geom_bar(stat = "identity", fill="steelblue")+ theme_minimal() + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   geom_text(aes(label = round(people_vaccinated_per_hundred, 1)), vjust=1.6, color="white", size=3.5)
# # make is from lowest to highest ????????????????

# lowest 5
# try to start the table from lowest as first position
country_low5 <- tail(country_desc, 5) 
# ggplot(country_low5, aes(x=Entity, y=people_vaccinated_per_hundred)) + 
#   geom_bar(stat = "identity", fill="steelblue")+ theme_minimal() + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   geom_text(aes(label = round(people_vaccinated_per_hundred, 1)), vjust=1.6, color="white", size=3.5)

# bar plot for top 5 and lowest five for people in ascending order
# top 5 countries 
par(mfrow=c(1,2))
barplot(country_top5$people_vaccinated_per_hundred,
        main = "Countries with highest vaccinations",
        xlab = "Country",
        ylab = "Vaccinated",
        names.arg = country_top5$Entity,
        col = "steelblue", border="lightblue", 
        cex.names=0.5, las=2 
)

# lowest 5 countries
barplot(country_low5$people_vaccinated_per_hundred,
        main = "Countries with lowest vaccinations",
        xlab = "Country",
        ylab = "Vaccinated",
        names.arg = country_low5$Entity,
        col = "steelblue", border="lightblue", 
        cex.names=0.5, las=2 
)

#######################################################################
# CANADA
CAN <- subset(share, Code == "CAN")
dim(CAN)
# USA <- subset(Share, Code == "USA")

# indexing according to date
# row1 <- which(grepl("2020-12-15", CAN$Day)) # row2 <- which(grepl("2021-07-15", CAN$Day)) # CAN1 <- CAN[row1:row2, ]
CANADA <- CAN[which(grepl("2020-12-15", CAN$Day)):which(grepl("2021-07-15", CAN$Day)), ]

# require(dplyr)
CANADA <- CAN %>% relocate(Day, .before = Entity)

canada_dim <- dim(CANADA)
# serial number
CANADA$Number <- 1:canada_dim[1]
# relocate
CANADA <- CANADA %>% relocate(Number, .before = Day)

# Descriptive Statistical Analysis
str(CANADA)
head(CANADA, 10)
summary(CANADA)#Data Summary Table
mean(CANADA$people_vaccinated_per_hundred, na.rn= FALSE)
median(CANADA$people_vaccinated_per_hundred, na.rn= FALSE)
min(CANADA$people_vaccinated_per_hundred, na.rn= FALSE)
max(CANADA$people_vaccinated_per_hundred, na.rn= FALSE)
quantile(CANADA$people_vaccinated_per_hundred, na.rn= FALSE, 0.25)
quantile(CANADA$people_vaccinated_per_hundred, na.rn= FALSE, 0.75)
quantile(CANADA$people_vaccinated_per_hundred, na.rn= FALSE, probs = c(0.05, 0.95))
quantile(CANADA$people_vaccinated_per_hundred, na.rn= FALSE)
IQR(CANADA$people_vaccinated_per_hundred)
var(CANADA$people_vaccinated_per_hundred) # variance
sd(CANADA$people_vaccinated_per_hundred)
standardError <- sd(CANADA$people_vaccinated_per_hundred) / 
  sqrt(length(CANADA$people_vaccinated_per_hundred))
round(standardError,2)

# compute the CORRELATION coefficient between -1 and +1
round(cor(CANADA$people_vaccinated_per_hundred, CANADA$Number),2)

# Graphical display of distributions

# A- Scatter plot using ggplot
#gg <- ggplot(CANADA, aes(people_vaccinated_per_hundred, Day)) + labs(title="Vaccination Progress in Canada",
#x="people_vaccinated_per_hundred", y = "Day")

gg <- ggplot(CANADA, aes(Day,people_vaccinated_per_hundred)) + theme_minimal() + 
  labs(title="Vaccination Progress in Canada", x="Day", y = "people_vaccinated_per_hundred")
sp <- gg + geom_point(colour = "steelblue", size = 2)
sp

# result: linearly increasing relationship

# B- Using Scatter.smooth
scatter.smooth(x=CANADA$Day, y=CANADA$people_vaccinated_per_hundred, main="Vaccination Progress in Canada")  # scatterplot

# C- histogram
ggplot(CANADA, aes(x=people_vaccinated_per_hundred)) + theme_minimal() + 
  geom_histogram(binwidth=1, color="steelblue", fill="lightblue")

# Histogram with density plot
h <- ggplot(CANADA, aes(x=people_vaccinated_per_hundred)) + theme_minimal() + 
  geom_histogram(aes(y=..density..),binwidth=1, color="steelblue", fill="lightblue")+
  geom_density(alpha=.2, fill="#FF6666")

# Add mean line
h + geom_vline(aes(xintercept=mean(people_vaccinated_per_hundred)), color="red", linetype="dashed", size=1)

################################################################################

# COMPUTATION
# Logistic regression
# [target variable] ~ [predictor variables]
# my choice 
fitCAN <- lm(Number ~ people_vaccinated_per_hundred, data = CANADA)
summary(fitCAN) #Review the results

# reference project option
# fitUS <- glm(Adjusted ~ Day, data = US, family = "quasibinomial")
fitCAN2 <- glm(Number ~ people_vaccinated_per_hundred, data = CANADA)
summary(fitCAN2) #Review the results

################################################################################
# Add the regression line on the scatter plot stat_smooth()
rl <- sp + stat_smooth(method = lm, color="darkred") #smooth line with confidence band
rl

###############################################################################
# CALCULATE T Statistic and p-Values of fitCAN
# https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
# http://r-statistics.co/Linear-Regression.html

modelSummary <- summary(fitCAN)  # capture model summary as an object

modelCoeffs <- modelSummary$coefficients  # model coefficients

beta.estimate <- modelCoeffs["people_vaccinated_per_hundred", "Estimate"]  # get beta estimate for people vaccinated

std.error <- modelCoeffs["people_vaccinated_per_hundred", "Std. Error"]  # get std.error for people vaccinated

t_value <- beta.estimate/std.error  # calc t statistic
t_value <- round(t_value, digits = 2)
print(paste0("t Value: ", t_value))

p_value <- 2*pt(-abs(t_value), df=nrow(CANADA)-ncol(CANADA))  # calc p Value
#p_value <- round(p_value, digits = 2)
print(paste0("p Value: ", p_value))

f_statistic <- modelSummary$fstatistic[1]  # fstatistic
f_statistic <- round(f_statistic, digits = 0)
print(paste0("Model F Statistic: ", f_statistic))

f <- modelSummary$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
print(paste0("Model P Value: ", model_p))

AIC(fitCAN)
BIC(fitCAN)

# Standard errors and confidence intervals
confint(fitCAN)

# MODEL ACCURACY
sigma(fitCAN)*100/mean(CANADA$people_vaccinated_per_hundred)