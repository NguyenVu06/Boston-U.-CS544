#Load packages
library(dplyr)
library(stats)
library(car)
library(emmeans)
library(lsmeans)
library(tcltk)
library(asbio)
library(moments)
library(ggplot2)
library(ggcorrplot)

#load Data File
#Location of data on Kaggle
#https://www.kaggle.com/goldenoakresearch/us-acs-mortgage-equity-loans-rent-statistics
#colums of interests:
#c(1, 6, 7, 14:18, 22, 25, 37, 40, 47, 50, 52, 54)
#read in giant CSV file (80 variables and 39030 observations):
masterDT <- read.csv("real_estate_db.csv", header = T)
# verify the names of the columns of interests above before subsetting:
vars_names <- names(masterDT[,c(1, 6, 7, 14:18, 22, 37, 47, 52)])
vars_names

#subset columns that contains columns of interests and replace the original df with the smaller df
masterDT <- masterDT[,c(1, 6, 7, 14:18, 22, 37, 47, 52)]#now data is reduced to 12 variables and 39030 observations
#remove rows of incomplete cases
masterDT <- masterDT[complete.cases(masterDT),] #data is reduced to 12 variables and 37942 observations
#create a back up of the data frame incase there is an error during analysis. 
masterDT_backup <- masterDT

#rename columns to be more readable:
new_vars_names <- gsub("hc", "Owner_Costs", vars_names)
new_vars_names <- gsub("hi_", "Household_Income_", new_vars_names)
names(masterDT) <- new_vars_names

#Create a varible to define the continental US vs states like Hawaii, Alaska and Puerto Rico to accurately
#study at the effect longitude and latitude
# Continental state = 1, non-continental states = 0
masterDT$Continental <- ifelse(masterDT$state_ab %in% c("HI", "AK", "PR"), 0, 1)

# Create a value to define the the ratio of water to land ratio of specific geographic area
masterDT$water_land_ratio <- masterDT$AWater/masterDT$ALand

#creat a subset of just the continental US for analysis for Hawaii, Alaska and Puertorico can be considered geographically outliers
contUSdf <- filter(masterDT, Continental !=0) #18 variables and 37228 observations
attach(contUSdf)#attach to memory for ease of analysis

# Arrange states by average latitude to explore the data
contUSdf %>% group_by(state) %>% summarise(avg_latitute = mean(lat), avg_longitude = mean(lng)) %>% arrange(avg_latitute)

# Arrange states by average longitude to explore the data
contUSdf %>% group_by(state) %>% summarise(avg_latitute = mean(lat), avg_longitude = mean(lng)) %>% arrange(avg_longitude)


# Let longitude of the US be devided into 3 equal segments.
#Westcoast will be defined as lowest 1/3rd segment, Eastcoast will be the higest 1/3rd segment and Central will be the middle segment
lng_seg<- (max(contUSdf$lng)-min(contUSdf$lng))/3
contUSdf$Coast <- cut(contUSdf$lng, breaks = c(-Inf, min(contUSdf$lng)+lng_seg,  min(contUSdf$lng)+2*lng_seg, Inf  ),
                      labels = c("West", "Middle", "East"))

#let latitude of the US be divided into 2 equal segments,
# North will be defined as the highest half and the south will be defined as the lowest half.
contUSdf$Region <- cut(contUSdf$lat, breaks = c(-Inf, mean(contUSdf$lat), Inf), 
                       labels = c("South", "North"))


# Calculate for the Population Density by divigin population by land Area
contUSdf$Pop_density <- contUSdf$pop/contUSdf$ALand

###SIMPLE DATA
#Which State has the lowest average median cost for owner interm of Mortgage and cost?
s<- contUSdf %>% group_by(state) %>% summarise(avg_mortgage_plus_cost = mean(Owner_Costs_mortgage_median), 
                                               avg_med_rent = mean(rent_median))
s %>% arrange(avg_mortgage_plus_cost)%>% slice(1:3)
#which State has the highest average median cost for owner of Mortgage and cost?
s %>% arrange(desc(avg_mortgage_plus_cost))%>% slice(1:3)

#Which state has the lowest average median gross median rent?
s %>% arrange(avg_med_rent)%>% slice(1:3)
#Which state has the highest average median gross median rent?
s %>% arrange(desc(avg_med_rent))%>% slice(1:3)


# get a table of averages summary of the average of Rent, Owner Cost, Owner Costs Mortgage and household Income by Coasts and region
avg_table <- contUSdf%>% group_by(Coast, Region) %>% summarise(US_avg_median_rent = mean(rent_median),
                                                               US_avg_median_Owner_Costs = mean(Owner_Costs_median),
                                                               US_avg_median_Owner_Costs_n_Mortgage = mean(Owner_Costs_mortgage_median),
                                                               US_avg_median_householdIncome = mean(Household_Income_median))
p <- ggplot(data = contUSdf)

p + geom_boxplot(aes(x = Coast, y = rent_median)) + facet_grid(Region~.) + xlab("Coast") + ylab("Median Rent")


#### Gross Rent Analysis by Location: East VS West VS Middle coasts and North VS South region
#Compare the average median rent the Northern VS. Sourthern US, and East Coast Vs. West Coast VS. Middle at the 95% CI:

summary(aov(rent_median~Region)) #not significant in global F test.
p + geom_boxplot(aes(x = Region, y = rent_median)) #Region ONly Plot
#Compare the average median rent by coasts alone
summary(aov(rent_median~Coast)) #Global F Test is significant
p + geom_boxplot(aes(x = Coast, y = rent_median)) #Coast Only Plot
pairwise.t.test(rent_median,Coast) #differences accross East Coasts, West Coast and Middle US. 

#Fit Rent when control for both Coasts and Region
m_test<-lm(rent_median~Coast*Region)
Anova(m_test, type = 3) #There is Interactions effect accross Groups
#box plot of Coasts with Region by Color
p + geom_boxplot(aes(x = Region, y = rent_median, colour = Coast))
#split data up by Region to stratify data to control for interaction

m_north <- lm(rent_median[Region=="North"] ~ Coast[Region == "North"])
m_south <- lm(rent_median[Region=="South"] ~ Coast[Region == "South"])
Anova(m_north, type = 3)#F significant
Anova(m_south, type = 3)#F Significant
#West is the reference group
fitted_m_north <- fitted(m_north)
summary(m_north) #in the north, no different in rent between West and East
# In the north, there is a difference between Middle and West 

fitted_m_south <- fitted(m_south)
summary(m_south) # in the south, rent is difference between middle and east and west



# There is a difference between the average median rent between East and West Coast
# There is not enough evidence to reject null that there is Zero difference between the average median rent between North and South Regions.


# Graphicsal Correlations analysis of variables
contUSdfFactors <- data.frame(Pop_density, water_land_ratio, Household_Income_median, rent_median, Owner_Costs_mortgage_median)


#Check for correlation
cor(contUSdfFactors)
c <- round(cor(contUSdfFactors), 2)
ggcorrplot(c, type = "lower", lab = T)
#there is strong correlation between rent and owner costs plus mortgage.
#there is some correlation between owner's cost and rent
# Strong correlation:  
#   * House hold income and Rent  
# * House hold income and Owner Cost + Mortgage  
# * House hold income and Owner Cost  
# * Owner Cost + Mortgage and Owner Cost  
# 
# Moderate correlation for Owner Cost and Rent
# 
# Weak or no correlation between Population Density, and water:land ratio

# More fun stuff
# Create dummy Variables for Coasts and Region for multiple regression analysis
contUSdf <- contUSdf %>% mutate(
  gNorth = if_else(Region=="North", 1, 0),
  gSouth = if_else(Region=="South", 1, 0),
  gWest = if_else(Region=="West", 1, 0),
  gMid = if_else(Region=="Middle", 1, 0),
  gEast = if_else(Region=="East", 1, 0)
)

# Build a multiple regression model to examine the effect of: longitude, latitude, interactions, population, household income and the ratio of water to land 
# as predictors of Gross Rent: gSouth and gEast are reference variables.
rent_mlrm <- lm(contUSdf$rent_median ~ Household_Income_median+
                  Pop_density+water_land_ratio+gNorth+gWest+gMid)

#R Square = 0.6181, Residual SE = 269.7
rent_mlrm2 <- lm(contUSdf$rent_median ~ Household_Income_median+
                   Pop_density+water_land_ratio+gNorth+gWest+gMid+
                   Owner_Costs_mortgage_median)
summary(rent_mlrm2)
#R Square = 0.646 , Residual SE = 259.6


fitted_rent <- unclass(fitted(rent_mlrm))
resid_rent <- unclass(resid(rent_mlrm))

hist(resid_rent, xlab = "Residual Gross Rent")
plot(fitted_rent, resid_rent)
abline(h = 0)
skewness(resid_rent)

#compare Gross rent Model with and without polynomial fit
anova(rent_mlrm, rent_mlrm2)
#get fitted and residuals value
fitted_rent2 <- unclass(fitted(rent_mlrm_pol))
resid_rent2 <- unclass(resid(rent_mlrm_pol))

par(mfrow = c(1, 2))
hist(resid_rent)
hist(resid_rent2)
#############
#############



# Owner's Costplus mortgage analysis with gSouth and gEast are reference variables
O_cost_mlrm <- lm(contUSdf$Owner_Costs_mortgage_median ~ rent_median+ Household_Income_median+
                    Pop_density+water_land_ratio+gNorth+gWest+gMid)

summary(O_cost_mlrm)
fittedO_cost <- unclass(fitted(O_cost_mlrm))
residO_cost <- unclass(resid(O_cost_mlrm))


hist(residO_cost)
plot(fittedO_cost, residO_cost)
abline(h=0)

O_cost_mlrm2 <- lm(contUSdf$Owner_Costs_median ~ Household_Income_median+
                     Pop_density+water_land_ratio+gNorth+gWest+gMid+
                     Owner_Costs_mortgage_median)


#############
#############


# Gross Rent polynomial fit
O_cost_mlrm_pol <- lm(contUSdf$Owner_Costs_median ~ Household_Income_median+
                        Pop_density+water_land_ratio+gNorth+gWest+gMid+
                        I(Household_Income_median^2)+I(Pop_density^2)+I(water_land_ratio^2))
#compare Model with and without polynomial fit
anova(O_cost_mlrm, O_cost_mlrm_pol)

# Owner's Cost analysis with gSouth and gEast are reference variables
O_costmortgage_mlrm <- lm(contUSdf$Owner_Costs_mortgage_median ~ Household_Income_median+
                            Pop_density+water_land_ratio+gNorth+gWest+gMid)
summary(O_costmortgage_mlrm)
fittedO_cost_mortgage <- unclass(fitted(O_costmortgage_mlrm))
residO_cost_mortgage <- unclass(resid(O_costmortgage_mlrm))

hist(residO_cost_mortgage)
plot(fittedO_cost_mortgage, residO_cost_mortgage)
abline(h=0)

skewness(residO_cost_mortgage)


# Preidicted location where it is Lowest cost for owner 
contUSdf[which.min(fitted(O_cost_mlrm_pol)),]
# Preidicted location where it is highest cost for owner 
contUSdf[which.max(fitted(O_cost_mlrm_pol)),]

##############################
##############################
# Comparing the states with the highest proportion of rent above 75 percentile price. 
#get the 90 percentile of all the median rent in the US
US90PercentileRent <- quantile(rent_median, 0.90)

# Create variable Top10% of area with median above the 90 percentile of the national median
# 1 is above, 0 is below
contUSdf$top10percent <- if_else(rent_median>US90PercentileRent, 1, 0)

#get the proportion of homes by coasts with proportion of median rent in the top 10% of the nation
contUSdf %>% group_by(Coast) %>% count(top10percent==1)
contUSdf %>% group_by(Coast) %>% summarise(n = n())
#Test the hypotheis that the East Coast has just as % rental market with median prices higher than 
# 90% of the median as the west coasts

prop.test(c(1789,1465), c(18239,7491), conf.level = 0.95)
#The % of rental in the East Coast to be outrageous pricing above the 90% of national median is actually less than the west by
# 8.7% to 10%. 


