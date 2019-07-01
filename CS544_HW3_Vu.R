library(UsingR)
# 
# Part 1) 10 points
# Use the primes (UsingR) dataset. Use the diff function to compute the differences
# between successive primes. Show the frequencies of these differences. Show the
# barplot of these frequencies.
prime_dif <- diff(primes)
prime.dif.freq <- table(prime_dif)
barplot(prime.dif.freq, col = "red", xlab = "differences in Prime numbers", ylab = "Frequency")



## Part 2) 10 points
# Use the coins (UsingR) dataset. Do not use explicit loops for any calculations. Do not
# hard code the denominations in the solution. The solution should work for any
# denominations.
# a) How many coins are there of each denomination?
# b) What is the total value of the coins for each denomination?
# c) What is the total value of all the coins?
# d) Show the barplot for the number of coins by year.


library(UsingR)

names(coins)
head(coins, 3)
#a
denom <- table(coins$value)
#b
denomdf <- as.data.frame(denom)
C <- as.numeric(as.vector(denomdf[,1]))
N <- as.vector(denomdf[,2])
coinsValdf <- data.frame(
  Coin = C,
  TotalVal = C*N
)
#c
total_coins_value <- sum(coinsValdf$TotalVal)
paste(total_coins_value, "USD")

#d
barplot(table(coins$year), xlab = "YEAR", ylab = "Number of Coins")


##Part 3) 10 points
# Use the south (UsingR) dataset.
# a) Show the stem plot of the data. What do you interpret from this plot?
# b) Show the five number summary of the data. Calculate the lower and upper ends of
# the outlier ranges. What are the outliers in the data?
# c) Show the horizontal boxplot of the data along with the appropriate labels on the plot.


library(UsingR)

sVect <- south
#a
stem(sVect)
#b
fivenum(sVect)
I <-IQR(sVect)
Q <-quantile(sVect)
Q

LSL <- Q[[2]] - 1.5* I
USL <- Q[[4]] + 1.5* I

paste("outlier below lower limit: ")
sVect[sVect < LSL]
paste("outlier beyong upper limit: ")
sVect[sVect > USL]

#c
boxplot(sVect, xaxt = "n", xlab ="number" ,horizontal = TRUE )
axis(side = 1, at = Q, labels = TRUE, las=2)





##Part 4) 10 points
# Use the pi2000 (UsingR) dataset.
# a) How many times each of the digits 0 to 9 occur in this dataset?
# b) Show the percentages of their frequencies.
# c) Show a barplot of the above frequencies.
# d) Show the histogram of the pi2000 data. Why is the first bar different from the rest?
# Using the breaks option, make this histogram similar to the barplot in c).s

library(UsingR)
#a
table(pi2000)
#b
piProp <- prop.table(table(pi2000))
#c
P1<-barplot(piProp)

#d
H1<- hist(pi2000)     
H1 #This bar is different because it combines the frequency resulted from 0 and combined with the freq resulted from 1.

H2<- hist(pi2000, breaks = seq(-1, 9, 1), xlim = c(-1, 9)) #similar plot to part c
H2



##Part 5) 15 points
# Suppose that a football (NFL), basketball (NBA), and hockey (NHL) games are being
# shown at the same time. Consider the two-way summarized data shown below showing
# the preferences of men and women what sport they wish to watch.
# a) Using cbind, create the matrix for the above data.
# b) Set the row names for the data.
# c) Set the column names for the data.
# d) Now, add the dimension variables Gender and Sport to the data.
# e) Show the marginal distributions for the Gender and the Sport.
# f) Show the result of adding margins to the data.
# g) Show the proportional data separately for Gender and Sport. Interpret the results.
# h) Using appropriate colors, show the mosaic plot for the data. Also show the barplot for
# Gender and Sport separately with the bars side by side. Add legend to the plots.



library(UsingR)
#a
sportmx <- cbind(c(25, 20), c(10, 40), c(15, 30))
#b
rownames(sportmx) <- c("Men", "Women")
#c
colnames(sportmx) <- c("NFL", "NBA", "NHL")
#d
genC <- rownames(sportmx)
sporC <- colnames(sportmx)
dimnames(sportmx) <- list(Gender = genC, Sport = sporC)
#e
apply(sportmx, 1, sum)
apply(sportmx, 2, sum)
#f
addmargins(sportmx)
#g
prop.table(sportmx, 1) #Proportion by Gender
# 50% of men watches the NFL, 20% watches the NBA, 30% watches the NHL
# 22% of women watches the NFL, 44% watches the NBA, 33% watches the NHL

prop.table(sportmx, 2) #Proportion by Sport
#of the total NFL watcher, 56% are men, 44% are women
#of the total NBA watcher, 22% are men, 80% are women
#of the total NHL watcher, 33% are men, 67% are women

#h
mosaicplot(sportmx, color = c("cyan", "coral", "gray55"))

barplot(sportmx, xlab = "sports", ylab = "N", beside = TRUE, col = c("cyan", "coral"), legend.text = TRUE)



##Part 6) 10 points
# Use the midsize (UsingR) dataset.
# a) Show the pair wise plots for all the variables.
# b) Provide at least 4 interpretations of the results


library(UsingR)

attach(midsize)
pairs(midsize)


# observations:
#   Intepretations:
#   1- There is a positive correlation relationship between the values of cars and years
# 2- Looking at plots on the 1st row and the 1st column, it looks like the values of the cars increase linearly with years up 
# to aproximately 1996-1998, at which point the value of cars increases at a faster rate with years
# 3- For new cars, all three cars are roughly the same price ~20K
# 4- comparing the Taurus versis the Accord and the Camry by year, the Taurus is at a lower value than the Camry and the Accord



##Part 7) 15 points
# Use the MLBattend (UsingR) dataset.
# a) Extract the wins for the teams BAL, BOS, DET, LA, PHI into the respective vectors.
# b) Create a data frame of five columns using these vectors. Use the team names for the
# columns
# c) Show the boxplot of the data frame.
# d) Provide at least 5 interpretations of the results.


library(UsingR)
data("MLBattend")
#a

BAL <- c(MLBattend$wins[MLBattend$franchise == "BAL"])
BOS <- MLBattend$wins[MLBattend$franchise == "BOS"]
DET <- MLBattend$wins[MLBattend$franchise == "DET"]
LA <- MLBattend$wins[MLBattend$franchise == "LA"]
PHI <- MLBattend$wins[MLBattend$franchise == "PHI"]

#b
winsdf <- data.frame(
  BAL <- BAL,
  BOS <- BOS,
  DET <- DET,
  LA <- LA,
  PHI <- PHI
)
names(winsdf) <- c("BAL", "BOS", 'DET', 'LA', 'PHI')

#c
F5 <- fivenum(c(BAL, BOS, DET, LA, PHI))
WinsLSL <- F5[2] - (1.5* IQR(c(BAL, BOS, DET, LA, PHI)))
WinsMed <- median(c(BAL, BOS, DET, LA, PHI))
WinsUSL <- F5[4] + (1.5* IQR(c(BAL, BOS, DET, LA, PHI)))
boxplot(winsdf,ylim = c(40, 120))
axis(side = 4, at = c(WinsUSL, WinsMed, WinsLSL), labels = TRUE)

# d) Intepretation:
#   1- Baltimore and Phily has the widest distribution of wins of the 5 teams.
# 1.1- Baltimore range of the number of wins extends from the min to the max of the total range.
# 2- Boston has the narrowest distribution of wins of the 5 teams.
# 2.1- Boston has 2 seasons where they have the number of wins that may be considered to be outlier of BOS's norm.
# 3- Philly has the lowest median in wins between the 5 teams. 
# 4- The Median wins of the population is 84 wins. LA, BAL and BOS all have median above this population median
# 5- The outlier of the population, calculated by +/- 1.5 IQR ranges below 50 and above 114 wins. None of the 5 teams have 
# any season that would qualify to be an outlier of the population based on this criteria.

##Part 8) 20 points
# Initialize the House and Senate data as shown below:
# house <- read.csv('http://kalathur.com/house.csv', stringsAsFactors = FALSE)
# senate <- read.csv('http://kalathur.com/senate.csv', stringsAsFactors = FALSE)
# Provide the simplest R code for the following:
# a) Show how many senators and house members are there by party lines?
# b) Show the top 10 states in decreasing order by the number of house members in that
# state?
# c) Use a box plot on the number of house members per state and determine which
# states are outliers?
# d) What is the average number of years served by party line in the house and senate
# respectively?


house <- read.csv('http://kalathur.com/house.csv', stringsAsFactors = FALSE)
senate <- read.csv('http://kalathur.com/senate.csv', stringsAsFactors = FALSE)

#a
H <- table(house$Party)
H #House members by party
S <- table(senate$Party)
S #Senate members by party

#b
ST <- as.data.frame(table(house$State))

head(ST[order(-ST$Freq),], 10)

#c
boxplot(ST$Freq, xlab = "Number of Representative", horizontal = TRUE)
LHL <- fivenum(ST$Freq)[2] - 1.5*IQR(ST$Freq)
UHL <- fivenum(ST$Freq)[4] + 1.5*IQR(ST$Freq)
axis(side = 3, at = c(LHL,median(ST$Freq),UHL), labels = TRUE)
text(c(LHL,median(ST$Freq),UHL), rep(1.25, 2), srt=90, adj=0, labels = c("LowerLim", "Median", "UpperLim"))
#outlier states are:
ST[ST$Freq > UHL,]#Cali, Florida, NY and Texas

#d
paste("average number of years served by Democrats in the house ", mean(subset(house, house$Party %in% c("Democratic"))$Years_in_office), " years") #
paste("average number of years served by Democrats in the senate ", mean(subset(senate, senate$Party %in% c("Democratic"))$Years_in_office), " years") #
paste("average number of years served by Republicans in the house ", mean(subset(house, house$Party %in% c("Republican"))$Years_in_office), " years") #
paste("average number of years served by Republicans in the senate ", mean(subset(senate, senate$Party %in% c("Republican"))$Years_in_office), " years") #
paste("average number of years served by Independents in the senate ", mean(subset(senate, senate$Party %in% c("Independent"))$Years_in_office), " years") #

