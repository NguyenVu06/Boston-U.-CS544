#PART 1
# Suppose that in a particular state, among the registered voters, 40% are democrats, 50 % are
# republicans, and the rest are independents. Suppose that a ballot question is whether to impose
# sales tax on internet purchases or not. Suppose that 70% of democrats, 40% of republicans,
# and 20% of independents favor the sales tax. If a person is chosen at random that favors the
# sales tax, what is the probability that the person is i) a democrat? ii) a republican, iii) an
# independent. Show the solutions with the calculations without using R. Then, verify with the
# bayes function provided in the code samples.

#A1 - Democrats, P(A1) = 0.4
#A2 - Republicans, P(A2) = 0.5
#A3 - Independent, P(A3) = 0.1

#B - sale tax: favor B vs. not favor -B:
# P(B|A1) = 0.7
# P(B|A2) = 0.4
# P(B|A3) = 0.2

#Person chosen at random that favors the sale tax, whats the prob that it is a democrat? = pDem_Fav
#Manual calucation: 
# P(A1|B) = P(A1)*P(B|A1) / (P(A2)*P(B|A2)+P(A3)*P(B|A3))
pDem_Fav <- (0.4*0.7)/(0.4*0.7+0.5*0.4+0.1*0.2)
pGOP_Fav <- (0.5*0.4)/(0.4*0.7+0.5*0.4+0.1*0.2)
pInd_Fav <- (0.1*0.2)/(0.4*0.7+0.5*0.4+0.1*0.2)


#Verification by R
bayes <- function(prior, likelyhood){
  numerators <- prior*likelyhood
  return(numerators/sum(numerators))
}

priorO <- c(.4, .5, .1)
posteO <- c(0.7, 0.4, 0.2)

prbCalc <- bayes(priorO, likelyhood = posteO)

cat("Prob of being a Dems given Favor Tax manual calc is ", pDem_Fav, " which is ", pDem_Fav == prbCalc[1] ," equal to R calcualted value")
cat("Prob of being a Republican given Favor Tax manual calc is ", pGOP_Fav, " which is ", pGOP_Fav == prbCalc[2] ," equal to R calcualted value")
cat("Prob of being a Indipendent given Favor Tax manual calc is ", pInd_Fav, " which is ", pInd_Fav == prbCalc[3] ," equal to R calcualted value")

##_______________________________________________________##

library(prob)
library(dplyr)

# PART 2:
# a) Consider the experiment of rolling a pair of dice. Using R, show how would you define a
# random variable for the absolute value of the difference of the two rolls.
#
D <- probspace(rolldie(2))
D <- addrv(D, T = abs(X1 - X2))

# b) Using the above result, what is the probability that the two rolls differ by exactly 2? What is
# the probability that the two rolls differ by at most 2? What is the probability that the two rolls
# differ by at least 3? Use the Prob function as shown in the code samples.

ans1 <- Prob(D, T==2) 
ans2 <- Prob(D, T<=2)
ans3 <- Prob(D, T>=3)
ans1 #Prob differ by exactly 2
ans2 #Prob differ by atmost 2
ans3 #Prob differ by atleast 3


# c) Show the marginal distribution of the above random variable (using R).
marginal(D, vars = "T") #marginal distribution of the above random variable

# 
# d) Using R, add another random variable to the above probability space using a user defined
# function. The random variable is TRUE if the sum of the two rolls is even, and FALSE
# otherwise. What is the probability that the sum of the two rolls is even? Show also the marginal
# distribution for this random variable.

evenSumCheck = function(x){
  return(ifelse(sum(x[1], x[2])%%2 != 0, FALSE, TRUE ))
}

D <- addrv(D, FUN = evenSumCheck, name = "U")
Prob(D, U == TRUE) #probability that the sum of the two rolls is even? 

marginal(D, vars = "U") #marginal distribution for this random variable

###_______#

#.
# PART 3

# Using a for loop, write your own R function, evensum(data), that returns the sum of all the even
# values in the given numeric data vector.

evensum <- function(vector){
  dftsum = 0
  for (i in vector){
    if(i%%2 == 0 ){
      dftsum = dftsum + i
    }
  }
  return(dftsum)
}

# Now, without using any loop, write your own R function, evensum2(data), that returns the sum
# of all the even values in the given numeric data vector.
# Test both functions with sample data.

evensum2 <- function(vector){
  fdf<- data.frame(
    vec = c(vector),
    odd = c(vector%%2)
  )
  return(sum(subset(fdf, odd == 0)$vec))
}
#test codes:
evensum(c(15, 20, 25, 30, 35)) == evensum2(c(15, 20, 25, 30, 35))
evensum(1:10)==evensum2(1:10)
evensum(seq(1, 15, by=2))==evensum2(seq(1, 15, by=2))


###_______#

#.
# PART 4
#importdata
dow <- read.csv('http://kalathur.com/dow.csv', stringsAsFactors = FALSE)

# a) Use the diff function to calculate the differences between consecutive values.
# Insert the value 0 at the beginning of these differences. Add this result as the DIFFS column of
# the data frame.

dow$DIFFS <- c(0, diff(dow$VALUE))

# b) How many days did the Dow close higher than its previous day value? How many days did
# the Dow close lower than its previous day value?

hi_day <- function(vector, higher = TRUE){
    hi = 0
    lo = 0
    sa = 0
    x = c()
    for (i in vector){
      if(i > 0){
        hi = hi + 1
      } else if(i<0){
        lo = lo +1
      } else {
        sa = sa +1
      }
    }
    x <- c(hi, lo, sa)
    if(higher == TRUE){
      statement = paste("days higher than previous day = ", x[1])
    } else {
      statement = paste("days lower than previous day = ", x[2])
    }
    return(statement)
}
hi_day(dow$DIFFS, higher = TRUE) #number of days Dow closed higher than the previous day
hi_day(dow$DIFFS, higher = FALSE) #number of days Dow closed lower than the previous day

# 
# c) Show the subset of the data where there was a gain of at least 400 points from its previous
# day value.

S <- subset(dow, DIFFS >= 400)
S

# 
# d) Provide the solution to compute the longest gaining streak of at least 100 points in the data.
# Show the data for that longest gaining streak. Hint: Use the rle function provided by R

dow$STREAK <- c(ifelse(dow$DIFFS >=100, "Y", "N"))
U <- rle(dow$STREAK)

max_gain_length <- max(U$length[U$values == "Y"])

# Value of max gain run
U$values[U$lengths == max_gain_length]

# index of max run in rle output

max_index <- which(U$lengths == max_gain_length & U$values == "Y")



# Number of values before this
prev_values_length <- ifelse(max_index == 1, 0, sum(U$lengths[1:(max_index-1)]))

# Show the data for that longest gaining streak
dow[c((prev_values_length+1):(prev_values_length+max_gain_length)),]


