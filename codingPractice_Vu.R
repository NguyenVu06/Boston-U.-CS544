
#1.
#Create a vector of 10 numbers. Assign your own values to the vector, 
#some of them should be positive and some of them should be negative.
set.seed(1220)
x1 <- runif(10, min = -30, max = 30)

#a) Using a loop, print how many positive values are there in the array.
#b) Using a loop, print how many negative values are there in the array.
#c) Combine the logic of a) and b) by using a single loop and print how many positive values and
#negative values are present.

p=0
n=0
for(i in x1){
	if(i>0){
		p = p+1
		}else{
		n = n+1
		}
}
print(p) # number of positive values 
print(n) # number of negative values 


#d) Without using any loops, print how many positive values and negative values are present.

length(x1[x1>0]) # number of positive values 
length(x1[x1<0]) # number of negative values 

#===============================================================

#2.
#Let x be a vector of 10 numbers. Let y be a vector of 10 numbers. 
#Assign your own values to
#both the vectors.

set.seed(1220)
x <- runif(10, min = 20, max = 100)
y <- runif(10, min = 20, max = 100)


#a) Using a loop, determine if all the values of x are 
#greater than their corresponding
#counterparts in y.
comp = vector()
for(i in 1:length(x)){
	test <- x[i] > y[i]
	comp[i] = test
}
if("FALSE" %in% comp){
	print(FALSE)
}else{print(TRUE)}

#b) Using a loop, determine if any of the values of x are 
#greater than their corresponding
#counterparts in y.

comp = vector()
for(i in 1:length(x)){
	test <- x[i] > y[i]
	comp[i] = test
	ifelse(comp[i] == TRUE, print(paste("True at i = ", i)), 
			print(paste("False at i = ", i)))
}

#c) Using a loop, multiply the corresponding values in x and y and 
#add the resulting numbers.
#Print out the result after the loop is done.
sumXY <- vector()
for(i in 1:length(x)){
	multi <- x[i] *y[i]
	sumXY[i] = multi
}

sum(sumXY)

#d) Without using any loops, perform the above operations.
all(x>y)
x>y
any <- x>y
which(any %in% c("TRUE"))



#=================================================================

#3.
#Given a vector, say,
#x <- c(2,3,4,1,5,8,2,3,7,5,7, 1)

x_3 <- c(2,3,4,1,5,8,2,3,7,5,7, 1)

#a) what is the maximum value?
x_3[which(x_3 == max(x_3))]

#b) where does the maximum value occur?
which(x_3 == max(x_3))

#c) what is the minimum value?
x_3[which(x_3 == min(x_3))]

#d) where are the minimum values?
which(x_3 == min(x_3))

#Practice the solutions with and without loops.
#with loops:

maxV = maxIx= minV = minIX = mean(x_3)

for(i in 1:length(x_3)){
	if(x_3[i] > maxV){
		maxV = x_3[i]
		maxIx = i
	}
	if(x_3[i] < minV){
		minV = x_3[i]
		minIx = i
	}
}

cat("\n","max val =", maxV," at ", maxIx, "\n",
	"min val =", minV," at ", minIX,"\n")

#===============================================================

#4.
#Given a vector of values, find the value(s) in the vector that are closest to a specified target value. For
#example, if
#data <- c(1,3,5,10,7,8,9,2,3,9,4,15,11,20)
#target <- 6
#then, the values we are looking for in the data should be 5 and 7.


data4 <- c(1,3,5,10,7,8,9,2,3,9,4,15,11,20)

target <- 6

diff <- vector()
for(i in 1:length(data4)){
	diff[i] = abs(data4[i]-6)
	
}

data4[which(diff ==min(diff))]

#===============================================================

#
#5.
#Using a for loop, write your own function, myWhich(x, n), that returns the indices of all occurrences of
#the given value, n, in the given vector, x.


myWhich <- function(x, n){
	vec <- vector()
	for(i in x){
		if(i == n){
		vec <- c(vec, c(match(n, x)))
		}
	}
	return(vec)
}
myWhich(data4, 10)

which(data4 == 10)







