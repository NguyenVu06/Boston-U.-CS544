#Part1) 60 points
#The following sample data shows the scores of the students in an exam:
# 58, 52, 93, 55, 54, 99, 68, 69, 98, 70
#Using R, assign the above data as a vector in the same order to a variable, say, scores. Do the
#following using R:

score <- c( 58, 52, 93, 55, 54, 99, 68, 69, 98, 70)

#a) How many students took the exam? Using indexing, show the expression for accessing the
#middle two items. The code should work for a vector of any size. You can assume there are an
#even number of values.
#Sample output:
# [1] 10
# [1] 54 99


NStudent <- length(score)
mid2 <- score[length(score)/2-1: length(score)/2+1]
NStudent
mid2



#b) Use median(scores) to show the median of the data. Using comparison operators, write the
#expression for scores less than the median of the data.
#Sample output:
#[1] 68.5
#[1] TRUE TRUE FALSE TRUE TRUE FALSE TRUE FALSE FALSE FALSE

medScore <- median(score)
comp <- c(score < medScore)
medScore
comp

#c) Using logical indexing and the expression from b), show all the scores that are less than the
#median value of the data. Similarly, show all the scores that are greater than or equal to the
#median.
#Sample output:
# [1] 58 52 55 54 68
# [1] 93 99 69 98 70

sc_less <- score[score < medScore]
sc_great <- score[score > medScore]
sc_less
sc_great

#d) Using logical indexing with TRUE and FALSE values, show the odd indexed values from the
#scores. The code should work for any size input data. You can assume that there are even
#number of values in scores.
#Sample output:
# [1] 58 93 54 68 98

odd_sc_idx <- score[(1:length(score))%%2 == 1]
odd_sc_idx

#e) Using the paste function with LETTERS, show the code for the following output.
# The code should not hardcode the value 10 for the number of scores.
# Sample output:
# [1] "A=58" "B=52" "C=93" "D=55" "E=54" "F=99" "G=68" "H=69" "I=98" "J=70"

paste(LETTERS[1:length(score)], score, sep = "=")


#f) Create a matrix of size 5 x 2 using the scores data. The first five values belong to the first
#column of the matrix. Assign the result to the variable, scores.matrix, and display the result.
#Sample output:
 #[,1] [,2]
#[1,] 58 99
#[2,] 52 68
#[3,] 93 69
#[4,] 55 98
#[5,] 54 70

sc_mtx <- matrix(score, ncol = 2, byrow = FALSE)


#g) Show the code for displaying the first and last rows of the matrix. The code should work for
#any size matrix.
#Sample output:
# [,1] [,2]
# [1,] 58 99
# [2,] 54 70

sc_mtx[c(1, nrow(sc_mtx)) , ]

#h) Assign row names for the scores.matrix as Student1, Student2,… and column names as
#Quiz1 and Quiz2. The code should work for any size matrix, i.e., for any number of rows in the
#matrix with two columns.
#Sample output:
# Quiz1 Quiz2
#Student1 58 99
#Student2 52 68
#Student3 93 69
#Student4 55 98
#Student5 54 70

studentsRow <- paste(rep("Student", times = nrow(sc_mtx)), c(1:nrow(sc_mtx)), sep = " ")
rownames(sc_mtx) <- studentsRow
colnames(sc_mtx) <- c("Quiz1", "Quiz2")
sc_mtx


#Part 2) 40 points
#Create a data frame, say weather.info, using the column names: Month, Monthly_Average,
#Daily_Max_Average, Daily_Min_Average, Record_High, and Record_Low. A snapshot of the
#resulting data frame with the required data is shown below:
#a) Show the code for creating the above data frame and display the resulting data frame.

#Randomly assign values to columns
MonthNum <- c(1:12)
Monthly_Average <- runif(12,32, 100)
Daily_Max_Average <- rnorm(12, 85, 5)
Daily_Min_Average <- rnorm(12, 32, 5)
Record_High <- rnorm(12, 100, 10)
Record_Low <- rnorm (12, 25, 20)
#Add columns to create Data Frame
weather.info <- data.frame(
	Months = MonthNum,
	MonthlyAvg = Monthly_Average,
	DayMaxAvg = Daily_Max_Average,
	DayMinAvg = Daily_Min_Average,
	RecordHigh = Record_High,
	RecordLow = Record_Low)

weather.info


#b) Show the summary for Monthly_Avg, DailyMax_Avg, DailyMin_Avg, Record_High, and
#Record_Low.

summary(weather.info)


#c) Show the data frame sliced using the columns Month, Record_High, and Record_Low.

weather.info[c("Months", "RecordHigh", "RecordLow")]

#d) Show the data frame sliced using the first and last row. Do not hard code 12 in the
#expression, i.e., the code should work for a data frame of any size

weather.info[c(1, nrow(weather.info)),]

#e) Show all rows of the data frame whose DailyMax_Avg is greater than 40.

weather.info[weather.info$DayMaxAvg >40,]

#f) Modify the data by adding a new column, Record_Deviation, showing the difference between
#the Record_High and Record_Low. Display the new resulting data frame.

weather.info$Record_deviation <- weather.info$RecordHigh - weather.info$RecordLow
weather.info