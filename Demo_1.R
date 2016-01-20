####Simple Arithmatic####
# +,-,*,/
     1+2
     2-3
     4*6
     7/8
     8**9

     
library(metafor)
     
lm(y ~ a + b, data=dat3)
     
####Simple Arithmatic with Objects####

#Add(+),Subtr(-),Mult(*),Div(/),Expon(**)
#where,
     #Vectors
     a <- c(1,2,3,4)
     b <- c(5,6,7,8)
     
     #Matrix
     c <- rbind(a,b)  #4x2 matrix
     d <- rbind(b,a)  #4x2 matrix
     e <- cbind(a,b)  #2x4 matrix

#Vectors
     a+b
     a-b
     a*b
     a/b
     a**b
     
#Matricies
     c+d
     c-d
     c*d            #This multiplies individual elements of the matrix. THIS IS NOT matrix multiplication.
     c/d
     c**d
     
     e %*% d        #This is matrix multiplication. (the dot product)

####Object Types####
#Vector : c(value1,value2,...)
#This is an object that holds a list of numbers
     vector <- c(1.1 , 2.2)   # c() is a function that generates vectors of any length.
                              # vectors may hold multiple types of objects
     vector
     str(vector)

     #Useful Actions
     #Create list
     vector.list <- c(1:10)
     vector.list
     
     vector.repeat <- rep(1 , 5)    # rep(<value>,<number of times to repeat>)
     vector.repeat
     
     vector.seq <- seq(from=0, to=100, by=10)   # seq(<starting number>,<ending number>,<increased by>)
                                                # in our example we start from zero, and count to 100 by 10s.
     vector.seq

     # Vectors will only every be (1 x column), for example
     c( c(1,2), c(3,4) ) == c(1,2,3,4)    # the vector does not contain 2 vectors, just the contents of the two vectors.

# List : list(obj1,obj2,...)
# Often used to hold multiple objects
container <- list( c(1,2) , c(3,4) )
container                              #Notice here we have two seperate vectors.

# Accessing Parts of a List
container[1]         # Returns another list with only the first element
container[[1]]       # Returns the object alone
                     # Remember we can check with str...
str(container[1])
str(container[[1]])

####Subsetting####
#Suppose I have a data.frame and want to select certain columns to put into a new subdataset
     #Create Variables (columns)
     Name <- c('Charles','Washington','Alex')
     Hair <- c('brown','grey','black')
     Lbs <- c(150, 167, 200)
     
     #Make a dataframe called "example" using the custom variables
     example <- data.frame(Name, Hair, Lbs)

     #Examine form of created object
     example
     
     #Make an object holding column names that we want
     #then,    ask R to return the data set but only with the variables we want
                wanted.columns <- c("Name","Lbs") #Names in quotations are case sensitive! "name" will not execute!
     example[ , wanted.columns]
              
     #above is equivalent to the function below. Note: "" or '' may be used to surround desireable column names.
     example[ , c("Name","Lbs")]

     #We of course could then create a subset and assign it to a new variable via
     example.sub <- example[ , wanted.columns]
     
     example.sub
     
     #EXERCISE: Create a subset that has "name" and "hair" variables without copy pasting.
     
     
     
     
####Creating Functions####
times2 <- function(user.input){
   return(user.input*2)
}
times2(4)
# notice user.input no longer exists.
user.input

#neither would any value we put into our function that is not in the return function
times2 <- function(user.input){
   not.used <- "useless"
   return(user.input*2)
}

not.used

# print() will show up in the commandline
print_times2 <- function(user.input){
   print(user.input)
}

multiply.values <- function(value1 , value2)
{
     return(value1*value2)
}

# Breakdown
# multiply.values <- function(value1,value2){
# multiply.values                                  name used to call our function
#                 <-                               assign to
#                    function(             )       indicate object multiply.values will be a function
#                             value1,value2        number of objects (2 here) that function will take & how they will be referenced within function
# {                                                START function block
#     return(             )                             give result of function back
#            value1*value2                                    Operations within function
# }                                                END function block
#
     
     
####To Do####
####Vectors vs Lists vs Matricies vs Data.Frames
####as.<obj type>() functions
####ProTips: () around assignment functions to show what is being assigned to the variable
####Logic