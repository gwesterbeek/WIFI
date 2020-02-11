# Construct a 5x6 df
X <- matrix(rnorm(30), nrow=5, ncol=6)
X <- as.data.frame(X)
view(X)

# Sum the values of each column with `apply()`
colsum <- apply(X, 2, sum)
colsum
class(colsum)

# Sum the values of each row with `apply()`
rowsum <- apply(X,1,sum)
dqdq


# Define 3 dataframes and list them:
A <- data.frame(v1 = c(1,2,3), v2 = c(4,5,6), v3 = c(7,8,9))
B <- data.frame(v1 = c(4,5,6,7), v2 = c(8,9,10,11), v3 = c(12,13,14,15))
C <- data.frame(v1 = c(8,9,10), v2 = c(8,9,10))
myList <- list(A, B, C)

#Extract the 2nd column from myList, using lapply and the dplyr function "jgjjgjj"
lapply(myList, subset, select = v2)

#Extract the 1st row from myList, using lapply     the dplyr function "slice"?????
lapply(myList, subset, slice(A, 1:1))

#Extract one single element from every dataframe in the list. You can use "[" as a function
lapply(myList,"[",1,2)


#### using rep() ####
# rep() is a function from base R that takes an input "x" and replicates it n times.

# Initialize `Z`
Z <- sapply(myList,"[", 1,1 )
Z

# Replicate the values of `Z` 3 times, 1 time and 2 times
Z <- rep(Z,c(3,1,2))
Z


#### mapply() ####
# Multivariate apply: it vectorizes arguments to functions that not usually accept vectors as arguments

# Create a 4x4 matrix called Q1 using the functions matrix() and rep() that looks like this:

# 1 2 3 4
# 1 2 3 4
# 1 2 3 4
# 1 2 3 4

Q1 <- matrix(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4)), nrow=4,ncol=4) 
  
Q1

# Now create the same matrix usinguse `mapply()`
Q2 <-mapply(rep,1:4,4)
  print(Q2)

# The results are the same, we have vectorized the action of the function rep()

# 
