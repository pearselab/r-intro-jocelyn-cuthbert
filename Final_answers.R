################################################
##   Session #1    #############################
################################################

#cat - concatenate and print:  Outputs the objects, but with less conversions than print, which makes it useful for producing output in user-defined functions. Converts argument to character vectors, concatenates them to a single character vector. 
cat(... , file = "", sep = " ", fill = FALSE, labels = NULL,
    append = FALSE)

#cbind2-combine two matrix like R objects by columns.  Generic functions with default methods.  
cbind2(x, y, ...)

#col - column indexes, returns a matrix of integers indicating their column number either in a matrix-like object or a factor of column labels. 
col(x, as.factor = FALSE)

#row - row indexes, returns a matrix of integers indicating their row number in a matrix-like object or a factor of column labels. 
row(x, as.factor = FALSE)

#cut - convert numeric to factor, which divides the range of x into intervals, codes the values in x according to which interval they fall. The leftmost interval corrosponds to level one, etc.
cut (x, ...)

#diff - Lagged Differences, which returns suitably lagged and iterated differences.  
diff(x, ...)

#dim -Dimensions of an Object, which retrieves or sets the dimension of an object
dim(x)
dim(x) <- value

#rownames - Row Names, retrieve or set the row names of a matrix-like object
rownames(x, do.NULL = TRUE, prefix = "row")
rownames(x) <- value
#colnames - Column Names, retrieve or set the column names of a matrix-like object
colnames(x, do.NULL = TRUE, prefix = "col")
colnames(x) <- value
#names - The names of an object, functions to get or set the names of an object
names(x)
names(x) <- value

#expand.grid - Create a data frame from all combinations of factor variables supplied.  
expand.grid(..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)

#eigen, %*%, lower.tri, upper.tri, diag - Spectral Decomposition of a Matrix, which computes eigenvalues and eigenvectors of numeric (double, integer, logical) or complex matrices. 
eigen(x, symmetric, only.values = FALSE, EISPACK = FALSE)

#gl - Generate Factor Levels, Generate factors by specifying the pattern of their levels.
Generate factors by specifying the pattern of their levels.

#identical - Test Objects for Exact Equlity, The safe and reliable way to test whether two objects are exactly equal, in which case it will say TRUE, FALSE if not. 
identical(x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
          ignore.bytecode = TRUE, ignore.environment = FALSE)

#image - Display a Color Image, which creates a grid of colored or gray scale rectangles with colors corresponding to z values.  Generic function, used to display three-domensional or spatial data.
image(x, ...)

#library - Loading/Attaching and Listing of Packages, which is for libraries that require load and attach add on packages. 
library(package, help, pos = 2, lib.loc = NULL,
        character.only = FALSE, logical.return = FALSE,
        warn.conflicts = TRUE, quietly = FALSE,
        verbose = getOption("verbose"))

require(package, lib.loc = NULL, quietly = FALSE,
        warn.conflicts = TRUE,
        character.only = FALSE)

#length - Length of an Object, which is to get or set the length of vectors and factors, can be used on any R object for which a method has been defined. 
length(x)
length(x) <- value

#jitter - Add noise to numbers, small amounts of noise to numeric vectors. 
jitter(x, factor = 1, amount = NULL)

#ls - List Objects, returns a vector of character strings which gives the names of the obejcts in the specified environment. It is useful in conjuction with browser, because when there is no argument at the top level prompt 1s shows what data sets and functions a user has defined, and when there is no argument inside a function, 1s returns name of the function local variables. 
#it can specify the environment from which object names are taken in a few forms:
#Integer as the position in the search list. 
#Character string name as an element in the search list
#explicit environment which includes sys.frame in order to access currently active function calls
ls(name, pos = -1L, envir = as.environment(pos),
   all.names = FALSE, pattern, sorted = TRUE)
objects(name, pos= -1L, envir = as.environment(pos),
        all.names = FALSE, pattern, sorted = TRUE)

#mean - Arithmetic Mean, compute function for arithmetic mean. The default in trim is zero, arithetic means in computed, cumeric or complex vector of length one.  If x is not logical, numeric, or complex. 
mean(x, ...)
## Default S3 method:
mean(x, trim = 0, na.rm = FALSE, ...)
#median - Median Value, Compute sample median
median(x, na.rm = FALSE)
#max - Maxima, Returns the parallel maxima of input values. 
#Maximum of all the values present in the arguments, as an integer if all are logical, double if all are numeric, and character otherwise. 
max(..., na.rm = FALSE)
pmax(..., na.rm = FALSE)
pmax.int(..., na.rm = FALSE)
#min - Minima - Returns the parallel minima of input values. 
#Minimum of all the values present in the arguments, as an integer if all are logical, double if all are numeric, and character otherwise. 
min(..., na.rm = FALSE)
pmin(..., na.rm = FALSE)
pmin.int(..., na.rm = FALSE)
#pmax and pmin take one or more vectors (or matrices) as arguments and return a single vector giving the ‘parallel’ max or min of the vectors. The first element of the result is the maximum (minimum) of the first elements of all the arguments, the second element of the result is the maximum (minimum) of the second elements of all the arguments and so on. 
#pmax.int and pmin.int are faster internal versions only if used when all arguments are atomic vectors and there are no classes because they drop all attributes. 

#paste - Concatenate Strings, a concetenate vectors after converting to character.  Converts its arguments to character strings, seperates them by the string given by sep. 
paste (..., sep = " ", collapse = NULL)
paste0(..., collapse = NULL)

#read.table & #read.csv- Data Input, reads a file in a table format, creating a data frame form it.  Cases corresponding to lines and variable to to fields in the file. 
read.table(file, header = FALSE, sep = "", quote = "\"'",
           dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
           row.names, col.names, as.is = !stringsAsFactors,
           na.strings = "NA", colClasses = NA, nrows = -1,
           skip = 0, check.names = TRUE, fill = !blank.lines.skip,
           strip.white = FALSE, blank.lines.skip = TRUE,
           comment.char = "#",
           allowEscapes = FALSE, flush = FALSE,
           stringsAsFactors = default.stringsAsFactors(),
           fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)

read.csv(file, header = TRUE, sep = ",", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", ...)

read.csv2(file, header = TRUE, sep = ";", quote = "\"",
          dec = ",", fill = TRUE, comment.char = "", ...)

read.delim(file, header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE, comment.char = "", ...)

read.delim2(file, header = TRUE, sep = "\t", quote = "\"",
            dec = ",", fill = TRUE, comment.char = "", ...)

#write.csv & #write.table - Data Output, prints its required argument to a file or connection after it is converted to a data frame. 
write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

write.csv(...)
write.csv2(...)

#rnorm,pnorm,qnorm - The normal distribution, which includes density, distribution function, quantile function, and random generation, for the normal distribution with mean equal to mean and standard deviation. 
dnorm(x, mean = 0, sd = 1, log = FALSE)
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean = 0, sd = 1)

#runif,rpois - The Uniform Distrution, which are functions that provide information about the uniform distribution on the interval from min to max.  
#If min or max are not specified they assume the default values of 0 and 1 respectively
#dunif gives the density, punif gives the distribution function, gunif gives the quantile function and runif generates random deviates
dunif(x, min = 0, max = 1, log = FALSE)
punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
runif(n, min = 0, max = 1)

#rank - sample ranks, which returns the sample ranks of the values in a vector.  Equal values and missing values can be handled in several ways.
#if there are no NAs, and the components are different, the ranks are well defined with values in seq_alongx.
#If there are some values equal, the argument ties.method determines the results at corrosponding indices. 
#The "first" method results in a permutation with increasing values at each index set of ties, and analogously "first" with decreasing values. The "random" method puts these in random order whereas the default, "average", replaces them by their mean, and "max" and "min" replaces them by their maximum and minimum respectively, the latter being the typical sports ranking.
rank(x, na.last = TRUE,
     ties.method = c("average", "first", "last", "random", "max", "min"))

#sort,rank,order - Sorting or Ordering Vectors, which sorts a vector or factor partially into ascending or descending order. Same as ordering, but only using one argument. 
sort.list(x, partial = NULL, na.last = TRUE, decreasing = FALSE,
          method = c("shell", "quick", "radix"))

#order - Ordering permutation which rearranges its its first argument into ascending or descending order, breaking ties by furthering arguments. 
#If there are ties in the first vector, values in the second are used to break it.  If its still tied, values in later arguments are used to break it. The stable sort is used except in quick method, so any ties left unresolved are left in original ordering. 
order(..., na.last = TRUE, decreasing = FALSE,
      method = c("shell", "radix"))

#outer - Outer Product of Arrays, which is X and Y is the array A with dimension C, where element A = FUN.
#X and Y must be suitable arguments for FUN. Each will be extended by rep to length the products of the lengths of X and Y before FUN un called. 
outer(X, Y, FUN = "*", ...)
X %o% Y

#rep - Replicate Elements of Vectors and Lists, which replicates the values in x.  It is a generic function, except rep.int and rep_len which are faster simplified versions for two common cases. 
rep(x, ...)

rep.int(x, times)

rep_len(x, length.out)

#rowSum - Give Column Sums of a Matrix or Data Frame, based on a Grouping Variable, which sums across rows of a numeric matrix like object for each level of a grouping variable. 
#It is generic, with a method for data frames and a default method for vectors and matrices. 
#default is to reorder the rows to agree with tapply.  TO sum over all the rows of a matrix, use colsums. 
rowsum(x, group, reorder = TRUE, ...)

## S3 method for class 'data.frame'
rowsum(x, group, reorder = TRUE, na.rm = FALSE, ...)

## Default S3 method:
rowsum(x, group, reorder = TRUE, na.rm = FALSE, ...)

#colSum - Form Row and Column Sums and Means for numeric arrays. 
#Equivalent to use of apply with FUN = mean or FUN = sum with approrpiate margins.  Written for speed, blur over subtleties of NaN or Na.  
colSums (x, na.rm = FALSE, dims = 1)
rowSums (x, na.rm = FALSE, dims = 1)
colMeans(x, na.rm = FALSE, dims = 1)
rowMeans(x, na.rm = FALSE, dims = 1)

.colSums(x, m, n, na.rm = FALSE)
.rowSums(x, m, n, na.rm = FALSE)
.colMeans(x, m, n, na.rm = FALSE)
.rowMeans(x, m, n, na.rm = FALSE)

#seq - Sequence generation.  Generates regular sequences.
#standard generic with a default method.
#seqint is primitive which is faster but with restrictions, seqalong and seqlen are also primitives. 
#numeric inputs should be finite, not infinite or NaN or NA. 
#Always name the arguments when programming, because the interprtation of the unnamed arguments of seq and seq.int are not standard. 
seq(...)

## Default S3 method:
seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
    length.out = NULL, along.with = NULL, ...)

seq.int(from, to, by, length.out, along.with, ...)

seq_along(along.with)
seq_len(length.out)

#source - Read R Code from a file or a Connection. Causes R to accept input from unnamed file, URL, or connection.  Read and parsed from file until end, then expressions are evaluated sequentially in chosen environment. 
#Need to include explicit print calls, and any syntax errors result in none of the code being run. 
source(file, local = FALSE, echo = verbose, print.eval = echo,
       verbose = getOption("verbose"),
       prompt.echo = getOption("prompt"),
       max.deparse.length = 150, chdir = FALSE,
       encoding = getOption("encoding"),
       continue.echo = getOption("continue"),
       skip.echo = 0, keep.source = getOption("keep.source"))

#which, Which indices are true, gives the true indices of a logical object, allowing for array indices. 
#Does not coerce to x to logical unlike most base functions, only accepts arguments with typeof logical. 
which(x, arr.ind = FALSE, useNames = TRUE)
arrayInd(ind, .dim, .dimnames = NULL, useNames = FALSE)

#which.min,which.max - Where is the Min or Max, or first true or false.  Determines location of the minimum or maximum of numeric vectors 
which.min(x)
which.max(x)

#setdiff,intersect,union - Set operations, performs a set union, intersection, difference, equality, and memberhsip on two vectors.
#Will discard any duplicated values in the arguments, and apply as.vector in the arguments.
union(x, y)
intersect(x, y)
setdiff(x, y)
setequal(x, y)

is.element(el, set)

#table - Cross Tabulations and Table Creation. Uses the cross classifying factors to build a contigency table of the counts at each combination of factor levels. 
table(...,
      exclude = if (useNA == "no") c(NA, NaN),
      useNA = c("no", "ifany", "always"),
      dnn = list.names(...), deparse.level = 1)

as.table(x, ...)
is.table(x)

## S3 method for class 'table'
as.data.frame(x, row.names = NULL, ...,
              responseName = "Freq", stringsAsFactors = TRUE,
              sep = "", base = list(LETTERS))


#with - Evaluate an expression in a data environment, which evaluates an R expression in an environment contructed by data possibly modifying the original data. 
#generic function that evaluates expr in a local environment contructed form data, with the callers environment as its environment. Useful for simplifying calls to modeling functions. 
with(data, expr, ...)
within(data, expr, ...)

################################################
##   Session #2    #############################
################################################

#1.  
for (i in 20:10) {
  print (i)
}

#2. 
for (i in 20:10) 
  if (i %% 2==0) {
    print (i)
  }

#3. 

is_prime <- function (num) {
  if (num == 2) {
    TRUE
  } else if (any(num%%2:(num-1)==0)) { # see example function of a "any
    FALSE
  } else {
    TRUE
  }
}

is_prime(7)


is_prime2 <-function (num) {
  if (num >1) {
    flag = 1
    for (i in 2:(num-1)) {
      if ((num %% i) == 0) {
        break
      }
    }
  }
  
  if(num== 2)  flag = 1
  if(flag ==1) {
    print(paste(num, "is a prime number"))
  } else {
    print(paste(num,"is not a prime number"))
  }
}
is_prime2(7)

#4. 

is_prime3 <-function (num) {
  if (num >1) {
    flag = 1
    for (i in 2:(num-1)) {
      if ((num %% i) == 0) {
        break
      }
    }
  }
  
  if(num== 2)  flag = 1
  if(flag ==1) {
    print(paste(num,"Job:NUMBER"))
  } 
}

is_prime3(7)

for (num in 1:20) {
  if (num %% 5==0) {
    print ("Good:NUMBER")
  } else {
    is_prime3(num)  
  } 
}

#5. 
a=30
b=1
c=10
t=12
pop_growth <-function (a, b, c, t){
  a*exp((-b)*exp(-c)*t)
}
pop_growth (30, 1, 10, 4)

#6.  The biologist likes your function so much they want you to write another function that plots the progress of the population over a given length of time. Write it for them.
#I am going to make the following assumptions in order to plot the function for the biologist, because I assume they would tell me things
#time is 1 year, looking at 12 months in one month increments
#a= carrying capacity of environment, 30 really cute bunnies/acre, reaches after 3 months
#b= displacement rate off of -axis.  The study started right at the beginning through their 3 month
#mating season, so baby bunnies start popping out after the 1 month gestational period right at the 1 time mark. 
#c= how fast the population grows.  I'm assuming they are looking at rabbits, and they
#reproduce at a rate of 10 bunnies/month
#y=4 bunnies

a=30
b=1
c=10
d=12
tme= c(1:d)

pop_growth=function(a, b, c, d){
  t<-0
  vector<-0
  tme= c(1:d)
  for (i in tme){
    t<-c(t,i)
    pop<- a*exp((-b)*exp((-c)*i))
    vector<-c(vector, pop)
  }
  plot(t, vector, xlab="Months", ylab="Population")
}

pop_growth(280,100,.4,30)


#7.

pop_growth=function(a, b, c, d){
  t<-0
  vector<-0
  tme= c(1:d)
  for (i in tme){
    t<-c(t,i)
    pop<- a*exp((-b)*exp((-c)*i))
    vector<-c(vector, pop)
  }
  plot(t,vector, xlab="Months", ylab="Population", col=ifelse(a<vector, "blue", ifelse(vector>b, "red", "black")))
}

pop_growth(280,100,.4,30)

#8.
pop_plot <-function(t, y){
  plot(t,y, xlab="Months", ylab="Population", col=ifelse(y>a & y>b, "purple"))
}

pop_growth=function(a, b, c, d){
  t<-0
  vector<-0
  tme= c(1:d)
  for (i in tme){
    t<-c(t,i)
    pop<- a*exp((-b)*exp((-c)*i))
    vector<-c(vector, pop)
  }
  plot(t,vector, xlab="Months", ylab="Population", col=ifelse(vector>a | vector>b, "purple","blue"))
}

pop_growth(5,1,.4,30)

#9. 
bx<- function(width, height, txt, symb){
  for (k in 1:width)
    cat(symb)
  cat("\n")
  
  for (j in 1:height){
    cat(symb, txt,  symb, "\n") 
  }
  
  for (i in 1:width)
    cat(symb)
  cat("\n")
}

bx(5,1, " ", "*")

#10.

bx<- function(width, height, txt, symb){
  for (k in 1:width)
    cat(symb)
  cat("\n")
  
  for (j in 1:height){
    cat(symb, txt,  symb, "\n") 
  }
  
  for (i in 1:width)
    cat(symb)
  cat("\n")
}

bx(32,1, "Coding is a foreign language", "*")

#11.

bx(5,1, "  Hey  ", "wdp")


#12.
presence <- function(p){
  return(rbinom(1, 1, p))
}

abundance <- function(p, lam){
  if(rbinom(1, 1, p) == 1){
    return(rpois(1, lam))
  }else{return(0)}
}

abundance(.1,6)

#13.

#Put in a example data set on my computer
mat<-matrix(ncol=1, nrow=nrow(data))
species_site <- function (data)
  for (i in 1:nrow(data)){
    results=abundance(data$p, data$lam)
    mat[i]=c(results)
  }
species_site(data)
mat

#14. 
Professor_walking <-function(x,y,t){
  for(i in 1:t){
    x<-(x+rnorm(1)) 
    y<-(y+rnorm(1))
    a[i]=c(x)
    b[i]=c(y)
  } 
  plot(a, b, type="l", xlab="Latitude", ylab="Longitude")
}
Professor_walking(0,0,100)

#15.

Time_to_death <-function(x,y,t){
  for(i in 1:t){
    x<-(x+rnorm(1)) 
    y<-(y+rnorm(1))
    if (x>5|x< -5)
      break
    if (y>5|y< -5)
      break
  } 
  return(i)
}

Time_to_death(0,0,100)

################################################
##   Session #3    #############################
################################################

#Session 3.R

#1. Implement a cat class, complete with race and print methods. 
Rascal <- list(length=40, weight=50, breed="Maine.Coone")
class(Rascal) <- "cat"

Leo <- list(length=20, weight=25, breed="Stray")
class(Leo) <- "cat"

class(Rascal)
class(Leo)

new.cat <- function(weight, breed){ 
  output <- list(weight=weight, breed=breed) 
  class(output) <- "cat" 
  return(output) 
}

race <- function (first, second){
  if(!inherits(first, "cat") | !inherits(second, "cat"))
    stop("you haven't given me two cats!")
  if(first$weight > second$weight) {
    print("first cat is the fattest!")
    return(first)
  }
  print("second cat is the fattest!")
  return(second)
}

race(Rascal, Leo)

#2. Implement a point class that holds x and y information for a point in space. 

x2y25 <- list(x=2, y=25)
class(x2y25) <- "point"

new.point <- function(x,y){ 
  output <- list(x=x, y=y) 
  class(output) <- "point" 
  return(output) 
}

pt.a <-new.point(2,1)
pt.b <-new.point(4,1)
pt.c <-new.point(2,2)

#3. Write a distance method that calculates the distance between two points in space. 
#a^2+b^2=c^2 = sqrt(((pt2$x-pt1$x)^2)+((pt2$y-pt1$y)^2))

distance1 <- function(pt1,pt2, ...){
  dif <- sqrt (( (pt2$x-pt1$x) ^2) + ( (pt2$y-pt1$y) ^2) )
  return(dif)
}
distance1(pt.a, pt.b)


#4. Implement a line class that takes two point objects and makes a line between them. 
#so not a method, a class. 

new.line <- function(x,y){ 
  output <- list(x,y) #or some distance function?
  class(output) <- "line" 
  return(output) 
}

lin.a <- new.line(pt.a, pt.b)
lin.b <- new.line(pt.b, pt.c)
lin.c <- new.line(pt.c, pt.a)

class(lin.a)
class(lin.b)
class(lin.c)

#5. Implement a polygon class that stores a polygon from point objects.

new.polygon <- function (lin.a,lin.b,lin.c){ 
  output <- list(lin.a,lin.b,lin.c) 
  class(output) <- "polygon" 
  return(output) 
}

poly.a <- new.polygon(lin.a, lin.b, lin.c)

class(poly.a)

#6. 
plot.point <- function (pt.a,...){
  plot(pt.a$x,pt.a$y)
}
plot.point(pt.a)
plot.point(pt.b)

#7.

plot.line <- function (pt.a, pt.b, ...){
  segments(pt.a$x, pt.a$y, pt.b$x, pt.b$y)
}

plot.line(pt.a,pt.b)

#8.

plot.polygon <- function (lin.a,lin.b,lin.c,...){
  x <- list(lin.a$x,lin.b$x,lin.c$x)
  y <- list(lin.a$y,lin.b$y,lin.c$y)
  polygon (x,y)
}
plot.polygon(pt.a,pt.b,pt.c)

#9.

new.circle <- function (x, y, r){
  output <- list(x=x, y=y, r=r)
  class(output) <- "circle"
  return(output)
}


circle.a <- new.circle (2,2,5)

#10.

area.of.a.circle <- function (r, ...){
  out <- pi*r^2
  return(out)
}
area.of.a.circle(5)

area <- function (pt.a,pt.b,pt.c){
  distance1(pt.a,pt.b,pt.c)
}
area(pt.a,pt.b,pt.c)

#I know the area of a polygon isn't just the distance between the points...
#closest thing I could figure out
#google says if you know the pts of a polygon, you can use the showlace formaula to calculate the area
#But that was really difficult to interpret well enough from math to english I 
#couldn't then get it into code

area2 <-function(pt.a, pt.b, pt.c)
  out <- 0.5*(c((pt.a$x, pt.a$y) + (pt.b$x, pt.b$y) + (pt.c$x, pt.c$y))-c((pt.a$x, pt.a$y) - (pt.b$x, pt.b$y) - (pt.c$x, pt.c$y))) 
  return (out) 
}

################################################
##   Session #4    #############################
################################################

#1.

randos <-replicate(3,rnorm(10,rnorm(1),runif(1, min=0, max=1)))

#2. 

randomrandos <- as.data.frame(randos)
randomrandos

my_super_awesome_summary_function1<- function(x){
  mn <- apply(x, 2, mean)
  rng <- apply(x, 2, range)
  return(c(mn,rng))
}
my_super_awesome_summary_function1(randos)

#3. 

my_super_awesome_categorical_summary_function <- function(x){
  cl <- class(x)
  known <- list(x)
  return(c(cl,known))
}

Rascal <- list(length=40, weight=50, breed="Maine.Coone")
class(Rascal) <- "cat"

Leo <- list(length=20, weight=25, breed="Stray")
class(Leo) <- "cat"

my_super_awesome_categorical_summary_function(Rascal)


#4. Tried, but am still getting an error

plz <- ("please dear god work")
charateristic <- !sapply (plz, is.numeric)

my_super_awesome_does_it_ALL_summary_function <- function(plz){
  numerical <- sapply (plz, is.numeric)
  charateristic <- !sapply (plz, is.numeric)
  rm_x <- subset (plz, numerical)
  ch_x <- subset (plz, characteristic)
  my_super_awesome_summary_function1 (rm_x)
  my_super_awesome_categorical_summary_function (ch_x)
}

my_super_awesome_does_it_ALL_summary_function(1:20)
my_super_awesome_does_it_ALL_summary_function(plz)

#5  

DNAsequence1 <-"ATGTGTTCCTGTTAA"
DNAsequence2 <- "ATG"
what.it.all.means <- (c("start", "stop", "cys", "ser"))
codon <- (c("ATG", "TAA", "TGT", "TCC"))
codontable <- as.data.frame(cbind(what.it.all.means, codon))
codontable

translate <- function (x, table){
  codons <- sapply (seq (1, nchar (x), 3), function (i) substr(x,i,i+2))
  matchy <- match (codons, table$codon)
  return(paste(codontable$what.it.all.means[matchy]))
}

translate(DNAsequence1, codontable)


rm(list=ls())


#6