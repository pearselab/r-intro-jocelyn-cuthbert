################################################
## Exercises ###################################
################################################

# c - concatenate
#     - takes two (or more) vector and joins them together
c(1, 2, 3)
c(c(1,2,3), c(4,5,6))
#     - they need to be of the same type, though!
c(1,2, "three")

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
## Bonus exercises #############################
################################################

bonus.text <- "It was the best of times, it was the worst of times, it was the age of
wisdom, it was the age of foolishness, it was the epoch of belief, it
was the epoch of incredulity, it it was the season of Light, it was the
season of Darkness, it was the spring of hope, it was the winter of
despair, we had everything before us, we had nothing before us, we
were all going direct to Heaven, we were all going direct the other
way- in short, the period was so far like the present period, that
some of its noiosiest authorities insisted on its being received, for
good or for evil, in the superlative degree of comparison only."
