#1. replicate is an important function that allows you to quickly generate 
#random numbers. Use it to create a dataset of 10 variables, each drawn from 
#a Normal distribution with different means and variances. This can be achieved in one line.

replicate(1, rnorm(10),simplify= "matrix")

#sooo for simplify matrix, array, and vector all look the same?

#2. Make your own version of the summary function for continuous datasets 
#(like the one you generated above). You don’t have to slavishly replicated 
#summary.data.frame; write something you would find useful.

#normal summary function:
myvector <-c(0:10)
summary(myvector)
#Mynewsummaryfunction
my_super_awesome_summary_function<- function(x){
  mn <-mean(x)
  rng <- range(x)
  return(c(mn,rng))
}

my_super_awesome_summary_function(myvector)

#3. Write a summary function to summarise datasets containing only categorical 
#(...!is.numeric...) data.

my_super_awesome_categorical_summary_function <- function(x){
    cl <- class(x)
    known <- list(x)
    return(c(cl,known))
}
                                        
Rascal <- list(length=40, weight=50, breed="Maine.Coone")
class(Rascal) <- "cat"

my_super_awesome_categorical_summary_function(Rascal)

#So this works but is not the most useful function.  I can have it spit back out
#everything I am putting back in, not sure if I should be having it give me
#only one of those things

#4. Finally, make a summary function capable of covering both kinds of data. 
#Hint: if your function doesn’t call the functions above, you’re likely doing it wrong.

my_super_awesome_does_it_ALL_summary_function <- function(x){
  
}
  