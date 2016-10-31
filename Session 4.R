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

Leo <- list(length=20, weight=25, breed="Stray")
class(Leo) <- "cat"

my_super_awesome_categorical_summary_function(Leo)

#So this works but is not the most useful function.  I can have it spit back out
#everything I am putting back in, not sure if I should be having it give me
#only one of those things

#4. Finally, make a summary function capable of covering both kinds of data. 
#Hint: if your function doesn’t call the functions above, you’re likely doing it wrong.

my_super_awesome_does_it_ALL_summary_function <- function(x){
  if(is.numeric(x)){
    return(my_super_awesome_summary_function(x))
  } else {
    return(my_super_awesome_categorical_summary_function(x))
  }
}

my_super_awesome_does_it_ALL_summary_function(1:20)
my_super_awesome_does_it_ALL_summary_function(Leo)

#5  A molecular biologist you owe a favour approaches you with a problem. 
#They have a DNA sequence (e.g., ‘ACGATATACGA’) that they need to group into codons 
#(groups of three) and translate into proteins (ignoring all complexities of 
#translation and transcription). Write them a function that will take an arbitrary 
#input sequence and an arbitrary codon lookup table, and output the translated sequence. 
#Hint: expand.grid will help you make a demo lookup table.

sequence<-"ATGTGTTCCTGTTAA"
Codon <-sapply()
codontable <-matrix(c("start", "stop", "cys", "ser"), ncol=4)
  colnames("ATG", "TAA", "TGT", "TCC")
  

  