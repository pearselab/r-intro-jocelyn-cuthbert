#1. replicate is an important function that allows you to quickly generate 
#random numbers. Use it to create a dataset of 10 variables, each drawn from 
#a Normal distribution with different means and variances. This can be achieved in one line.

replicate(1,rnorm(10,rnorm(1),runif(1, min=0, max=1)))

#2. Make your own version of the summary function for continuous datasets 
#(like the one you generated above). You don’t have to slavishly replicated 
#summary.data.frame; write something you would find useful.

#normal summary function:
myvector <-c(0:10)
summary(myvector)
#Mynewsummaryfunction
my_super_awesome_summary_function<- function(x){
  matrix(ncol(x), nrow=3)
  apply()
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
    #list$length - is there a way for me to call out one of the list items?
    return(c(cl,list$length))
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

#expand grid linked all possible row and columns, don't want 

Codon <-sapply(seq(1,nchar(x),3))
function(i) substr(sequence,i,i+2)
codontable <-matrix(c("start", "stop", "cys", "ser"), ncol=4)
colnames("ATG", "TAA", "TGT", "TCC")



codon <- c("ATG", "TAA", "TGT", "TCC")
proteins <- c("start", "stop", "cys", "ser")

codontable <- as.data.frame(cbind(codon,proteins))

table

DNAsequence1 <-'ATGTGTTCCTGTTAA'
DNAsequence2 <- "ATG"

translate <- function (x){
  codons <- sapply(seq (1, nchar (x), 3), function (i) substr(x,i,i+2))
  codonmatrix <- matrix(c("start", "stop", "cys", "ser"), ncol=4)
  colnames(codontable)("ATG", "TAA", "TGT", "TCC")
  codontable <- as.table(codonmatrix)
  for(i in codons){
    return(codontable$i)
  }
}

translate(DNAsequence1)

#apparently I messed up making the DNA sequence an object correctly...
class(DNAsequence1)
#Says character, which is correct I think?
#changed "" to '' and now error message says it is an attempt to apply non-function...
  