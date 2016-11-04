#1. replicate is an important function that allows you to quickly generate 
#random numbers. Use it to create a dataset of 10 variables, each drawn from 
#a Normal distribution with different means and variances. This can be achieved in one line.

randos <-replicate(3,rnorm(10,rnorm(1),runif(1, min=0, max=1)))

#2. Make your own version of the summary function for continuous datasets 
#(like the one you generated above). You don’t have to slavishly replicated 
#summary.data.frame; write something you would find useful.


randomrandos <- as.data.frame(randos)
randomrandos

my_super_awesome_summary_function1<- function(x){
  mn <- apply(x, 2, mean)
  rng <- apply(x, 2, range)
  return(c(mn,rng))
}
my_super_awesome_summary_function1(randos)

#3. Write a summary function to summarise datasets containing only categorical 
#(...!is.numeric...) data.

my_super_awesome_categorical_summary_function <- function(x){
    cl <- class(x)
    known <- list(x)
    #list$length - is there a way for me to call out one of the list items?
    return(c(cl,known))
}
                                        
Rascal <- list(length=40, weight=50, breed="Maine.Coone")
class(Rascal) <- "cat"

Leo <- list(length=20, weight=25, breed="Stray")
class(Leo) <- "cat"

my_super_awesome_categorical_summary_function(Rascal)

#So this works but is not the most useful function.  I can have it spit back out
#everything I am putting back in, not sure if I should be having it give me
#only one of those things

#and now that I have fixed x not as an vector this doesn't work....
#probably need to do the sapply on a data.frame to calculate column averages, or apply on a matrix to calculate column averages

#4. Finally, make a summary function capable of covering both kinds of data. 
#Hint: if your function doesn’t call the functions above, you’re likely doing it wrong.
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

#5  A molecular biologist you owe a favour approaches you with a problem. 
#They have a DNA sequence (e.g., ‘ACGATATACGA’) that they need to group into codons 
#(groups of three) and translate into proteins (ignoring all complexities of 
#translation and transcription). Write them a function that will take an arbitrary 
#input sequence and an arbitrary codon lookup table, and output the translated sequence. 
#Hint: expand.grid will help you make a demo lookup table.

#expand grid linked all possible row and columns, don't want 

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

#what I had before
#translate <- function (x){
#  codons <- sapply(seq (1, nchar (x), 3), function (i) substr(x,i,i+2))
#  codonmatrix <- matrix(c("start", "stop", "cys", "ser"), ncol=4)
#  colnames(codontable) <- ("ATG", "TAA", "TGT", "TCC")
#  codontable <- as.table(codonmatrix)
#  for(i in codons){
#    return(codontable$i)
#  }
#}

#apparently I messed up making the DNA sequence an object correctly...
class(DNAsequence1)
#Says character, which is correct I think?
#changed "" to '' and now error message says it is an attempt to apply non-function...

#6 Molecular biologist wants you to write a function that will take 
#multiple sequences, translate them, and flag where sequences match




#7 Write a summary-type function to report percentage overlap

#8 no ignoring start and stop codons! (although I'm just saying here I totally put them in)
# So if I remembered stop and start codons even though they didn't,
#do I still need to do this problem?
#Cut off all bits before start, and after stop codon
  