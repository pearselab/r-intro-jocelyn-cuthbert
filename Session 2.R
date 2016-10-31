#Session 2
#Loop exercises in handout

#1.  Write a loop that prints out the numbers from 20 to 10
for (i in 20:10) {
  print (i)
}

#2. Write a loop that prints out only the numbers from 20 to 10 that are even
for (i in 20:10) 
  if (i %% 2==0) {
  print (i)
}

#3. Write a function that calculates whether a number is a prime number

is_prime <- function (num) {
  if (num == 2) {
    TRUE
  } else if (any(num%%2:(num-1)==0)) {# see example function of a "any
    FALSE
  } else {
      TRUE
    }
  }

is_prime(7)


is_prime2 <-function (num) {
flag = 0
if (num >1) {
  flag = 1
  for (i in 2:(num-1)) {
    if ((num %% i) == 0) {
    flag = 0
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

#4. Write a loop that prints out the numbers from 1 to 20, printing "Good:NUMBER" if the number is divisible by five
#and "Job: NUMBER" if the number is prime, and nothing otherwise. 
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

#5. A biologist is modelling population growth using a Gompertz curve, which is defined as y(t) = a.e−b.e−c.t
#where y is population size, t is time, a and b are parameters, and e is the exponential function. Write them a function that calculates population size at any time for any values of its parameters.
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


#7.The biologist has fallen in love with your plotting function, but want to colour y values above a as blue, and y values above b as red. Change your function to allow that.

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

#8.You are beginning to suspect the biologist is taking advantage of you. Modify your function to plot in
#purple any y value that’s above a and b. Hint: try putting 3==3 & 2==2 and 3==4 | 2==2 into an if
#statement and see what you get. Using this construction may make this simpler.
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

#9. Write a function that draws boxes of a specified width and height that look like this (height 3, width 5)



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

#10.Modify your box function to put text centred inside the box, like this:

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

#11.  Modify your box function to build boxes of arbitrary text, taking dimensions specified in terms of
#dimensions, not the text. For example, box("wdp", 3, 9, "hey") might produce:

bx(5,1, "  Hey  ", "wdp")


#12.In ecology, hurdle models are often used to model the abundance of species 
#found on surveys. They first model the probability that a species will be present 
#at a site (drawn, for example, from a Bernoulli distribution) and then model the 
#abundance for any species that is present (drawn, for example, from the Poisson distribution). 
#Write a function that simulates the abundance of a species at n sites given a
#probability of presence (p) and that its abundance is drawn from a Poisson with a given λ. 
#Hint: there is no Bernoulli distribution in R, but the Bernoulli is a special case of what distribution?...

presence <- function(p){
  return(rbinom(1, 1, p))
}
#rbinom(n, size, probability)

abundance <- function(p, lam){
  if(rbinom(1, 1, p) == 1){
    return(rpois(1, lam))
  }else{return(0)}
}

abundance(.1,6)

#13. An ecologist really likes your hurdle function (will you never learn?). Write them a function 
#that simulates lots of species (each with their own p and λ) across n sites. Return the results 
#in a matrix where each species is a column, and each site a row (this is the standard used for ecology data in R).
#If we are storing results, and using columns and rows, we need a matrix
#First I will create an empty matrix

mat<-matrix(ncol=1, nrow=nrow(data))
for (i in 1:nrow(data)){
  results=abundance(data$p, data$lam)
  mat[i]=c(results)
}
print(mat)

mat<- matrix(ncol=1, nrow=nrow(data)) #creating an empty matrix to store results in
for (i in 1:nrow(data)){
  results=abundance(data$p, data$lam)
  mat[i]= c(results)
}
print(mat)


#14. Professor Savitzky approaches you with a delicate problem. A member of faculty became disoriented 
#during fieldwork, and is now believed to be randomly wandering somewhere in the desert surrounding Logan. 
#He is modelling their progress through time in five minute intervals, assuming they cover a random, 
#Normally-distributed distance in latitude and longitude in each interval. Could you simulate this process 100 times and plot it for him?

rnorm(100,5,.02)
#this gives you 100 random numbers....but we don't have a mean or standard deviation, 
#and if you want to plot it you want the point to be able to build on each other 
#you could probably use it to see the different between a fast young anxious professor 
#and a old slow calm professor in time before they fell of a cliff. 
a<-matrix(ncol=1)
tm<-(t*5)/60 
#a is matrix of distance over time (in hours that have passed, in 5 min increments)
t<-100
for (i in 1:t){
  movement = runif(1, min=0, max=.5)
  a[i]=c(movement)
}

print(round(a, digits=1))
progress<- cumsum(round(a, digits=1))
#we need to add up his movement from the 5 minute intervals, not just plot 100 random points. 
tm <- seq(5, 500, length=length(a)) 
plot(tm, progress, type="l", xlab="Time (min)", ylab="Movement (miles)")
