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
  flag = 0
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
pop_plot <-function(t, y){
  plot(t,y, xlab="Months", ylab="Population")}
pop_plot (12, 4)

#7.The biologist has fallen in love with your plotting function, but want to colour y values above a as blue, and y values above b as red. Change your function to allow that.
pop_plot <-function(t, y){
  plot(t,y, xlab="Months", ylab="Population", col=ifelse(a<y, "blue", ifelse(y>b, "red", "black")))
}
pop_plot(8,6)

#8.You are beginning to suspect the biologist is taking advantage of you. Modify your function to plot in
#purple any y value that’s above a and b. Hint: try putting 3==3 & 2==2 and 3==4 | 2==2 into an if
#statement and see what you get. Using this construction may make this simpler.
pop_plot <-function(t, y){
  plot(t,y, xlab="Months", ylab="Population", col=ifelse(y>a & y>b, "purple"))
}
pop_plot(8,65)

#9. Write a function that draws boxes of a specified width and height that look like this (height 3, width 5)
#so......
Draw_box <-function(X,Y){
  box(which = "plot", lty= "solid", dim(X))
}
Draw_box(3,5)

Draw_rectangle <-function(X,Y){
rect(xleft=1, ybottom=1, xright=5, ytop=3, density=NULL, angle=90, col=NULL, border=NULL, lty=par("lty"), lwd=1,...)
}
symbols(x, y = NULL, stars=(13),
        inches = TRUE, add = FALSE,
        fg = par("col"), bg = NA,
        xlab = NULL, ylab = NULL, main = NULL,
        xlim = NULL, ylim = NULL, ...)

#10.Modify your box function to put text centred inside the box, like this:

