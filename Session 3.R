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

lin.a <- list(x=2,5, y=7,3)
class(Line1) <- "line"

lin.b <- list(x=7,5, y=9,2)
class(Line1) <- "line"

lin.c <- list(x=3,6, y=6,12)
class(Line1) <- "line"

class(lin.a)
class(lin.b)
class(lin.c)

#5. Implement a polygon class that stores a polygon from point objects. Hint: a polygon is really just a
#load of lines.
#In this class, need to just loop the line class until all the points are connected?
#method that can be applied to class points and class lines
#I need it to recognize points it needs to connect - if 6 points, not connecting
#every posisble combo, just 1 to 2, 2 to 3, etc. 
#so store the last point or last line?

new.polygon <- function (lin.a,lin.b,lin.c){ 
  output <- list(lin.a,lin.b,lin.c) 
  class(output) <- "polygon" 
  return(output) 
}

poly.a <- list(lin.a, lin.b, lin.c)
class(poly.a) <- "polygon"

class(poly.a)
      
#6. Write plot methods for point and line objects. 
#method is a function that applies to a class
#plot.line = function draws a line
#plot something using plot
#plot(1, 3)
#plot(3 ~ 1)
#plot(1:2, 4:5, type="l")

plot.point <- function (pt.a,...){
  plot(pt.a$x,pt.a$y)
}
plot.point(pt.a)
#works, but this is only one point
plot.points <- function (pt.a, pt.b, ...){
  plot(pt.a$x:pt.a$y, pt.b$x:pt.b$y)
}
plot.points <- function (pt.a, pt.b, ...){
  plot(pt.a$x:pt.b$x, pt.a$y:pt.b$y)
}
plot.points(pt.a,pt.b)
#got an error: Error in xy.coords(x, y, xlabel, ylabel, log) : 
#'x' and 'y' lengths differ for both versions

plot.points <- function (pt.a, pt.b, ...){
  plot(pt.a$x, pt.a$y, pt.b$x, pt.b$y)
}
plot.points(pt.a, pt.b)

# got an invalid xlim value error message...
#tried this then:

plot.points <- function (pt.a, pt.b, ...){
  plot(pt.a$x, pt.a$y, pt.b$x, pt.b$y, xlim <- (0,10), ylim <- (0,10)
}
plot.points(pt.a, pt.b)
#error  Error in strsplit(log, NULL) : non-character argument

plot.points <- function (pt.a, pt.b, ...){
  plot(pt.a$x, pt.a$y, pt.b$x, pt.b$y, xlim=c(0,10), ylim=c(0,10))
}
plot.points(pt.a, pt.b)
# Error in strsplit(log, NULL) : non-character argument
#merpmerpmerp

#ok giving up a bit, plotting a line

plot.line <- function (pt.a, pt.b, ...){
  segments(pt.a$x, pt.a$y, pt.b$x, pt.b$y)
}
plot.line <- function (pt.a, pt.b, ...){
  segments(pt.a$x,pt.a$y,pt.b$x, pt.b$y)
}
plot.line (pt.a, pt.b) 

#error wants me to call new.plot
#wait....not a function to create a new plot
#but a already existing function 
#This function (frame is an alias for plot.new) 
#causes the completion of plotting in the current plot (if there is one) 
#and an advance to a new graphics frame. This is used in all high-level plotting 
#functions and also useful for skipping plots when a multi-figure region is in use.
#ok..... so

plot.new(pt.a, pt.b, pt.c)

#tried just running it
#gave me the same error
#segments: Draw line segments between pairs of points.
#maybe not the one I want to be using?

plot.line <- function (pt.a, pt.b, pt.c, ...){
  plot.new(pt.a, pt.b, pt.c)
  segments(pt.a$x,pt.a$y,pt.b$x, pt.b$y, pt.c$x,pt.c$y)
}
plot.line (pt.a, pt.b, pt.c)

new.plot <- function (pt.a,pt.b,pt.c){ 
  output <- list(pt.a,pt.b,pt.c) 
  class(output) <- "plot" 
  return(output) 
}  


plot_1 <- function (pt.a,pt.b,pt.c){ 
  output <- plot.line (pt.a,pt.b,pt.c) 
}

plot.1 <-new.plot(pt.a,pt.b,pt.c)

plot.line(pt.a,pt.b,pt.c)

#... refers to arguments passed down from a calling function...so I need a function first

plot.new(line1,line2,line3)
line1<-new.line(pt.a$x, pt.a$y, pt.c$x, pt.c$y)
line2<- new.line(pt.b$x, pt.b$y, pt.c$x, pt.c$y)
line3<- new.line(pt.a$x, pt.a$y, pt.b$x, pt.b$y)

#7. Write a plot method for a polygon. Hint: if this isn’t trivial, you’re doing something wrong. 
plot.polygon <- function (lin.a,lin.b,lin.c,...){
  x <- list(lin.a$x,lin.b$x,lin.c$x)
  y <- list(lin.a$y,lin.b$y,lin.c$y)
  polygon (x,y)
}
plot.polygon(pt.a,pt.b,pt.c)

#gah still mad at me for not calling plot new.
# the difference between 


plot(1:2, 4:5, type="l")

points(1, 3)
lines(1:2, 3:4)


#8. Create a canvas object that the add function can add point, line, circle, and polygon objects to.
#Write plot and print methods for this class.

new.canvas <- function (point, line, circle, polygon){
  output <- list(point=point, line=line, circle=circle, polygon=polygon)
  class(output) <- "canvas"
  return(output)
}

print.canvas <- function (ob, ...){
  return(ob)
}

plot.canvas <- function (ob1,ob2,ob3, ...){
  plot.point (ob1)
  plot.line (ob1, ob2)
  plot.polygon (ob1, ob2, ob3)
}

plot.canvas (pt.c, pt.b, pt.a)
# Error message that argument pt.c is missing with no default.
#Error message is still addressing the plot.new()function
#Even when I traced back up and triple checked pt.c was
#put into a pt class/new object and reran plot.new on pts. 

#9. Implement a circle object that takes a point and a radius and 
#stores a circle. Don’t make a circle out of lines!

  

#10. Write area generic methods for circle and polygon objects. 
area <- function(pt.a,pt.b,pt.c){
  distance1(pt.a,pt.b,pt.c)
  
}
area <- function (r){
  UseMethod("area")
}
area(5)

#circle worked, polygon didn't
#really a polygon is more difficult than a circle though
#Circle you only need one variable, r
#for a polygon, the # of sides is unknown, so the 
#equation is unknown - triangle, its area = height x base /2
# for a square its just height X base
#google says if you know the pts of a polygon, you can use the showlace formaula to calculate the area
#so for 3 pts it would be

area <-function(pt.a, pt.b, pt.c) #unexpected comma here error
  out <- 0.5 (((pt.a$x, pt.a$y) + (pt.b$x, pt.b$y) + (pt.c$x, pt.c$y)-((pt.a$x, pt.a$y) - (pt.b$x, pt.b$y) - (pt.c$x, pt.c$y)) 
               #unexpected commas in the line above
               #oh god....I don't think I interpreted those maths right at all
  return (out) #error object out not found? but it worked for circle...
}

area.of.a.circle <- function (r, ...){
  out <- pi*r^2
  return(out)
}
area.of.a.circle(5)

#11. Add support for circle objects to your canvas. 



#12. Add a summary method for canvas that calculates the height and width of the canvas, the fraction of the
#canvas covered in filled-in polygons and circles (if appropriate), and average distance between any points on the canvas (if appropriate).

#13. Add optional colour support for your objects.


#14. Professor Savitzky wants you to update your simulations from the last session to use your new classes. 
#This is your first time refactoring code (changing it afterwards to make it more streamlined). It’s very hard to do! 
#Is it easier to write all your code again from scratch (it may not be)? If so/not, why?


