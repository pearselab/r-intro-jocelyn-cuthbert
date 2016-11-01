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

pt1 <-new.point(42,1)
pt2 <-new.point(4,10)
pt3 <-new.point(2,12)

#3. Write a distance method that calculates the distance between two points in space. 
#a^2+b^2=c^2
#A method can only give us one thing back - in this case I want distance
dist(pt3, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)


#4. Implement a line class that takes two point objects and makes a line between them. 
#so not a method, a class. 

new.line <- function(x,y){ 
  output <- list(x-y) #or some distance function?
  class(output) <- "line" 
  return(output) 
}
#Not totally sure how to implement a funciton for a class- in 
#this case you want it to draw a line?  Or just in theory draw a line
#I think we are just creating a line class for the polygon function 
#And maybe using the distance method within the class for output?

Line1 <- list(x=2,5, y=7,3)
class(Line1) <- "line"

class(Line1)

#5. Implement a polygon class that stores a polygon from point objects. Hint: a polygon is really just a
#load of lines.
#In this class, need to just loop the line class until all the points are connected?
#method that can be applied to class points and class lines
#I need it to recognize points it needs to connect - if 6 points, not connecting
#every posisble combo, just 1 to 2, 2 to 3, etc. 
#so store the last point or last line?

#6. Write plot methods for point and line objects. 

#won't do until I have the point and line objects done
plot(x, y, ...)
#type we want is b = both points and lines, or just p points l lines
#don't need titles
#x<-list(1,2,3)
#y<-list(1,2,3)
#?Is this how I tell it what the different points are?
#... refers to arguments passed down from a calling function...so I need a function first


#7. Write a plot method for a polygon. Hint: if this isn’t trivial, you’re doing something wrong. 


#8. Create a canvas object that the add function can add point, line, circle, and polygon objects to.
#Write plot and print methods for this class.



#9. Implement a circle object that takes a point and a radius and stores a circle. Don’t make a circle out of lines!

  

#10. Write area generic methods for circle and polygon objects. 



#11. Add support for circle objects to your canvas. 



#12. Add a summary method for canvas that calculates the height and width of the canvas, the fraction of the
#canvas covered in filled-in polygons and circles (if appropriate), and average distance between any points on the canvas (if appropriate).

#13. Add optional colour support for your objects.


#14. Professor Savitzky wants you to update your simulations from the last session to use your new classes. 
#This is your first time refactoring code (changing it afterwards to make it more streamlined). It’s very hard to do! 
#Is it easier to write all your code again from scratch (it may not be)? If so/not, why?


