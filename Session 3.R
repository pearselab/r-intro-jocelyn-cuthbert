#Session 3.R

#1. Implement a cat class, complete with race and print methods. 
Rascal <- list(length=40, weight=50, breed="Maine.Coone")
class(Rascal) <- "cat"

Leo <- list(length=20, weight=25, breed="Stray")
class(Leo) <- "cat"

class(Rascal)
class(Leo)

race <- function (first, second){
  if(!inherits(first, "cat") | !inherits(second, "cat"))
    stop("you haven't given me two cats!")
  if(first$weight < second$weight) {
    print("first cat is the fattest!")
    return(first)
  }
  print("second cat is the fattest!")
  return(second)
}

race(Rascal, Leo)

#2. Implement a point class that holds x and y information for a point in space. 


#3. Write a distance method that calculates the distance between two points in space. 

#4. Implement a line class that takes two point objects and makes a line between them. 

#5. Implement a polygon class that stores a polygon from point objects. Hint: a polygon is really just a
#load of lines.

#6. Write plot methods for point and line objects. 

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
#This is your first time refactoring code (changing it afterwards to make it more streamlined). It’s very hard to do! Is it easier to write all your code again from scratch (it may not be)? If so/not, why?