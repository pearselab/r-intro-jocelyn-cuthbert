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
num = as.integer (10)

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

#4. Write a loop that prints out the numbers from 1 to 20, printing "Good:NUMBER" if the number is divisible by five
#and "Job: NUMBER" if the number is prime, and nothing otherwise. 
for (i in 1:20) 
  if (i %% 5==0) {
    print ("Good:NUMBER")
  } else {
    if (i)
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
  }
        
        if(num== 2)  flag = 1
        if(flag ==1) {
          print(paste("Job:NUMBER"))
        } else {
          break
        }

#5. 

#6.

#7.

#8.

#9.

#10.

