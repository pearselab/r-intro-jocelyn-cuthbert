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
        flag = 0
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
  } else {
    break
  }
}

#5. 

#6.

#7.

#8.

#9.

#10.

