#1  basic loop functions

#1) 

x <-100
for (i in 1:5) {
 x <- x / 13
 print(x)
}

starting <- vector()
x <-2
y <-2
for (i in 2:7) {
  z <- (x+y)^i
  print(z)
}


# 2 Looping over non-interger vectors
   #1)
shrek <- c("WHAT", "ARE", "YOU", "DOING","ON", "MY", "SWAMP")

for (i in 1:length(shrek)) {
  print(paste(tolower(shrek[i])))
}

  #2)

for (i in shrek) {
  print(tolower(i))
}

  #3
fuck_words <- c("buzz","cross","by","fore","broads", "greats"," ")
wordwords <- gsub( pattern = "$", replacement = "word", x = fuck_words)
wordwords <- vector()
for (i in 1:length(fuck_words)) {
  wordwords[i] <- output[i]
  print(wordwords)
}
wordwords

  #4
surnames <- c("Harris", "Jones", "Lewis", "Lechter", "Bright", 
              "Harvey", "Weissman", "Doyle", "Sakurai", "Doe")

for (i in ) {
  
}

#3

  #1
x <- 0.999

while (x >= 0.5) {
  x <- x^2
  print(paste("The number is now", x))
}

y <- 0

while (x >= 0.5) {
  x <- x^2
  y <- y + 1
  print(paste("if you square 0.999", y, "times, you get the value of",
              x, "where", y, "is the number of loops" ))
}

#4
  #1)
fib_vec <- function(x){
sequence <- c(1,1)
for (i in 3:20) {
  sequence[i] <- sequence[i-1] + sequence[i-2]
 } 
return(sequence)
}
 fib_vec()
 
 #2)
fib <- function(x,y,z){
  sequence <- c(x,y,z)
  for (i in 4:20){
    sequence[i] <- sequence[i-1] + sequence[1-2] + sequence[i-3]
  }
  return(sequence)
}
fib(1,1,1)

warning()

#5

  #1)




  #2)







