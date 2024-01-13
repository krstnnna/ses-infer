# Assignment 4

hello_world <- function(name, is_morning, time_of_day, return_n_greetings) {
  greetings <- c("Lovely day!", "Woo, it's cold outside!", "Annyeong is Hello in Korean!", "Happy 2024!")
  
  # Conditional statement to select greeting
  if (is_morning) {
    greeting <- greetings[1]
  } else {
    greeting <- greetings[sample(4, 1)] # Randomly choose one of the 4 greetings
  }
  
  # Loop to return the specified number of greetings
  result <- c()
  for (i in 1:return_n_greetings) {
    if (is_morning) {
      greeting <- greetings[1]  # Specific morning greeting
    } else {
      greeting <- sample(greetings, 1)  # Randomly select one greeting
    }
    selected_greeting <- paste(greeting, ifelse(time_of_day <= 12, "Dress warm,", "Have a wonderful week,"), name)
    result <- c(result, selected_greeting)
  }
  
  return(result)
}

**Example**
hello_world("My friend", FALSE, 10, 3)
