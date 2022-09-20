#BAN400 - Lecture 2 Iterations

#Loading packages
library(purrr)
library(tidyverse)

#Toy data ----------------------------------------------------------------------

df <- 
  tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10),
    e = rnorm(10)
  )

head(df)
tail(df)

#Column summaries --------------------------------------------------------------

mean(df$a)
mean(df$b)
mean(df$c)
mean(df$d)
mean(df$e)

#Repetitive code, inefficient

#Loops are more efficient ------------------------------------------------------

for(i in 1:ncol(df)) {
  print(
    mean(
      df[[i]])
    )
}

#Better way to iterate, returns the indices of the columns
seq_along(df)

for(i in seq_along(df)) {
  print(
    mean(
      df[[i]])
  )
}

#Storing results
out <-
  vector("double", length(df))

for(i in seq_along(df)) {
  out[i] <-
    mean(
      df[[i]]
  )
}

out

#Defining function -------------------------------------------------------------
column_means <- function(df) {
  for(i in seq_along(df)) {
    out[i] <-
      mean(
        df[[i]]
      )
  }
}

column_means(df)
out


#Making the function generic ---------------------------------------------------

column_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for(i in seq_along(df)) {
    out[i] <-
      fun(
        df[[i]]
      )
  }
  out
}

column_summary(df, mean)
column_summary(df, median)
column_summary(df, sd)

#-------------------------------------------------------------------------------

#purrr has much of the same functionality
map_dbl(df, mean)

map_dbl(df, mean, trim=,1)

#Standardized means (mean(x)/sd(x))

map_dbl(df, ~ mean(.) / sd(.))


# map and regressions -----------------------------------------------------

#dataset
mtcars

#regression, splitting on number of cylinders (4,6, or 8)
mtcars %>% 
  split(.$cyl) %>% 
  map(~ lm(mpg~wt, data = .)) %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)


# dealing with errors -----------------------------------------------------

x <- list(1,10,"a")

y <- map(x, safely(log))

map(x, safely(log))

map(x, possibly(log, NA_real_))


# iterations over multiple lists ------------------------------------------

#defining two lists
mu <- list(-10000, 0, 10000)
sd <- list(1, 5, 10)

#Use map2 to iterate over two lists simultaneously
map2(mu, sd, rnorm, n = 5)

n <- list(1,100,25)

#Use pmap to iterate over >2 lists simultaneously

list(
  mean = mu,
  sd = sd,
  n = n
) %>% pmap(rnorm)





