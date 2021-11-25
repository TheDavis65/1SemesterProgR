# install.packages("purrr")

pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR", "MASS", "testthat", "leaps", "caret",
               "RSQLite", "class", "babynames", "nasaweather",
               "fueleconomy", "viridis", "stats")

#  --------------------------------------
#CHT 17 Improving your programming skills
#  --------------------------------------
#CHT 18 Pipes
#The pipe, %>%, comes from the magrittr 
#package - Tidyverse loads it automatically
#  --------------------------------------
library(magrittr)
#  --------------------------------------

#Memory
diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)

#pryr::object_size() gives the memory occupied by all of its arguments. 
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)
#Abundant dataframes are no problem

diamonds$carat[1] <- NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)


rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

# %T>% works like %>% except that it returns the left-hand side 
# instead of the right-hand side

# %$% It “explodes” out the variables in a data frame so that 
# you can refer to them explicitly
mtcars %>% 
  cor(disp, mpg)

mtcars %$%
  cor(disp, mpg)

cor(mtcars$disp, mtcars$mpg)


# The %<>% operator which allows you to replace code like:
mtcars <- mtcars %>% 
  transform(cyl = cyl * 2)
# with:
mtcars %<>% transform(cyl = cyl * 2)


#  --------------------------------------
#CHT 19 - Functions

#Writing a function has three big advantages over using copy-and-paste:
# 1. You can give a function an evocative name that makes your
# code easier to understand.
# 2. You only need to update code in one place.
# 3. You eliminate the chance of making incidental mistakes when 
# you copy and paste 


vvv <- c(0, 67, 10)
range(vvv)
range(vvv)[1]
range(vvv)[2]



rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(vvv)

# Fjerne kommenteringen og marker de to linjer. Ctrl+ALT+x --> find på godt navn!
# rng <- range(x, na.rm = TRUE)
# (x - rng[1]) / (rng[2] - rng[1])



rescale01(c(0, 67, 10))
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))





#Functions need to have en name (pick good name) ~ rescale01
#List the input  ~ x (if more then add more like x, y, z)
#Code in the body surrounded by curly brackets/braces {}
#It’s easier to start with working code and turn it into a function; 
#it’s harder to create a function and then try to make it work.

set.seed(123)
df <- tibble::tibble(
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100),
  d = rnorm(100)
)
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
df$a
df$b
df$c
df$d



#Only have to make change one place.
x <- c(1:10, Inf)
rescale01(x)


?finite
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)
# The more repetition you have in your code, 
# the more places you need to remember to update 

#If several functions are related then give them the same prefix
#(not sufffix) as the autocomplete makes it much easier


#  --------------------------------------

#Conditional execution
#The condition must evaluate to either TRUE or FALSE.

add <- function(x, y) {
  z <- x + y
  if (z > 10) {
    seq(1:10)
  } else {
    seq(1:9)
   }
}

add(2, 30)
add(1, 2)



calc <- function(x, y, type) {
  if (type == "add") {
    x + y
  } else if (type == "minus") {
    x - y
  } else if (type == "multiply") {
    x * y
  } else if (type == "divide") {
    x / y
  } else {
    stop("Unknown type of operation")
  }
}



calc(2, 3, "multiply")
calc(2, 3, "multiiiiiply")

# if you end up with a very long series of chained if statements, 
# you should consider rewriting. switch() function
?switch()

calc2 <- function(x, y, type) {
  switch(type,
         plus = x + y,
         minus = x - y,
         multiply = x * y,
         divide = x / y,
         stop("Unknown type of operation 2")
  )
}

calc2(2, 3, "multiply")
calc2(2, 3, "multiiiiiply")

?switch

?mean

?stats
require(stats)

centre <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}

set.seed(123)
x <- rcauchy(10)
centre(x, "mean")
centre(x, "median")
centre(x, "trimmed")
centre(x, "triiiimmed")

#Use || (or) and && (and) to combine multiple logical expressions
#If you do have a logical vector, you can use any() or all() to 
#collapse it to a single value.



# Cut --------------------------------------------------------------------
temp <- seq(-10, 50, by = 5)

#(using <=)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), right = TRUE,
    labels = c("freeezing", "cold", "cool", "warm", "hot"))


#(using < )
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), right = FALSE,
    labels = c("freeezing", "cold", "cool", "warm", "hot"))

#Advantage of cut is that it works on vectors, whereas if only works
#on a single value. 

#Function arguments

#Generally, data arguments come first. Detail arguments should go on 
#the end, and usually should have default values. You specify a default 
#value in the same way you call a function with a named argument.
#The default value should almost always be the most common value.

# Compute confidence interval around mean using normal approximation

mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}


x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)


wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}

wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}

wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

#Not same length will not promt an error (vector recycling rules):
wt_mean(1:6, 1:3)


?is.logical(na.rm)
?stopifnot


wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {# hvis na.rm = TRUE så er betingelsen opfyldt
    miss <- is.na(x) | is.na(w) #vector of FALSE AND TRUE; NA -> TRUE
    x <- x[!miss] #Not TRUE i.e. FALSE i.e. available
    w <- w[!miss] #Ditto
  }
  sum(w * x) / sum(w)
} #x and w are local variables.

wt_mean(1:6, 6:1, na.rm = "foo")
wt_mean(1:6, 6:1, na.rm = F)
wt_mean(c(1, 2, 3, ., 5, 6), c(6, 5, 4, 3, 2, 1), na.rm = T)
wt_mean(c(1, 2, 3, 4, NA, 6), c(6, 5, 4, 3, NA, 1), na.rm = T)
wt_mean(c(1, 2, 3, 4, NA, 6), c(6, 5, 4, 3, NA, 1), na.rm = F)
wt_mean(c(1, 2, 3, 4, N, 6), c(6, 5, 4, 3, 2, 1), na.rm = T)
wt_mean(c(1, 2, 3, 4, N, 6), c(6, 5, 4, 3, 2, 1), na.rm = TRUE)
wt_mean(c(1, 2, 3, 4, NA, 6), c(6, 5, 4, 3, 2, NA), na.rm = TRUE)

is.logical(TRUE)
is.logical(FALSE)
is.logical(T)
is.logical(F)
is.logical(NA)

length(FALSE) == 1
length(TRUE) == 1
length(F) == 1
length(T) == 1
length(NA) == 1


# Change the values in the x vector to scope out the values of variables.... 
x <- c(1, NA, 3)
miss <- is.na(x) # | is.na(w)
y <- x[!miss]
miss
x
y

#... dotdotdot


commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

getOption("OutDec")
?getOption
rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 7
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

rule("Important output")
rule(12*12*12)

rule(sum(1, 2))

?paste0
getOption("width")
nchar("Important output")
?cat
?stringr::str_dup()
str_dup("*", 75)
stringr::str_dup("*", 75)


x <- 0
x <- c(0, 2, NA)
x
sum(x, na.rm = TRUE)
sum(x)


fruit <- c("apple", "pear", "banana")
str_dup(fruit, 2)
str_dup(fruit, 1:3)
str_c("ba", str_dup("na", 0:5))

a = 1
b = 3

sum = function(a, b){
  c = a + b
  return(c)
}
sum(a,b)
c = "Hello"
d = "World"

sum2 = function(c, d){
  e = c + d
  return(e)
}
sum2(c, d)

# Return values -----------------------------------------------------------
check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  return(result) #Explicit return statement
}

check(1)
check(-10)
check(0)


check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  result
}
check(1)
check(-10)
check(0)



show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
   invisible(df) #Remove invisible and check if still pipeable  (*******)
}
?invisible

show_missings(mtcars)
#df not printed by default
x <- show_missings(mtcars)
class(x)
dim(x)


#Pipeable (*******)
# View(mtcars)
mtcars %>% 
  show_missings() %>%  # 0 missing
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() # 18 missing



#The environment of functions

f <- function(x) {
  x + y
} 

y <- 100
f(10)
y <- 1000
f(10)
#Since y is not defined inside the function, R will look 
#in the environment where the function was defined:

#Don't do this at home
`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}

1 + 2 #WtF


table(replicate(1000, 1 + 2))

rm(`+`)

?rm
?replicate






##CHT 21 - Iteration

#Functions reduces duplication by identifying repeated patterns of 
#code and extract them out into independent pieces that can be easily 
#reused and updated.


#Iteration helps if you need to do the same thing to multiple inputs: 
#repeating the same operation on different columns, or 
#on different datasets.

#Three basic ways to loop over a vector

#1 - Loop over the elements: for (x in xs)

#2 - Loop over the names: for (nm in names(xs))
results <- vector("list", length(x))
names(results) <- names(x)

#3 - Iteration over the numeric indices is the most general form, 
#because given the position you can extract both the name and the 
#value:

for (i in seq_along(x)) {
  name <- names(x)[[i]]
  value <- x[[i]]
}


#EX 3 - Unknown output length is not good - inefficient
means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

#EX 3 - A better solution to save the results in a list, and 
#   then combine into a single vector after the loop is done:
means <- c(0, 1, 2)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
  nn <- length(out)
}
out
out[1] ## vctor
out[[1]] #list
out[2]
out[3]
str(out)
str(unlist(out))
purrr::flatten_dbl(out)
paste(out, collapse = "")


#Unknown sequence length -  while loop - see below: no of flips varies

# for (i in seq_along(x)) {
#   # body
# }
# 
#  # Equivalent to
# i <- 1
#while (i <= length(x)) {
# # body
#i <- i + 1 
#}


flip <- function() sample(c("T", "H"), 1)
flips <- 0
nheads <- 0

while (nheads < 20) { #Dont try more than say 20! Unless you are bored. no of flips=1.232.731
  if (flip() == "H") { #Here you run the flip function
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips
#Often used in connection with simulation.



?mtcars

str(mtcars)


#  ------------------------------------------------------------------------
#  What is this code doing? How does it work?
trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)

for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}



#  ------------------------------------------------------------------------


df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
) 
#Five variables:
df
typeof(df)
attributes(df)


median(df$a)
median(df$b)
median(df$c)
median(df$d)

#Instead we could make a "for loop".
output <- vector("double", ncol(df))  # 1. output 
# Vector() - type and length 
for (i in seq_along(df)) {            # 2. sequence - what to loop over
  #each run of the for loop will assign i to a different value from seq_along(df)
  #seq_along(df) is a safe version of the familiar 1:length(df)  
  output[[i]] <- median(df[[i]])      # 3. body
}
output


mtcars02 <- vector("double", ncol(mtcars))
names(mtcars02) <- names(mtcars)
#loop over names
for (i in names(mtcars)) {
  mtcars02[i] <- mean(mtcars[[i]])
  }
mtcars02


data(iris)
iris_uniq <- vector("double", ncol(iris))
names(iris_uniq) <- names(iris)
#loop over names
for (i in names(iris)) {
  iris_uniq[i] <- length(unique(iris[[i]]))
}
iris_uniq


# number to draw
n <- 10
# values of the mean
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
# loop over indices
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mu[i])
}
normals


#However, we don’t need a for loop for this since rnorm recycles means.
matrix(rnorm(n * length(mu), mean = mu), ncol = n)



out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
out

stringr::str_c(letters, collapse = "")
letters


#  ------------------------------------------------------------------------


humps <- c("five", "four", "three", "two", "one", "no")
for (i in humps) {
  cat(stringr::str_c("Alice the camel has ", rep(i, 3), " humps.",
                     collapse = "\n"), "\n")
  if (i == "no") {
    cat("Now Alice is a horse.\n")
  } else {
    cat("So go, Alice, go.\n")
  }
  cat("\n")
}


#  ------------------------------------------------------------------------



bottles <- function(i) {
  if (i > 2) {
    bottles <- stringr::str_c(i - 1, " bottles")
  } else if (i == 2) {
    bottles <- "1 bottle"
  } else {
    bottles <- "no more bottles"
  }
  bottles
}

?seq()
beer_bottles <- function(n) {
  # should test whether n >= 1.
  for (i in seq(from = n, to = 1)) {
    cat(stringr::str_c(bottles(i), " of beer on the wall, ", bottles(i), " of beer.\n"))
    cat(stringr::str_c("Take one down and pass it around, ", bottles(i - 1),
                       " of beer on the wall.\n\n"))
  }
  cat("No more bottles of beer on the wall, no more bottles of beer.\n")
  cat(stringr::str_c("Go to the store and buy some more, ", bottles(n), " of beer on the wall.\n"))
}
beer_bottles(100)


#  ------------------------------------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
df$a
df$b
df$c
df$d

#Now with 'for loop':
#Digression: Check values:
seq_along(df)
df

#iterate over each column with seq_along(df)
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
df


# For loops vs. functionals

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
out
#The idea of passing a function to another function is extremely powerful 
#idea, and it’s one of the behaviours that makes R a functional programming 
#language. 
col_summary(df, median)
col_summary(df, mean)
col_summary(df, sd)

#purrr package, which provides functions that eliminate the need for many 
#common for loops.

#The apply family of functions in base R (apply(), lapply(), tapply(), etc) 
#solve a similar problem, but purrr is more consistent and thus is easier 
#to learn.



library(purrr) #A functional programming tool - instead of 'for loops'

#map() makes a list.
#map_lgl() makes a logical vector.
#map_int() makes an integer vector.
#map_dbl() makes a double vector.
#map_chr() makes a character vector.

#The chief benefits of using functions like map() is not speed, but clarity: they 
#make your code easier to write and to read.

df
class(df)

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)


#Similar but more convenient:
df %>% map_dbl(mean)

df %>% map_dbl(median)

df %>% map_dbl(sd)

#Compared to col_summary() (see above) map_*() is a little bit faster 

map_dbl(df, mean, trim = 0.1)
?map_dbl

?mean

z <- list(x = 1:3, y = 4:5)

map_int(z, length)



library(purrr)

#Shortcuts
# fitting a linear model to each group in the mtcars dataset:
models <- mtcars %>% 
  split(.$cyl) %>%  
  purrr::map(function(df) lm(mpg ~ wt, data = df))

models
#the split fct makes the grouping.
#the dot refers to mtcars

View(mtcars)
class(models)



models <- mtcars %>% 
  split(.$cyl) %>% 
  purrr::map(~lm(mpg ~ wt, data = .))
#the dot refers to models <- mtcars %>% 
models[[3]][1][1]


models %>% 
  purrr::map(summary) %>% 
  purrr::map_dbl(~.$r.squared) #Still making summary by groups.

models %>% 
  purrr::map(summary) %>% 
  purrr::map_dbl(~.$r.squared) #Still making summary by groups - contained in models.

#purrr provides an even shorter shortcut: you can use a string.
models %>% 
  purrr::map(summary) %>% 
  purrr::map_dbl("r.squared")



#You can also use an integer to select elements by position:
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% purrr::map_dbl(2)
# [1] 2 5 8


#Base R
#lapply(), vapply(), sapply() 




#Dealing with failure
safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% 
  purrr::map(safely(log))
y
str(y)


#Nicer: This would be easier to work with if we had two lists: one of all 
#the errors and one of all the output 
?purrr::transpose()
yy <- y %>% 
  transpose()
str(yy)

is_ok <- yy$error %>% map_lgl(purrr::is_null)

x[!is_ok]
yy$result[is_ok] %>% 
  flatten_dbl()




?possibly() 
#is simpler than safely(), because you give it a default value to return 
#when there is an error.

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

?quietly()
#performs a similar role to safely(), but instead of capturing errors, 
#it captures printed output, messages, and warnings:
x <- list(2, 1, -1)
x %>% purrr::map(quietly(log)) %>% 
  str()  






#often you have multiple related inputs that you need iterate along in parallel.

mu <- list(5, 10, -3)
mu %>% 
  purrr::map(rnorm, n = 5) %>% 
  str()

?rnorm

set.seed(117)
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)

set.seed(117)
seq_along(mu) %>% 
  purrr::map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

# But that obfuscates the intent of the code. Instead we could use map2() 
# which iterates over two vectors in parallel:
set.seed(117)
map2(mu, sigma, rnorm, n = 5) %>% str()
?rnorm

# Arguments that vary for each call come before the function
# Arguments that are the same for every call come after


#Like map(), map2() is just a wrapper around a for loop:
#  map2 <- function(x, y, f, ...) {
#    out <- vector("list", length(x))
#    for (i in seq_along(x)) {
#      out[[i]] <- f(x[[i]], y[[i]], ...)
#    }
#    out
#  }

?pmap() 
#takes a list of arguments.
n <- list(1, 3, 5)
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)


args1 <- list(n, mu, sigma)

args1 %>%
  pmap(rnorm) %>% 
  str()
#it’s better to name the arguments:

n <- list(1, 3, 5)
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()



params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)

params %>% 
  pmap(rnorm)

?invoke_map()
#invoking different functions
#step up in complexity - as well as varying the arguments to the function 
#you might also vary the function itself:

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)
invoke_map(f, param, n = 5) %>% str()



#
sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

gem <- sim %>% 
  mutate(sim = invoke_map(f, params, n = 10))

gem[[1]]
gem[[2]]
gem[[3]]



?walk
#Walk is an alternative to map that you use when you want to call a function 
#for its side effects, rather than for its return value. You typically do this
#because you want to render output to the screen or save files to a disk - the
#important thing is the action, not the return value.

x <- list(1, "a", 3)
x
x %>% 
  walk(print)


?pwalk
plots <- mtcars %>% 
  split(.$cyl) %>% 
  purrr::map(~ggplot(., aes(mpg, wt)) + geom_point())
#plots
paths <- stringr::str_c(names(plots), ".pdf")
paths
pwalk(list(paths, plots), ggsave, path = tempdir())


#Predicate functions
#keep() and discard() keep elements of the input where the predicate is TRUE 
#or FALSE respectively

iris
View(iris)


iris %>% 
  keep(is.factor) %>% 
  str()


iris %>% 
  discard(is.factor) %>% 
  str()

#some() and every() determine if the predicate is true for any or for all of 
#the elements.

x <- list(1:5, letters, list(10))
x %>% 
  some(is_character)

x %>% 
  every(is_vector)

#detect() finds the first element where the predicate is true; 
#detect_index() returns its position.
x <- sample(10)
x

x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)


#head_while() and tail_while() take elements from the start or end 
#of a vector while a predicate is true:


x <- sample(10)
x

x %>% 
  head_while(~ . < 5)

x %>% 
  tail_while(~ . > 5)



#For example, you might have a list of data frames, and you want to 
#reduce to a single data frame by joining the elements together:
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)

dfs01 <- dfs %>% reduce(full_join)
dfs01


vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)
vs %>% reduce(intersect)

#use it to implement a cumulative sum:
x <- sample(10)
x
x %>% accumulate(`+`)


