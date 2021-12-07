#pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
 #              "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
  #             "feather", "htmlwidgets", "broom", "pander", "modelr",
   #            "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
    #           "splines", "ISL2", "MASS", "testthat", "leaps", "caret",
     #          "RSQLite", "class", "babynames", "nasaweather",
      #
#"fueleconomy", "viridis", "boot", "stats")
pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR", "MASS", "testthat", "leaps", "caret",
               "RSQLite", "class", "babynames", "nasaweather",
               "fueleconomy", "viridis", "stats")

# Functions

# 19.2.1.1
rescale01_alt <- function(x, na.rm = FALSE) {
  rng <- range(x, na.rm = na.rm, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01_alt(c(NA, 1:5), na.rm = FALSE)
rescale01_alt(c(NA, 1:5), na.rm = TRUE)
# finite = TRUE i range() vil fjerne alle ikke-endelige elementer, 
# og NA er et ikke-endeligt element.


rescale01_alt2 <- function(x, na.rm = FALSE, finite = FALSE) {
  rng <- range(x, na.rm = na.rm, finite = finite)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01_alt2(c(NA, 1:5), na.rm = FALSE, finite = FALSE)
# Matematiske operationer med NA giver NA


# 19.2.1.2

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == -Inf] <- 0
  y[y == Inf] <- 1
  y
}

rescale01(c(Inf, -Inf, 0:5, NA))

# 19.2.1.3


# Marker nedenstående kodelinje og tryk: Ctrl+Alt+x
proportion_na <- function(x) {
  mean(is.na(x))
}
proportion_na(c(1,2,3))



prop_na <- function(x) {
  mean(is.na(x))
}


sum_to_one <- function(x, na.rm=FALSE) {
  x / sum(x, na.rm = na.rm)
}
sum_to_one(1:5)
sum_to_one(c(1:5, NA))
sum_to_one(c(1:5, NA), na.rm = TRUE)


coef_variation <- function(x, na.rm=FALSE) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = na.rm)
}

coef_variation(1:5)
coef_variation(c(1:5, NA))
coef_variation(c(1:5, NA), na.rm = TRUE)



# 19.2.1.4
variance <- function(x, na.rm = TRUE) {
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  sq_err <- (x - m)^2
  sum(sq_err) / (n - 1)
}
variance(1:10000)
var(1:10000)

skewness <- function(x, na.rm = FALSE) {
  n <- length(x)
  m <- mean(x, na.rm = na.rm)
  v <- var(x, na.rm = na.rm)
  (sum((x - m) ^ 3) / (n - 2)) / v ^ (3 / 2)
}

skewness(c(1, 2, 5, 100))

# 19.2.1.5
both_na <- function(x, y) {
  sum(is.na(x) & is.na(y))
}

both_na(
  c(NA, NA, 1, 2),
  c(NA, 1, NA, 2)
)

both_na(
  c(NA, NA, 1, 2, NA, NA, 1),
  c(NA, 1, NA, 2, NA, NA, 1)
)


# 19.2.1.6
is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0

x <- "C:\\Users\\bjso\\OneDrive - EaDania\\Undervisning\\r4ds\\"
y <- "C:/Users/bjso/OneDrive - EaDania/Undervisning/r4ds/"

# C:\\Users\\bjso\OneDrive - EaDania\\Undervisning\\r4ds\\
# C:/Users/bjso/OneDrive - EaDania/Undervisning/r4ds/

is_directory(x)
is_readable(x)

is_directory(y)
is_readable(y)


# 19.3.1.1

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
f1(c("abc", "abcde", "ad"), "ab")
# f1 -> has_prefix()

f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

f2(c(1, 2, 3))
f2(1:2)
f2(1)
# f2 -> drop_last()


f3 <- function(x, y) {
  rep(y, length.out = length(x))
}
f3(1:3, 4)  
# f3 -> recycle()


# 19.4.4.1
# if tester en enkel betingelse, mens ifelse() tester hvert element.
x <- c(6:-1)
sqrt(ifelse(x >= 0, x, NA))

x <- c(6:-1)
if (x>=0)  sqrt(x) else NA

x <- c(6)
if (x>=0)  sqrt(x) else NA

x <- c(-1)
if (x>=0)  sqrt(x) else NA


# 19.4.4.2
greet <- function(time = lubridate::now()) {
  hr <- lubridate::hour(time)
  if (hr < 12) {
    print("good morning")
  } else if (hr < 17) {
    print("good afternoon")
  } else {
    print("good evening")
  }
}
greet()
greet(ymd_h("2017-01-08:05"))
greet(ymd_h("2017-01-08:13"))
greet(ymd_h("2017-01-08:20"))



Iterations


# 21.2.1.1
output <- vector("double", ncol(mtcars))
names(output) <- names(mtcars)
for (i in names(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}
output

output <- vector("list", ncol(nycflights13::flights))
names(output) <- names(nycflights13::flights)
for (i in names(nycflights13::flights)) {
  output[[i]] <- class(nycflights13::flights[[i]])
}
output


data("iris")
iris_uniq <- vector("double", ncol(iris))
names(iris_uniq) <- names(iris)
for (i in names(iris)) {
  iris_uniq[i] <- n_distinct(iris[[i]])
}
iris_uniq

n <- 10
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mean = mu[i])
}
normals



# 21.2.1.2
out <- ""
for (x in letters) {
  out <- str_c(out, x)
}
out

str_c(letters, collapse = "")


x <- sample(100)
sd. <- 0
for (i in seq_along(x)) {
  sd. <- sd. + (x[i] - mean(x))^2
}
sd. <- sqrt(sd. / (length(x) - 1))
sd.

sd(x)

sqrt(sum((x - mean(x))^2) / (length(x) - 1))

set.seed(117)
x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
out

cumsum(x)

all.equal(cumsum(x), out)


# 21.3.5.1
?dir
dire <- "C:/Users/bjso/OneDrive - EaDania/Undervisning/r4ds/data/"

files <- dir(dire, pattern = "\\.csv$", full.names = TRUE)
files
df_list <- vector("list", length(files))
for (i in seq_along(files)) {
  df_list[[i]] <- read_csv(files[[i]])
}
print(df_list)
# df <- bind_rows(df_list)
# print(df)

# 21.3.5.2
x <- c(11, 12, 13)
print(names(x))
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

x <- c(a = 11, 12, c = 13)
names(x)
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

x <- c(a = 11, b = 12, c = 13)
names(x)
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}




# 21.5.3.1
map_dbl(mtcars, mean)

map_chr(nycflights13::flights, typeof)

map_int(iris, n_distinct)
map_dbl(iris, n_distinct)

purrr::map(c(-10, 0, 10, 100), ~rnorm(n = 10, mean = .))

# 21.5.3.2
is.factor(diamonds$color)
map_lgl(diamonds, is.factor)


# 21.5.3.3
# map funktioner fungerer også sammen med andre vektorer - ikke kun lists.
purrr::map(1:5, runif)

# 21.5.3.4
purrr::map(-2:2, rnorm, n = 5)
map_dbl(-2:2, rnorm, n = 5)


# 21.5.3.5
purrr::map(x, function(df) lm(mpg ~ wt, data = df))

x <- split(mtcars, mtcars$cyl)
purrr::map(x, function(df) lm(mpg ~ wt, data = df))

purrr::map(x, ~ lm(mpg ~ wt, data = .))







