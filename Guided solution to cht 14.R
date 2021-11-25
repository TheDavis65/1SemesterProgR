pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR", "MASS", "testthat", "leaps", "carat",
               "RSQLite", "class", "babynames", "nasaweather",
               "fueleconomy", "viridis")


# 14.2.5.1
paste("Jens", "Jensen")
paste0("Jens", "Jensen")

str_c("Jens", "Jensen")


paste("Jens", NA)
paste0("Jens", NA)

str_c("Jens", NA)

?str_c

# 14.2.5.2

# sep argumentet er en streng, som indsættes mellem argumenter
# i funktionen str_c(), mens collapse er en streng, der bruges 
# i forbindelse med at adskille ethvert element af en character-
# vektor og reducere disse til en vektor af længden 1. 

str_c("Hi", ",", "these", "are", "some", "strings") 
str_c("Hi", ",", "these", "are", "some", "strings", sep = " ")

x <- c("Hi", ",", "these", "are", "some", "strings") 
str_c(x) 
str_c(x, collapse = " ") 

# 14.2.5.3
x <- c("a", "abc", "abcd", "abcde", "abcdef")
L <- str_length(x)
m <- ceiling(L / 2)
m
n <- floor(L/2)
n
str_sub(string = x, start = m, end = m)
str_sub(string = x, start = n+1, end = n+1)

# 14.2.5.4

# Funktionen str_wrap() pakker tekst ind, så den blot fylder 
# en vis bredde, width. 

x <- "Hej med dig, dette er en vanvittig lang streng" 
str_wrap(x, 30)
str_wrap(x, 10)
str_wrap(x, 5)
str_wrap(x, 2)

writeLines(str_wrap(x, 5))


# 14.2.5.5
str_trim(" abc ")
str_trim(" abc ", side = "left")
str_trim(" abc ", side = "right")

str_pad("abc", 5, side = "both")
str_pad("abc", 4, side = "right")
str_pad("abc", 4, side = "left")

# 14.2.5.6

# EN ANDEN DAG

comma_sep <- function(x, delim = ",") {
  n <- length(x)
  if (n == 0) {
    ""
  } else if (n == 1) {
    x
  } else if (n == 2) {
    str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {
    not_last <- str_c(x[seq_len(n - 1)], delim)
    last <- str_c("and", x[[n]], sep = " ")
    str_c(c(not_last, last), collapse = " ")
  }
}
comma_sep("")
comma_sep("a")
comma_sep(c("a", "b"))
comma_sep(c("a", "b", "c"))
comma_sep(c("a", "b", "c", "d"))

# x <- c("a", "b", "c", "d")
# n <- 3
# delim = ","
# not_last <- str_c(x[seq_len(n - 1)], delim)
# not_last

# 14.3.2.1.1
str_view(c("$^$"), "^\\$\\^\\$$")

# 14.3.2.1.2
str_view(stringr::words, "^y", match = TRUE)

str_view(stringr::words, "x$", match = TRUE)

str_view(stringr::words, "^...$", match = TRUE)
stringr::words[str_length(words)==3] # cheating anyway
str_extract_all(stringr::words, "^...$", simplify = TRUE)
str_subset(stringr::words, "^...$")

str_view(stringr::words, ".......", match = TRUE)
str_view(stringr::words, "^[:alpha:]{7,}", match = TRUE)

# 14.3.3.1.1
str_subset(stringr::words, "^[aeiou]")

str_subset(stringr::words, "[aeiou]", negate=TRUE)
# eller 
str_view(stringr::words, "[aeiou]", match=FALSE)

str_subset(stringr::words, "[^e]ed$")
str_subset(stringr::words, "i(ng|se)$")

# 14.3.3.1.2
str_subset(stringr::words, "(cei|[^c]ie)")
length(str_subset(stringr::words, "(cei|[^c]ie)"))

str_subset(stringr::words, "(cie|[^c]ei)")
length(str_subset(stringr::words, "(cie|[^c]ei)"))

# 14.3.3.1.3
qu <- str_subset(stringr::words, "(qu)")
qu_not <- str_subset(stringr::words, "(q.)")

compare(qu, qu_not)

str_view_all(stringr::words, "(q.)", match = TRUE)


# 14.3.4.1.1
# ?	{0,1}	Match at most 1
# +	{1,}	Match 1 or more
# *	{0,}	Match 0 or more

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC*?")
str_view(x, "CC{0,1}")
str_view(x, "CC+")
str_view(x, "CC{1,}")
str_view_all(x, "C[LX]+")
str_view_all(x, "C[LX]{1,}")
str_view_all(x, "C[LX]*")
str_view_all(x, "C[LX]{0,}")
str_view_all(x, "C[LX]*?")

# 14.3.4.1.2
# ^.*$ matcher enhver streng 
str_view(c("Dania", "1000 Kroner", "Søren Sørensen"), "^.*$")

# 14.3.4.1.3
# "\\{.+\\}" matcher enhver streng med Tuborg-klammer omkring mindst
# en karakter
str_view(c("{Dania}", "1000{} Kroner", "Søren{ }Sørensen"), "\\{.+\\}")


# 14.3.5.1.1
# (.)\1\1: Den samme karakter forekommer 3 gange i streg, fx "aaa"


# 14.3.5.1.2

# "(.)(.)\\2\\1": Et karakterpar efterfulgt at det samme karakterpar 
# i modsat rækkefølge, fx. "abba".


# 14.4.1.1.1
words[str_detect(words, "^x|x$")]


# 14.4.1.1.2
str_subset(words, "^[aeiou].*[^aeiou]$") %>% head()


# 14.4.2.1.1
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match
m <- sentences[str_count(sentences, colour_match) > 1]
m

colour_match2 <- str_c("\\b(", str_c(colours, collapse = "|"),")\\b")
colour_match2
str_view_all(m, colour_match2, match = TRUE)

str_extract(sentences, "[A-Za-z]+") %>% head()
str_extract(sentences, "[A-Za-z][A-Za-z']*") %>% head()

# 14.4.3.1.1
# \\b word boundaries
# \\w any word character (\W for non-word chars)

numword <- "\\b(one|two|three|four|five|six|seven|eight|nine|ten) +(\\w+)"
sentences[str_detect(sentences, numword)] %>%
  str_extract(numword)

str_extract(sentences, numword)
