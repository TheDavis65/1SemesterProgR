pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS", "testthat", "leaps", "caret",
               "RSQLite", "class", "babynames", "nasaweather",
               "fueleconomy", "viridis")

# Factors: forcats

# 15.3.1.1
rincome_plot <-
  gss_cat %>%
  ggplot(aes(x = rincome)) +
  geom_bar()
rincome_plot

rincome_plot +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rincome_plot +
  coord_flip()


gss_cat %>%
  filter(!rincome %in% c("Not applicable")) %>%
  mutate(rincome = fct_recode(rincome,
                              "Less than $1000" = "Lt $1000"
  )) %>%
  mutate(rincome_na = rincome %in% c("Refused", "Don't know", "No answer")) %>%
  ggplot(aes(x = rincome, fill = rincome_na)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous("Number of Respondents", labels = scales::comma) +
  scale_x_discrete("Respondent's Income") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray")) +
  theme(legend.position = "None")

gss_cat %>%
  filter(!rincome %in% c("Not applicable", "Don't know", "No answer", "Refused")) %>%
  mutate(rincome = fct_recode(rincome,
                              "Less than $1000" = "Lt $1000"
  )) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous("Number of Respondents", labels = scales::comma) +
  scale_x_discrete("Respondent's Income")

# 15.3.1.2
gss_cat %>%
  count(relig) %>%
  arrange(desc(n)) %>%
  head(1)

gss_cat %>%
  count(partyid) %>%
  arrange(desc(n)) %>%
  head(1)

# 15.3.1.3
levels(gss_cat$denom)
# Protestant

gss_cat %>%
  filter(!denom %in% c(
    "No answer", "Other", "Don't know", "Not applicable",
    "No denomination"
  )) %>%
  count(relig)

gss_cat %>%
  count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom, size = n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))


# 15.4.1.1
summary(gss_cat[["tvhours"]])

gss_cat %>%
  filter(!is.na(tvhours)) %>%
  ggplot(aes(x = tvhours)) +
  geom_histogram(binwidth = 1)

# median i stedet for gennemsnit?

# 15.4.1.2
xxxx <- keep(gss_cat, is.factor) %>% names()
levels(gss_cat[["marital"]])
gss_cat %>%
  ggplot(aes(x = marital)) +
  geom_bar()
levels(gss_cat$race)
gss_cat %>%
  ggplot(aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

levels(gss_cat$rincome)
gss_cat %>%
  ggplot(aes(rincome)) +
  geom_bar() +
  coord_flip()

levels(gss_cat$relig)
gss_cat %>%
  ggplot(aes(relig)) +
  geom_bar() +
  coord_flip()

levels(gss_cat$partyid)
gss_cat %>%
  ggplot(aes(partyid)) +
  geom_bar() +
  coord_flip()

# 15.4.1.3
# level = “Not applicable” --> heltalværdi på 1.


# 15.5.1.1
levels(gss_cat$partyid)

gss_cat %>%
  mutate(
    partyid =
      fct_collapse(partyid,
                   other = c("No answer", "Don't know", "Other party"),
                   rep = c("Strong republican", "Not str republican"),
                   ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                   dem = c("Not str democrat", "Strong democrat")
      )
  ) %>%
  count(year, partyid) %>%
  group_by(year) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(
    x = year, y = p,
    colour = fct_reorder2(partyid, year, p)
  )) +
  geom_point() +
  geom_line() +
  labs(colour = "Party ID.")


# 15.5.1.2
levels(gss_cat$rincome)

library("stringr")
gss_cat %>%
  mutate(
    rincome =
      fct_collapse(
        rincome,
        `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
        `Lt $5000` = c("Lt $1000", str_c(
          "$", c("1000", "3000", "4000"),
          " to ", c("2999", "3999", "4999")
        )),
        `$5000 to 10000` = str_c(
          "$", c("5000", "6000", "7000", "8000"),
          " to ", c("5999", "6999", "7999", "9999")
        )
      )
  ) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip()


# Dates and times: lubridate

# 16.2.4.1
ret <- ymd(c("2010-10-10", "bananas"))
print(class(ret))
ret
# En advarselsbesked

# 16.2.4.2
# Den bestemmer tidszone for date. Eftersom forskellige tidszoner kan have 
# forskellige dates, kan today() variere afhængigt af den akutelle tidszone.

today()


# 16.2.4.3
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14"

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)


# 16.3.4.1
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  dplyr::select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>%
  filter(!is.na(dep_time)) %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  mutate(month = factor(month(dep_time))) %>%
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(binwidth = 60 * 60)


flights_dt %>%
  filter(!is.na(dep_time)) %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  mutate(month = factor(month(dep_time))) %>%
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(aes(y = ..density..), binwidth = 60 * 60)

# 16.3.4.2

# Konsistent: dep_time = sched_dep_time + dep_delay
flights_dt %>%
  mutate(dep_time_ = sched_dep_time + dep_delay * 60) %>%
  filter(dep_time_ != dep_time) %>%
  dplyr::select(dep_time_, dep_time, sched_dep_time, dep_delay) 

# Der er tydeligvis nogle uoverensstemmelser. Der er flights hvor faktisk
# afgang er den næste dag ift. sched_dep_time.
# Vi skulle have konstrueret dep_time som sched_dep_time + dep_delay

# 16.3.4.3
flights_dt %>%
  mutate(
    flight_duration = as.numeric(arr_time - dep_time),
    air_time_mins = air_time,
    diff = flight_duration - air_time_mins,
    flight_dur2 = sched_arr_time - sched_dep_time + (arr_delay - dep_delay)  
  ) %>%
  dplyr::select(origin, dest, flight_duration, air_time_mins, diff, flight_dur2)

# 16.3.4.4
# sched_dep_time!
flights_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()
hour(flights_dt$sched_dep_time)
# 16.3.4.5
flights_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  print(n = Inf)
wday(flights_dt$sched_dep_time)
flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = ave_dep_delay)) + 
  geom_bar(stat = "identity")


flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarize(ave_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = ave_arr_delay)) + 
  geom_bar(stat = "identity")
?geom_bar
# 16.3.4.6

table(diamonds$carat)

ggplot(diamonds, aes(x = carat)) +
  geom_density()

ggplot(diamonds, aes(x = carat %% 1)) +
  geom_histogram(binwidth = 0.01)

ggplot(flights_dt, aes(x = minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)


# 16.3.4.7
flights_dt %>% 
  mutate(minute = minute(dep_time), early = dep_delay < 0) %>% 
  group_by(minute) %>% 
  summarise(early = mean(early, na.rm = TRUE)) %>% 
  ggplot(aes(minute, early)) +
  geom_line()

# 16.4.5.1 
ddays(12)
dweeks(43)
dmonths(1)*3 == dmonths(3)
dmonths(3)

# months() har forskellige længder

# 16.4.5.2 
# Variablen **overnight** er lig med TRUE eller FALSE. Hvis der er
# tale om en overnight flight, så bliver days(overnight * 1) = 1, og
# hvis ikke, så bliver days(overnight * 1) = 0, og ingen days er føjet
# til date.

?months
# 16.4.5.3
ymd("2015-01-01") + months(0:11)
floor_date(today(), unit = "year") + months(0:11)
?floor_date

x <- ymd_hms("2009-08-03 12:01:59.23")
floor_date(x, ".1s")
floor_date(x, "second")
floor_date(x, "minute")
floor_date(x, "hour")
floor_date(x, "day")
floor_date(x, "week")
floor_date(x, "month")
floor_date(x, "bimonth")
floor_date(x, "quarter")
floor_date(x, "season")
floor_date(x, "halfyear")
floor_date(x, "year")
x
