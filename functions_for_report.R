# clear variables
rm(list=ls())

# include packages
library(nycflights13)
library(highcharter)
library(dplyr)

# suppress summarize warnings
options(dplyr.summarise.inform = FALSE)

airlines <- nycflights13::airlines
flights <- nycflights13::flights
airports <- nycflights13::airports
weather <- nycflights13::weather
planes <- nycflights13::planes

conf <- function(x, level = 0.95, digits = 1) {
  # calculate a confidence interval, rounds to 1 digit automatically
  n = length(x)
  mean = mean(x)
  sd = sd(x)
  return(c(
    mean = round(mean, digits), n = n, sd = round(sd, digits),
    low = round(mean - qnorm(level) * sd / sqrt(n), digits),
    high = round(mean + qnorm(level) * sd / sqrt(n), digits)
  ))
}
delay_by_hour <- function() {
  # delay times broken up 
  
  # round scheduled hour of departure for grouping
  flights$rhour <- flights$hour + round(flights$minute / 60)
  # get rounded mean per hour
  hour.flights <- do.call(data.frame, aggregate(dep_delay ~ rhour , flights, conf))
  
  
  highchart() %>%
    hc_add_series(data = hour.flights$dep_delay.mean, type = "column", name = "Departure Delay") %>%
    hc_add_series(hour.flights, type='errorbar', hcaes(low = dep_delay.low, high = dep_delay.high), name = "error") %>%
    # set axis labels
    hc_yAxis(title = list(text="Average Departure Delay (min)"), min = -2) %>%
    hc_xAxis(title = list(text="Scheduled Hour of Departure"), categories = paste0(hour.flights$rhour, ":00")) %>%
    # set main title
    hc_title(text="Daily Average Departure Delay") %>% 
    # remove legend
    hc_legend(NA) 
}

delay_by_carrier <- function(thresh) {
  # delay times by carrier
  carrier.flights <- do.call(data.frame, aggregate(
    dep_delay ~ carrier, 
    flights,
    conf)
  )
  
  # only consider carriers with high frequency
  carrier.flights <- filter(carrier.flights, dep_delay.n > thresh)
  
  # attach full name of carrier
  carrier.flights <- merge(carrier.flights, airlines, by = "carrier")
  # sort by departure delay
  carrier.flights <- carrier.flights[order(carrier.flights$dep_delay.mean), ]
  
  highchart() %>%
    hc_add_series(data = carrier.flights$dep_delay.mean, type = "column", name = "Departure Delay") %>%
    # add error bar
    hc_add_series(data = cbind(carrier.flights$dep_delay.low, carrier.flights$dep_delay.high), type = "errorbar", name = "error") %>%
    # set axis labels
    hc_yAxis(title = list(text="Average Departure Delay (min)")) %>%
    hc_xAxis(
      type = "category",
      categories = carrier.flights$name) %>%
    # set main title
    hc_title(text = "Average Departure Delay of Popular Carriers") %>%
    # remove legend
    hc_legend(NA)
  
  
}

delay_by_date <- function(window) {
  # delay times by date
  flights$year.day = as.Date(flights$time_hour)
  # flights$year.week = flights$year.day %/% 7

  
  date.flights <- do.call(data.frame, aggregate(
    dep_delay ~ year.day,
    flights,
    function(x) {
      c(total = sum(x), n=length(x))
    }
  ))

  # calc rolling average
  roll.avg.n = rep(0, dim(date.flights)[1] - window + 1)
  roll.avg.delay = rep(0, dim(date.flights)[1] - window + 1)
  for (i in 1:window) {
    roll.avg.n <- roll.avg.n + date.flights$dep_delay.n[i : (dim(date.flights)[1] - window + i)]
    roll.avg.delay <- roll.avg.delay + date.flights$dep_delay.total[i : (dim(date.flights)[1] - window + i)]
  }
  roll.avg.delay <- round(roll.avg.delay / roll.avg.n, 1)
  roll.avg.n <- round(roll.avg.n / window, 1)
  
  # get date labels xaxis, use last day of window
  x <- date.flights$year.day[window:dim(date.flights)[1]]
  
  # define some dates of interest
  my.dates <- list(
    list(value = datetime_to_timestamp(as.Date("2013-12-25")), label = list(text = "Christmas")),
    list(value = datetime_to_timestamp(as.Date("2013-11-28")), label = list(text = "Thanksgiving")),
    list(value = datetime_to_timestamp(as.Date("2013-06-24")), label = list(text = "My birthday")),
    list(value = datetime_to_timestamp(as.Date("2013-07-04")), label = list(text = "US Independence Day"))
  )

  # get default HC colors
  c1 <- getOption("highcharter.color_palette")[1]
  c2 <- getOption("highcharter.color_palette")[2]

  highchart(hc_opts = list(
    title = list(text = sprintf("%s Day Rolling Average Flight Volume and Delay Times", window)),
    # shared separate y Axes, 
    # define here instead of using built in hc_add_yAxis because that does not support shared axes
    yAxis = list(
      list(title = list(text = "Average Departure Delay (min)", color = c1), min = 0, labels = list(style = list(color = c1))),
      list(title = list(text = "Number of Flights"), labels = list(style = list(color = c2)), opposite = TRUE)
    ),
    tooltip = list(shared = TRUE)
  )) %>%
    # format x axis as date variable
    hc_xAxis(type = "datetime", plotLines = my.dates) %>%
    hc_add_series(
      data = data.frame(x = datetime_to_timestamp(x), y = roll.avg.delay),  
      type = "spline", name = "delay", yAxis = 0) %>% 
    hc_add_series(
      data = data.frame(x = datetime_to_timestamp(x), y = roll.avg.n),  
      type = "spline", name = "volume", yAxis = 1)  %>%
    hc_legend(NA)
}

monthly_flight_volume <- function(thresh) {
  df <- flights %>% 
    group_by(month, carrier) %>%
    summarize(n = length(carrier)) %>%
    filter(n > thresh) %>% # limit to high volume carrier
    left_join(airlines, by = "carrier") %>%
    mutate(month.name = format(as.Date(sprintf("2013-%s-01", month)), "%b"))

  # get flight volume by carrier in Jan for sort
  jan <- df[df$month.name == "Jan", ]
  jan.sort <- jan$name[order(jan$n)]
  
  
  uq.months = unique(df$month.name)
  # get all unique carriers, preserving sort order from january
  uq.carriers = unique(c(jan.sort, df$name))
  
  # create grid to ensure that there is a row for each month-carrier combo
  grid <- expand.grid(name = uq.carriers, month.name = uq.months) %>%
    # join summarized data
    left_join(df[c('month.name', 'name', 'n')], by = c('month.name', 'name')) %>%
    replace(is.na(.), 0) # replace NA with 0
  
  
  series = lapply(uq.carriers, function(car) {
    list(
      name = car, 
      data = grid$n[grid$name == car],
      type = "column"
    )
  })

  
  highchart() %>%
    hc_plotOptions(series = list(stacking = 'normal')) %>%
    hc_title(text = "Flights per Carrier per Month") %>%
    hc_yAxis(title = list(text = "Number of Flights")) %>%
    hc_xAxis(categories = uq.months, title = list(text="Month")) %>%
    hc_add_series_list(series)

}


