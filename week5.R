#Tasks and inputs

testfun <- function() {}

testfun()

class(testfun)

testfun <- function() {
  print("this function does nothing")
}

testfun()

testfun <- function(sometext) {
  print(sometext)
}

testfun(sometext = "this function does slightly more, but still not much")

my_age <- function(birthday, output_unit) {
  difftime(Sys.time(), birthday, units = output_unit)
}

my_age(birthday = "1997-04-23", output_unit = "days")

my_age("1997-04-23", "days")

my_age <- function(birthday, output_unit = "days") {
  difftime(Sys.time(), birthday, units = output_unit)
}

# if not stated otherwise, our function uses the unit "days"
my_age("1997-04-23")

# We can still overwrite units
my_age("1997-04-23", "hours")

# Task 1: Write your own functions ----------------------------------------

# Function to calculate BMI
calculate_bmi <- function(weight_kg, height_m) {
  bmi <- weight_kg / (height_m ^ 2)
  return(bmi)
}

# Function to convert Celsius to Fahrenheit
celsius_to_fahrenheit <- function(celsius) {
  fahrenheit <- (celsius * 9/5) + 32
  return(fahrenheit)
}

# Function to calculate Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(distance)
}

# Task 2: Prepare Analysis ------------------------------------------------

library(tidyverse)

data <- read_delim("Datasets/wildschwein_BE_2056.csv")

data <- data %>% 
  filter(TierName %in% c("Sabi", "Rosa") & 
           DatetimeUTC >= as.POSIXct("2015-04-01") & 
           DatetimeUTC <= as.POSIXct("2015-04-15"))

# Task 3: Create Join Key -------------------------------------------------

data <- data %>%
  mutate(DatetimeUTC = round_date(DatetimeUTC, unit = "15 minutes"))

# Task 4: Measuring distance at concurrent locations ----------------------

sabi <- filter(data, TierName == "Sabi")

rosa <- filter(data, TierName == "Rosa")


wildschwein_join <- inner_join(sabi, rosa, by = "DatetimeUTC", suffix = c("_sabi", "_rosa"))


# build function to calculate euclidian distance

euclid <- function(x1, x2, y1, y2){
  distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(distance)
}

wildschwein_join$distance <- euclid(rosa$E, sabi$E, rosa$N, rosa$N)

wildschwein_join <- wildschwein_join %>% 
  mutate(close = distance <= 100)

# Task 5: Visualize data --------------------------------------------------

# meet and greet
meet <- wildschwein_join  %>%
  filter(close == TRUE)

ggplot() +
  geom_point(data = rosa, aes(x = E, y = N), color = "black", size = 1.5, alpha = 0.3) +
  geom_point(data = sabi, aes(x = E, y = N), color = "red4", size = 1.5, alpha = 0.3) +
  # Meets
  geom_point(data = meet, aes(x = E_rosa, y = N_rosa, alpha = 0.75), color = "blue", size = 3) +
  geom_point(data = meet, aes(x = E_sabi, y = N_sabi, alpha = 0.75), color = "green", size = 3) +
  ggtitle("meet and greet") +
  xlab("E") + ylab("N") +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

# Task 6 (optional): Visualize data as timecube with plotly ---------------

library(plotly)

plot_ly() %>%  
  # Rosa
  add_trace(data = rosa, x = ~E, y = ~N, z = ~DatetimeUTC, type = 'scatter3d', mode = 'lines+markers', line = list(color = '#cea50b'), marker = list(size = 1, color = '#cea50b'), name = 'Rosa') %>% 
  # Sabi
  add_trace(data = sabi, x = ~E, y = ~N, z = ~DatetimeUTC, type = 'scatter3d', mode = 'lines+markers', line = list(color = '#7F1425'), marker = list(size = 1, color = '#7F1425'), name = 'Sabi') %>% 
  # Meet and Greet
  add_trace(data = meet, x = ~E_rosa, y = ~N_rosa, z = ~DatetimeUTC_sabi, type = 'scatter3d', mode = 'markers',
            marker = list(size = 7, color = 'green', symbol = 'circle'), name = 'Hello There') %>% 
  layout(scene = list(xaxis = list(title = 'E'),
                      yaxis = list(title = 'N'),
                      zaxis = list(title = 'Time')))



