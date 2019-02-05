library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

fires <- read_csv("forestfires.csv")

#Change data type of column to factor
fires <- fires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))) %>%
  mutate(day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))) %>%
  filter(area != 0)

#Visualize the number of forest fires occuring during each month
fires_by_month <- fires %>%
  group_by(month) %>%
  summarise(count = n())

ggplot(data = fires_by_month) +
  aes(x = month, y = count) +
  geom_bar(stat = "identity") +
  labs(title = "Occurence of Forest Fires by Month") +
  theme(panel.background = element_rect(fill = "white"))

#Visualize the number of forest fires occuring on each weekday
fires_by_weekday <- fires %>%
  group_by(day) %>%
  summarise(count = n())

ggplot(data = fires_by_weekday) +
  aes(x = day, y = count) +
  geom_bar(stat = "identity") +
  labs(title = "Occurence of Forest Fires by Weekday") +
  theme(panel.background = element_rect(fill = "white"))

# Write function to easily create boxplots
create_boxplot_by_month = function(x, y) {
  ggplot(data = fires) + 
    aes_string(x = x, y = y) +
    geom_boxplot() +
    theme(panel.background = element_rect(fill = "white"))
}

# Create vectors of variable names
x_var <- names(fires)[3]
y_var <- names(fires)[5:12]

# Apply the function to create the plots
variables_by_month <- map2(x_var, y_var, create_boxplot_by_month)

# Write function to easily create scatterplots to visualize distribution of variables by month
create_scatter_by_month = function(x,y) {
  ggplot(data = fires) + 
    aes_string(x = x, y = y) +
    geom_point(stat = "identity") +
    theme(panel.background = element_rect(fill = "white"))
}

# Create vectors of variable names
x_var <- names(fires)[5:12]
y_var <- names(fires)[13]

# Apply the function to create the plots
variables_by_month_scatter <- map2(x_var, y_var, create_scatter_by_month)

# Print plots
variables_by_month_scatter