library(tidyverse)

# The functions might be useful for A4
source("C:/Users/caitl/Documents/info201/assignments/a4-chunt170/source/a4-helpers.R")

data <- get_data()

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Min year in dataset
get_min_year <- function() {
  min_year <- data %>%
    summarize(min_year = min(year))
  return(min_year)
}
# Max year in dataset
get_max_year <- function() {
  max_year <- data %>%
    summarize(max_year = max(year))
  return(max_year)
}
# Average 1970s jail population
get_average_jail_pop_1970s <- function() {
  average_jail_pop_1970s <- data %>%
    filter(year < 1980) %>%
    summarize(total_jail_pop_1970s = mean(total_jail_pop, na.rm = TRUE))
  return(average_jail_pop_1970s)
}
# Average 2010s jail population
get_average_jail_pop_2010s <- function() {
  average_jail_pop_2010s <- data %>%
    filter(year >= 2010) %>%
    summarize(total_jail_pop_2010s = mean(total_jail_pop, na.rm = TRUE))
  return(average_jail_pop_2010s)
}
# Average female jail population
get_average_female_jail_pop <- function() {
  average_female_jail_pop <- data %>%
    summarize(female_adult_jail_pop = mean(female_adult_jail_pop, na.rm = TRUE))
  return(average_female_jail_pop)
}
# Average male jail population
get_average_male_jail_pop <- function() {
  average_male_jail_pop <- data %>%
    summarize(male_adult_jail_pop = mean(male_adult_jail_pop, na.rm = TRUE))
  return(average_male_jail_pop)
}
# Average white jail population
get_average_white_jail_pop <- function() {
  average_white_jail_pop <- data %>%
    summarize(white_jail_pop = mean(white_jail_pop, na.rm = TRUE))
  return(average_white_jail_pop)
}
# Average aapi jail population
get_average_aapi_jail_pop <- function() {
  average_aapi_jail_pop <- data %>%
    summarize(aapi_jail_pop = mean(aapi_jail_pop, na.rm = TRUE))
  return(average_aapi_jail_pop)
}
# Average black jail population
get_average_black_jail_pop <- function() {
  average_black_jail_pop <- data %>%
    summarize(black_jail_pop = mean(black_jail_pop, na.rm = TRUE))
  return(average_black_jail_pop)
}
# Average latinx jail population
get_average_latinx_jail_pop <- function() {
  average_latinx_jail_pop <- data %>%
    summarize(latinx_jail_pop = mean(latinx_jail_pop, na.rm = TRUE))
  return(average_latinx_jail_pop)
}
# Average native jail population
get_average_native_jail_pop <- function() {
  average_native_jail_pop <- data %>%
    summarize(native_jail_pop = mean(native_jail_pop, na.rm = TRUE))
  return(average_native_jail_pop)
}
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Creates new total jail population over time df that contains plot specific information
get_year_jail_pop <- function() {
  new_df <- data %>%
    filter(!is.na(total_jail_pop)) %>%
    select(year, total_jail_pop)
  return(new_df)   
}
# Creates total jail population over time plot with df
plot_jail_pop_for_us <- function()  {
  jail_graph <- ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = 'Identity') +
    scale_x_continuous("Timeline (years)", breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
    scale_y_continuous("Total Jail Populations", breaks = c(200000, 400000, 600000, 800000), labels = c("200,000", "400,000", "600,000", "800,000"))
  return(jail_graph)   
}
#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Creates new state over time df that contains plot specific information
get_jail_pop_by_states <- function(states) {
  new_df <- data %>%
    filter(is.element(state, states) & !is.na(total_jail_pop)) %>%
    group_by(year, state) %>%
    summarize(total_jail_pop = sum(total_jail_pop))
  return(new_df)
}
# Creates plot with state over time df
plot_jail_pop_by_states <- function(states) {
  state_graph <- ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, color = state)) + 
    geom_line() +
    scale_x_continuous("Timeline (years)", breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
    scale_y_continuous("Total Jail Populations")
  return(state_graph)
}
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Create df of gender incarceration information
get_gender_df <- function() {
  gender_graph <- data %>%
    filter(!is.na(female_jail_pop) & !is.na(male_jail_pop) & !is.na(female_pop_15to64) & !is.na(male_pop_15to64)) %>%
    group_by(year) %>%
    summarize(female_jail = sum(female_jail_pop), male_jail = sum(male_jail_pop))
  return(gender_graph)
}

# Create plot of gender incarceration
plot_jail_pop_by_gender <- function() {
  state_graph <- ggplot(get_gender_df(), aes(x = year)) + 
    geom_line(aes(y = male_jail, color = "Male")) +
    geom_line(aes(y = female_jail, color = "Female")) +
    scale_x_continuous("Timeline") +
    scale_y_continuous("Total Population (by person)")
  return(state_graph)
}
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Create a df of state coordinates and other information
get_state_shape <- function() {
  state_shape <- map_data("state") %>%
    rename(state = region)
  return(state_shape)
}

# Merge the state incarceration information with the state coordinates
get_total_jail_state <- function() {
  state_df <- data %>%
    filter(!is.na(total_jail_pop)) %>%
    mutate(state = tolower(state.name[match(state, state.abb)])) %>%
    group_by(state) %>%
    summarize(total_jail_pop = sum(total_jail_pop)) %>%
    ungroup() %>%
    left_join(get_state_shape(), by="state")
  return(state_df)
}

# Get map plot of chloropleth total prison population
get_total_jail_state_plot <- function() {
  state_plot <- ggplot(get_total_jail_state()) +
                geom_polygon(
                  mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
                  color = "white",
                  size = .1
                ) +
                coord_map() +
                scale_fill_continuous(breaks = c(750000, 1500000, 2250000, 2900000), low = "#003f5c", high = "#bc5090") +
                labs(fill = "Total Prison Population")
  return(state_plot)
}
#----------------------------------------------------------------------------#