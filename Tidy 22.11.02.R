# Title, Author, Purpose, Date --------------------------------------------

## The Secretary Problem, with variation
## Robert Novo
## Purpose: To simulate the secretary problem
## 2022 Oct. 28

# Load libraries----
library(tidyverse)
library(ggthemes)

# Create n=reps lists of random integers from 1:applicant_pool----
reps <- 100
applicant_pool <- 100
candidates <- matrix(nrow = reps, ncol = applicant_pool)
candidates <- as_tibble(candidates) # output of loop
for (i in 1:reps) {
  candidates[i, ] = as.list(sample.int(applicant_pool, applicant_pool,
                               replace = FALSE))
}
candidates

# Run the simulation n=reps times----

# The loop 
# Create a tibble representing the baseline 
baseline_list <- as_tibble(candidates, dim=c(reps, applicant_pool)) # Output of loop
for (n in 1:nrow(candidates)) {
  for(i in seq_along(candidates)) {
    baseline_list[n, i] <- max(candidates[n, 1:i])
  }
}

# Create a loop showing the results of the hiring the process
n_iter <- reps # Number of iterations of the loop

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for(i in 1:reps) {
  
  #---------------------
  # Code to be executed
  #---------------------
  
  hired_list <- as_tibble(candidates, dim=c(reps, applicant_pool)) # Output of loop
  for (n in 1:nrow(candidates)) {
    for (i in seq_along(candidates)) {
      if (candidates[n, i] > baseline_list[n, i]) {
        hired_list[n, i] <- candidates[n, i]
      }
      else {
        for (j in i:ncol(candidates)) {
          if (candidates[n, j] > baseline_list[n, i] ||
              candidates[n, j] == applicant_pool) {
            hired_list[n, i:j] <- candidates[n, j]
            break
          }
          else {
            hired_list[n, j:applicant_pool] <- candidates[n, applicant_pool]
          }
        }
      }
    }
  }
  
  # Sets the progress bar to the current state
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

# Find mean probability of hiring best candidate for each strategy---- 
hired_list <- hired_list %>%
  add_column(trial = 1:reps, .before = TRUE)

hire_better_than <- function(a) {
  p_hiring_best <- vector("double", length(applicant_pool))
  for (i in 1:applicant_pool) {
    p_hiring_best[i] <- (length(which(hired_list[, i] >= a)))/reps
  }
  p_hiring_best <- tibble(strategy = 1:applicant_pool, p = p_hiring_best)
}

p_hiring_best <- hire_better_than(100)

# Graph results----
ggplot(p_hiring_best) +
  geom_line(aes(x = strategy, y = p), color = "blue") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_clean()

# Find highest probability of hiring best candidate as function of selectivity----
highest_probability <- function(a) {
  p_hiring_best <- vector("double", length(applicant_pool))
  for (i in 1:applicant_pool) {
    p_hiring_best[i] <- (length(which(hired_list[, i] >= a)))/reps
  }
  return(max(p_hiring_best))
}

probability_table <- tibble(baseline = 1:applicant_pool,
                            highest_p = 0)
for (i in 1:applicant_pool) {
  probability_table[i, 2] <- highest_probability(i)
}
probability_table


## Graph results----
ggplot(probability_table) +
  geom_line(aes(x = baseline, y = highest_p), color = "blue") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(ylim = c(0, 1)) + 
  ggtitle("Probability of hiring best candidate as a function of selectivity", 
          subtitle = "E.g., y for baseline = 90 gives you the chance of selecting a top ten candidate") +
  theme_clean()

## Let's zoom in---
ggplot(probability_table) +
  geom_point(aes(x = baseline, y = highest_p), color = "blue") +
  coord_cartesian(xlim = c(50, 100), ylim = c(0, 1)) + 
  scale_x_continuous(breaks = seq(50, 100, by = 5)) +
  theme_clean()

ggplot(probability_table) +
  geom_point(aes(x = baseline, y = highest_p), color = "blue") +
  coord_cartesian(xlim = c(80, 100), ylim = c(0, 1)) + 
  scale_x_continuous(breaks = seq(50, 100, by = 2)) +
  theme_clean()

# Facet wrap probability of hiring f(selectivity)

## Create expanded hiring list----
hired_list_expanded <- p_hiring_best %>%
  add_column(selectivity = 100)

p_hiring_ninetyfive <- hire_better_than(99) %>% 
  add_column(selectivity = 99)
p_hiring_ninety <- hire_better_than(98) %>% 
  add_column(selectivity = 98)
p_hiring_eightyfive <- hire_better_than(97) %>% 
  add_column(selectivity = 97)

hired_list_expanded <-  as_tibble(rbind(hired_list_expanded, p_hiring_ninetyfive,
                              p_hiring_ninety,
                              p_hiring_eightyfive))

## Plot results----
ggplot(hired_list_expanded) +
  geom_line(aes(x = strategy, y = p), color = "blue") +
  facet_wrap(~selectivity) +
  theme_clean()




hired_list_expanded <- hired_list_expanded %>% # Obvious problems with this
  for (i in 95:100) {
    hire_better_than(i) %>%
      add_column(selectivity = 99)
  }
