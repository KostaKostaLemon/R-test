---
title: "002.1Quarto_ExpDesign"
format: html
editor: visual
---

# Load Packages

```{r}
library(ggplot2)# plotting
library(dplyr)# data management and summary statistics
library(ggpubr)# plotting
library(agricolae)# experimental design
library(openxlsx)# import and export Excel files
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
```

# Generate Plot - Statistical Power depends on replications, SD and Treatmenteffcts

```{r}
sample_sizes=c(2,3,5,10,20) # Different sample sizes to test
treatment_effects <- c(5)  # Effect size to test
std_devs <- c(5)         # Standard deviations to test

# Generate one example dataset per combination for visualization
example_data <- do.call(rbind, lapply(sample_sizes, function(n) {
  do.call(rbind, lapply(std_devs, function(sd) {
    do.call(rbind, lapply(treatment_effects, function(effect) {
      
      # Generate data for control and treatment groups
      control <- rnorm(n, mean = 20, sd = sd)
      treatment <- rnorm(n, mean = 20 + effect, sd = sd)
      
      # Combine control and treatment data into a single data frame
      data.frame(
        group = rep(c("Control", "Treatment"), each = n),
        value = c(control, treatment),
        sample_size = n,
        sd = sd,
        effect = effect
      )
    }))
  }))
}))

# Plot the example data for each combination
ggplot(example_data, aes(x = group, y = value, fill = group)) +
 geom_point(shape=3, size=1)+
  stat_summary(color = "#00BA38", fun = mean, geom = "point",  size = 2, position = position_nudge(x =0.2)) +
 stat_compare_means(method = "t.test", label.y.npc  =0.9) +
  facet_grid(sd ~ sample_size, labeller = labeller(
    sd = function(x) paste("SD =", x),
    sample_size = function(x) paste("Sample size =", x)
  )) +
  theme_bw() +
  theme(legend.position = "none")
```

# Powercurve for different n, SD, Treatmenteffects

```{r}
sample_sizes <- c(2,3,5,10,20, 100)    # Different sample sizes to test
alpha <- 0.05                          # Significance level for the t-test
n_simulations <- 1000                  # Number of simulations for each combination
treatment_effects <- c(0.2,2, 5, 10, 100)  # Different effect sizes to test
std_devs <- c(5, 10, 15) 

# Different standard deviations to test

# Function to calculate power through simulation
simulate_power <- function(n, sd, effect) {
  significant_results <- 0
  for (i in 1:n_simulations) {
    # Generate data for control and treatment groups with specified mean and SD
    control <- rnorm(n, mean = 20, sd = sd)
    treatment <- rnorm(n, mean = 20 + effect, sd = sd)
    
    # Perform a t-test comparing the two groups
    test <- t.test(treatment, control)#, alternative = c("greater")
    
    # Check if p-value is below alpha, indicating a significant result
    if (test$p.value < alpha) {
      significant_results <- significant_results + 1
    }
  }
  # Calculate power as the proportion of significant results out of total simulations
  return(significant_results / n_simulations)
}

# Run the simulation for each combination of sample size, standard deviation, and effect size
results <- expand.grid(sample_size = sample_sizes, sd = std_devs, effect = treatment_effects)
results$power <- mapply(simulate_power, results$sample_size, results$sd, results$effect)

# Plot the power curves
ggplot(results, aes(x = sample_size, y = power*100, color = as.factor(effect))) +
   geom_hline(yintercept=80, col="grey", linetype = "dashed")+
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_grid(sd ~ effect, labeller = labeller(
    sd = function(x) paste("SD =", x),
    effect = function(x) paste("Effect size =", x)
  )) +
  labs(title = "Power curve for different sample sizes, effect sizes, and standard deviations",
       x = "Sample size",
       y = "Power",
       color = "Effect size") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
  scale_x_sqrt(breaks=sample_sizes, labels=sample_sizes)
```
