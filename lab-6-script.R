
# load packages -----------------------------------------------------------

library(tidyverse)

# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")
crabs <- read_csv("chap15q27FiddlerCrabFans.csv")


# data to tidy format -----------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

# do stuff ----------------------------------------------------------------

ttest_results <- t.test(formula = species ~ location, data = fish_long)

ttest_results

diff(ttest_results$estimate)

# Graphs ------------------------------------------------------------------

fish_summary <-
  fish_long %>%
  group_by(location) %>%
  summarize(
    sample_size = n(),
    mean = mean(species),
    str_dev = sd(species),
    var = var(species),
    sem = sd(species)/sqrt(n()),
    ci_upper = mean + 1.96 * sem,
    ci_lower = mean - 1.96 * sem,
  )

fish_summary

fish_long %>% 
  ggplot(aes(x = location, y = species)) + 
  geom_jitter(aes(color = location), 
              shape = 16, alpha = 0.3, width = 0.4) + 
  geom_errorbar(aes(y = mean, ymax = ci_upper, ymin = ci_lower), 
                data = fish_summary, 
                width = 0.1) + 
  geom_point(aes(y = mean), 
             data = fish_summary)+ 
  scale_color_manual(values = c("purple", "darkblue")) + 
  theme_minimal() + 
  guides(color = "none")

# Histogram ---------------------------------------------------------------

fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity",
    na.rm = TRUE
  ) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  theme_minimal()
  guides(color = "none")

# crab data ---------------------------------------------------------------

crabs %>%
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType),
    bins = 12,
    alpa = 0.5,
    position = "identity",
    na.rm = TRUE
  ) +
  theme_minimal()

# ANOVA -------------------------------------------------------------------

aov_crab_temp <-
  aov(bodyTemperature ~ crabType, data = crabs)

aov_crab_temp
summary(aov_crab_temp)





  










