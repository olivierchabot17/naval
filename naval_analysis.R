# Load packages
library(tidyverse)
library(lubridate)

# Set the working directory
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)

# Load the data
sample <- read_csv(file = "180_days_naval.csv")

# Make sure each column is in the right format.
sample <- sample %>%
  transmute(
    date = ymd(date),
    mood_level = as.numeric(mood_level),
    period = factor(period, levels = c("Before", "During", "After"))
  )

# Peek at the data 
head(sample)
str(sample)

# Calculate group means and standard deviations
sample_stats <- sample %>% 
  group_by(period) %>%
  summarise(
    average_mood = mean(mood_level),
    sd_mood = sd(mood_level)
  )

# Visualize the group differences
ggplot(
  data = sample_stats,
  aes(x = period, y = average_mood)
) +
  geom_pointrange(aes(ymin = average_mood - sd_mood, ymax = average_mood + sd_mood), show.legend = FALSE) +
  geom_hline(yintercept = mean(sample$mood_level), linetype = "dashed", alpha = 0.2) +
  theme_classic() +
  #ylim(1, 5) +
  labs(
    title = "Effect on Mood of Naval Ravikant's Meditation Challenge",
    x = "Sixty-day block relative to the treatment",
    y = "Mood rating (1 - 5)",
    caption = "Data collected using the Daylio phone application."
  )


# Difference between group means
mood_model <- aov(mood_level ~ period, data = sample)
summary(mood_model)

# Pairwise t-test without corrections for multiple testing
pairwise.t.test(sample$mood_level, sample$period, p.adj = "none")
# Pairwise t-test with Bonferroni correction for multiple testing
pairwise.t.test(sample$mood_level, sample$period, p.adj = "bonf")
# Tukey's HSD pairwise comparisons
TukeyHSD(mood_model)

# Compare group variances using Bartlett's test
bartlett.test(mood_level ~ period, data = sample)

# Not enough evidence to conclude that the means or variances differ 
# at 5% significance level





