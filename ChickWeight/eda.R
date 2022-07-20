# EDA for ChickWeight data set
# Andrew Zalesak July 20th, 2022

cw <- ChickWeight

# How many chicks are there?
length(unique(cw$Chick)) # 50

# What ages do they live to?
max_ages <- cw %>%
  group_by(Chick) %>%
  summarize(max(Time)) %>%
  arrange(`max(Time)`)
# All but 5 live to age 21

# List of chicks that lived to 21
chicklist <- max_ages$Chick[max_ages$`max(Time)` == 21]

# What is the distribution of weights at age 21?
cw %>%
  filter(Time == 21) %>%
  summarize(
    count = n(),
    mean_weight = mean(weight),
    sd_weight = sd(weight)
  )
# count = 45, mean = 218.7, sd = 71.5

cw %>%
  filter(Time == 21) %>%
  ggplot(mapping = aes(x=weight)) +
  geom_histogram(bins = 8)
# Looks roughly normal?

# Age vs. weight relationship for chicks that lived to 21?
cw %>%
  filter(Chick %in% chicklist) %>%
  ggplot(mapping = aes(x=Time, y=weight, color=Diet))+
  geom_point()

# Oops... the variance in weight increases with Time...
