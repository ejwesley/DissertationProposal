library(tidyverse)
library(brms)
library(here)

summer2010 <- read_csv(here("data", "Summer2010.csv"),
                       col_types = cols(GEOID10 = col_factor(),
                                        Date = col_date(format = ""),
                                        n = col_integer(),
                                        severity = col_factor(),
                                        median = col_double()))


# Pediatric asthma rate per tract -----------------------------------------

# Males and females 17 and under

age.table <- read_csv(here("data", "Decennial2010", "Age2010.csv")) %>%
  select(GEO_ID, ends_with("2003"):ends_with("2006"), ends_with("2027"):ends_with("2030")) %>%
  filter(row_number() != 1) %>%
  mutate_at(vars(starts_with("P")), as.numeric) %>%
  pivot_longer(cols = 2:9, names_to = "bracket") %>%
  group_by(GEO_ID) %>%
  summarise(n.kids = sum(value)) %>%
  filter(GEO_ID != "0100000US") %>%
  mutate(GEO_ID = str_replace(GEO_ID, "1400000US", ""))

# Calculate daily rates per tract, all severity levels

all.levels <- summer2010 %>%
  group_by(GEOID10, Date) %>%
  summarise(count = sum(n),
            LST = first(median)) %>%
  left_join(age.table, by = c("GEOID10" = "GEO_ID")) %>%
  mutate(rate = count / n.kids,
         GEOID10 = as.factor(GEOID10)) %>%
  ungroup()


# Descriptive plots -------------------------------------------------------


# Plot histogram of rates without zeros
all.levels %>%
  filter(rate > 0) %>%
ggplot() +
  geom_dotplot(aes(rate), binwidth = 0.00025)

# Scatterplot of rate vs LST without zeros
all.levels %>%
  filter(rate > 0) %>%
ggplot() +
  geom_point(aes(LST, rate)) +
  geom_smooth(aes(LST, rate), method = "lm") +
  ylim(0, 0.012)

# Cases by date
ggplot(all.levels) +
  geom_point(aes(Date, count)) +
  facet_wrap(~ GEOID10)

# Cases per tract for entire summer
ggplot(all.levels) +
  geom_bar(aes(count, color = GEOID10))

all.levels %>%

  ggplot() +
    geom_bar(aes(count, fill = GEOID10), position = "dodge")


# Descriptive statistics --------------------------------------------------

daily.n <- all.levels %>%
  group_by(Date) %>%
  summarize(n = sum(count))

ggplot() +
  geom_histogram(data = daily.n, aes(n), bins = 13, binwidth = 0.5)

v <- var(daily.n$n)
m <- mean(daily.n$n)

# Variance is greater than the mean indicating over dispersion

daily.geog.n <- all.levels %>%
  group_by(Date, GEOID10) %>%
  summarize(n = sum(count)) %>%
  ungroup() %>%
  group_by(GEOID10) %>%
  summarise(v = var(n),
           m = mean(n),
           over = v > m)

1 - (sum(daily.geog.n$over) / nrow(daily.geog.n))
# 58% of the census tracts are overdispersed

# Bayesian model ----------------------------------------------------------



fit <- brm(n ~ (median | GEOID10), data = summer2010, family = poisson())
