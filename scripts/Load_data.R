# ---- data ---------------------------------------------------------------

# summer2010 <- read_csv(here("data", "Summer2010.csv"),
#                        col_types = cols(GEOID10 = col_factor(),
#                                         Date = col_date(format = ""),
#                                         n = col_integer(),
#                                         severity = col_factor(),
#                                         median = col_double()))

# Read in asthma data

tract.obs <- read_csv(here("data", "TractObs.csv"))

# Get tract list

tracts <- tract.obs %>%
  select(GEOID10) %>%
  distinct()

write_csv(tracts, here("data", "Decennial2010", "tract_list.csv"))



# Pediatric asthma rate per tract

# Males and females 17 and under
# Table from Census.gov

age.table <- read_csv(here("data", "Decennial2010", "Age2010.csv")) %>%
  select(GEO_ID, ends_with("2003"):ends_with("2006"), ends_with("2027"):ends_with("2030")) %>%
  filter(row_number() != 1) %>%
  mutate_at(vars(starts_with("P")), as.numeric) %>%
  pivot_longer(cols = 2:9, names_to = "bracket") %>%
  group_by(GEO_ID) %>%
  summarise(n.kids = sum(value)) %>%
  filter(GEO_ID != "0100000US") %>%
  mutate(GEO_ID = str_replace(GEO_ID, "1400000US", ""))

# Race - percent minority (non-white)

# Percent total population, white alone
# Table from Social Explorer
# PCT_SE_T054_002

race.table <- read_csv(here("data", "Decennial2010", "Race2010.csv"), skip = 1) %>%
  mutate(GEOID10 = as.factor(Geo_FIPS),
         pct.maj = PCT_SE_T054_002 * 0.01,
         pct.min = 1 - pct.maj) %>%
  select(GEOID10, pct.min)


# Calculate daily rates per tract, all severity levels

all.levels <- summer2010 %>%
  group_by(GEOID10, Date) %>%
  summarise(count = sum(n),
            LST = first(median)) %>%
  left_join(age.table, by = c("GEOID10" = "GEO_ID")) %>%
  left_join(race.table) %>%
  mutate(asthma.rate = count / n.kids,
         GEOID10 = as.factor(GEOID10)) %>%
  ungroup()

zero.tracts <-
  all.levels %>%
  group_by(GEOID10) %>%
  summarise(n = sum(count)) %>%
  filter(n == 0)

# Remove tracts where there are no cases on any date over the entire period

all.levels <-
  all.levels %>%
  filter((GEOID10 %in% zero.tracts$GEOID10) == F)

# Add mean impervious per tract from NLCD 2011, from Google Earth Engine

imp <- read_csv(here("data", "Mean_impervious.csv")) %>%
  select(GEOID10 = geoid10, mean.imp = mean) %>%
  mutate(GEOID10 = as.factor(GEOID10))

all.levels <- all.levels %>%
  left_join(imp)
