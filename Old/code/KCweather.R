dat <-  tribble(
  ~DATE, ~Var, ~Value,
  "2019-10-20", "TMAX", 65,
  "2019-10-20", "TMIN", 42,
  "2019-10-21", "TMAX", 57,
  "2019-10-21", "TMIN", 44,
  "2019-10-22", "TMAX", 58,
  "2019-10-22", "TMIN", 41,
  "2019-10-23", "TMAX", 67,
  "2019-10-23", "TMIN", 45,
  "2019-10-24", "TMAX", 46,
  "2019-10-24", "TMIN", 38,
  "2019-10-25", "TMAX", 54,
  "2019-10-25", "TMIN", 36
) %>% mutate(DATE = as_date(DATE),
             doy = yday(DATE),
             Value = (Value - 32) * (5/9))

atmo2 <- atmo %>% 
  bind_rows(dat) %>% 
  group_by(doy, Var) %>% 
  mutate(avg = mean(Value, na.rm = T))  

atmo2 %>% filter(
                 year(DATE) == 2019,
                 Var != "PRCP")  %>% 
  ggplot() +
  geom_line(aes(DATE, Value, color = Var), size = 1.5) +
  geom_line(aes(DATE, avg, group = Var))
