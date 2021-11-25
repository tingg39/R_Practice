library(fpp3)

y<-tsibble(Year =2015:2019, Observation =  c(123, 39,78,52,110), index = Year)
View(y)




# check olympic running
View(olympic_running)

head(olympic_running,5)

or_df<-olympic_running %>% distinct(Sex) %>% as.data.frame()
View(or_df)

# visualization 
# ansett airline

melsyd_economy <- ansett %>%
  filter(Airports=='MEL-SYD',Class =='Economy')

melsyd_economy %>%
  autoplot(Passengers)%>%
  labs(title = "Ansett economy class passengers",
       subtitle = "Melbourne -Sydney")

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6) -> a10
View(PBS)

# ansett
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

View(ansett)

# The time plot immediately reveals some interesting features.
# 
# There was a period in 1989 when no passengers were carried — this was due to an industrial dispute.
# There was a period of reduced load in 1992. This was due to a trial in which some economy class seats were replaced by business class seats.
# A large increase in passenger load occurred in the second half of 1991.
# There are some large dips in load around the start of each year. These are due to holiday effects.
# There is a long-term fluctuation in the level of the series which increases during 1987, decreases in 1989, and increases again through 1990 and 1991.
# There are some periods of missing observations.


a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales") +
  expand_limits(x = ymd(c("1972-12-28", "1973-12-04")))


holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")


holidays %>%
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")

vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )
View(tourism)

visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

View(visitors)

visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")

visitors %>%
  pivot_wider(values_from=Trips, names_from=State) %>%
  GGally::ggpairs(columns = 2:9)

#ACF

a10 %>%
  ACF(Cost, lag_max = 48) %>%
  autoplot() +
  labs(title="Australian antidiabetic drug sales")

# white noise
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise", y = "")

y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "White noise")

recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)

recent_production %>% ACF(Beer, lag_max = 9)

recent_production %>%
  ACF(Beer) %>%
  autoplot() + labs(title="Australian beer production")

