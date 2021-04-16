library(tidyverse)

nobel <- read_csv("nobel.csv")
head(nobel)

nobel %>% 
  filter( year >=1901 & year <= 2016) %>%
  count()

nobel %>%
  count(sex)

nobel %>%
  count(birth_country)
prop_usa_winners <- nobel %>%
  mutate(usa_born_winner = (birth_country == "United States of America"),
         decade = (year - year%%10)) %>%
  group_by(decade) %>%
  summarise(proportion = mean(usa_born_winner, na.rm = TRUE))

prop_usa_winners

options(repr.plot.width=7, repr.plot.height=4)


ggplot(prop_usa_winners, aes(x=decade, y=proportion))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = scales::percent,
                     limits = 0:1, expand = c(0,0))
prop_female_winners <- nobel %>%
  mutate(female_winner = (sex == "Female"),
         decade = (year - year%%10)) %>%
  group_by(decade, category)%>%
  summarise(proportion = mean(female_winner, na.rm = TRUE))



ggplot(prop_female_winners, aes(x=decade, y=proportion, color=category))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = scales::percent,
                     limits = 0:1, expand = c(0,0))
library(lubridate)


nobel_age <- nobel %>%
  mutate(age = year - year(birth_date))


ggplot(nobel_age, aes(x = year, y = age))+
  geom_point()+
  geom_smooth(se = FALSE)