# created: 3/19/2020
# updated:
#
# author: gina
#
# purpose: look at mustard seed penetrometer data


library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(glue)

draw <- read_excel("data/raw-fixed/20200319_penetrometer.xlsx", skip = 4) %>% 
  clean_names() %>% 
  filter(!is.na(field))



d <- draw %>% 
  mutate(date = ymd("2020-03-16"),
       year = year(date),
       doy = yday(date)) %>%
  mutate(samp_id = parse_number(number),
         samp_id = paste(field, samp_id, sep = "_")) %>%
  #--gather depths into one col, make numeric
  gather(x0_0_cm:x45_0_cm, key = "depth_cm", value = "resis_kpa") %>%
  mutate(depth_cm = str_sub(depth_cm, start = 2, end = -4),
         depth_cm = str_replace(depth_cm, "_", "."),
         depth_cm = as.numeric(depth_cm)) %>% 
  #--weird depths
  filter(depth_cm <35) %>% 
  #--replace 0s with NAs
  mutate(resis_kpa = ifelse(resis_kpa == 0, NA, resis_kpa))


# visualize ---------------------------------------------------------------

# Flat, map style
d %>% 
  filter(depth_cm <=10) %>% 
  group_by(field, row, column) %>% 
  summarise(resis_kpa = mean(resis_kpa)) %>% 
  ggplot(aes(column, row)) + 
  geom_tile(aes(fill = resis_kpa)) + 
  scale_y_reverse() +
  facet_wrap(~field, scales = "free") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Top Is North") + 
  theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank())

#--individual fields
d %>% 
  ggplot(aes(depth_cm, resis_kpa, group = samp_id)) + 
  geom_line() + 
  facet_grid(.~field) + 
  scale_x_reverse() +
  coord_flip()


# all together, compared
d %>% 
  group_by(field, depth_cm) %>% 
  summarise(resis_kpa = mean(resis_kpa, na.rm = T)) %>% 
  ggplot(aes(depth_cm, resis_kpa)) + 
  geom_line(aes(color = field), size = 4) + 
  scale_x_reverse() +
  coord_flip()
