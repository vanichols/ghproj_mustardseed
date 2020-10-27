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
d_out %>% 
  filter(depth_cm <=10) %>% 
  group_by(field, row, column) %>% 
  summarise(resis_kpa = mean(resis_kpa)) %>% 
  ggplot(aes(column, row)) + 
  geom_raster(aes(fill = resis_kpa), interpolate = TRUE) + 
  scale_y_reverse() +
  facet_wrap(~field, scales = "free") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Top Is North") + 
  theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank())

#--individual fields

library(LaCroixColoR)
d_out %>% 
  ggplot(aes(depth_cm, resis_kpa, group = samp_id)) + 
  geom_line(aes(color = samp_id)) + 
  facet_grid(.~field) + 
  scale_x_reverse() +
  scale_color_manual(values = lacroix_palette("Pamplemousse", n = 59,type = "continuous")) +
  coord_flip()

#--assess outliers
pout <- d %>% 
  ggplot(aes(depth_cm, resis_kpa, group = samp_id)) + 
  geom_line(aes(color = samp_id)) + 
  facet_grid(.~field) + 
  scale_x_reverse() +
  facet_wrap(~field) +
  coord_flip()

ggplotly(pout)

d_out <- d %>% 
  #--completely get rid of
  filter(!samp_id %in% c("prairie-trees_1", "OG_5")) %>% 
#--just certain 'levels'
filter(!(samp_id == "leafbag_7" & depth > 30))
filter(!(samp_id == "broccoli_16" & depth < 10))
filter(!(samp_id == "broccoli_14" & depth > 15))

d_out %>% write_csv("data/td_resis-out.csv")
#--write it for shiny app
d_out %>% write_csv("MustardSeedPenetrometer/data-resis.csv")

# Flat, map style
d_out %>% 
  filter(depth_cm <=10) %>% 
  group_by(field, row, column) %>% 
  summarise(resis_kpa = mean(resis_kpa)) %>% 
  ggplot(aes(column, row)) + 
  geom_raster(aes(fill = resis_kpa), interpolate = TRUE) + 
  scale_y_reverse() +
  facet_wrap(~field, scales = "free") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(subtitle = "Top Is North",
       title = "0-10 cm Penetration Resistance") + 
  theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank())


# Flat, map style
d_out %>% 
  filter(depth_cm > 10, depth_cm <20) %>% 
  group_by(field, row, column) %>% 
  summarise(resis_kpa = mean(resis_kpa)) %>% 
  ggplot(aes(column, row)) + 
  geom_raster(aes(fill = resis_kpa), interpolate = TRUE) + 
  scale_y_reverse() +
  facet_wrap(~field, scales = "free") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Top Is North") + 
  theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank())





library(plotly)
# all together, compared
p1 <- d_out %>% 
  group_by(field, depth_cm) %>% 
  mutate(mresis_kpa = mean(resis_kpa, na.rm = T)) %>% 
  ggplot(aes(color = field)) + 
  geom_line(aes(depth_cm, resis_kpa, group = samp_id)) +
  geom_line(aes(depth_cm, mresis_kpa), size = 4) + 
  scale_x_reverse() +
  coord_flip()

ggplotly(p1)
