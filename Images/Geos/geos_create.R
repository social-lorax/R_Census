library(tidyverse)
library(tidycensus)
library(sf)


key <- read_csv("C:/Users/brenner/Documents/GitHub/R_Census/Data/state_codes.csv")

states <- tigris::states(cb = TRUE) %>%
  filter(as.double(GEOID) %in% c(1:2, 4:6, 8:13, 15:42, 44:51, 53:56))

ny <- states %>% filter(GEOID == 36)


places <- get_acs(
  geography = "cbsa",
  #state = "36",
  variables = c(Population = "B01003_001")
)

places %>% 
  filter(str_detect(NAME, " Metro Area$")) %>% 
  transmute(
    `Metro Area` = str_remove_all(NAME, " Metro Area$"),
    FIPS = GEOID,
    Population = estimate
  ) %>%
  arrange(desc(Population)) %>% 
  head(50) %>% 
  write_csv("C:/Users/brenner/Documents/GitHub/R_Census/Data/metro_codes.csv")
  
  
  
  filter(str_detect(NAME, "city, New York")) %>% 
  transmute(
    City = str_remove_all(NAME, " city, New York"),
    FIPS = GEOID,
    Population = estimate
  ) %>% 
  arrange(desc(Population)) %>% write_csv("C:/Users/brenner/Documents/GitHub/R_Census/Data/place_codes.csv")



states %>% 
  filter(!as.double(GEOID) %in% c(2, 15)) %>% 
  tidylog::left_join(key, by = c("NAME" = "State")) %>%
  mutate(
    Division = str_remove_all(Division, "\xa0"),
    Division = factor(Division, levels = c(
      "East North Central",
      "West North Central",
      "New England",
      "Middle Atlantic",
      "East South Central",
      "West South Central",
      "South Atlantic",
      "Pacific",
      "Mountain"
    ))
  ) %>% 
  ggplot(aes(fill = Division)) + 
  geom_sf(color = "gray") + 
  scale_fill_manual(values = c(
    "East North Central" = "#E5BC4A",
    "West North Central" = "#8A6400",
    "New England" = "#63A487",
    "Middle Atlantic" = "#194131",
    "East South Central" = "#A569A8",
    "West South Central" = "#854F87",
    "South Atlantic" = "#4E244F",
    "Pacific" = "#5E86B0",
    "Mountain" = "#12314C"
    )
  ) + 
  theme_void() +
  theme(legend.position = "none")


states %>% 
  filter(as.double(GEOID) == 2) %>% 
  ggplot() + 
  geom_sf(fill = "#5E86B0") + 
  theme_void()

states %>% 
  filter(as.double(GEOID) == 15) %>% 
  ggplot() + 
  geom_sf(fill = "#5E86B0") + 
  theme_void()

counties <- tigris::counties(cb = TRUE, state = 36)
tracts <- tigris::tracts(cb = TRUE, state = 36, county = "New York")

tracts %>% 
  mutate(fill = case_when(
    GEOID == "36061006500" ~ "A",
    .default = "C")
  ) %>% 
  ggplot() + 
  geom_sf(aes(fill = fill)) + 
  scale_fill_manual(values = c("dodgerblue", "gray80")) + 
  theme_void() + 
  theme(legend.position = "none")

tigris::blocks(state = "36", county = "061") %>% 
  filter(str_detect(GEOID20, "^36061006500")) %>% 
  mutate(fill = case_when(
    GEOID20 == "360610065001006" ~ "A",
    .default = "C")
  ) %>% 
  ggplot() + 
  geom_sf(aes(fill = fill)) + 
  scale_fill_manual(values = c("dodgerblue", "gray80")) + 
  theme_void() + 
  theme(legend.position = "none")
