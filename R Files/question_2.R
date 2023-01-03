library(tidyverse)
library(sf)
library(shiny)
library(spData)
library(scales)
library(cowplot)

path <- "https://raw.githubusercontent.com/cuadrosangeles/data/main"
file <- "C:/Users/cuadr/OneDrive/Escritorio/UCHICAGO/4. Harris classes/4th quarter/Data and Programming II/Problem sets/PS2/homework-2-cuadrosangeles"
chicago <- st_read(file.path(file, "geo_export_72722924-731b-4948-9dbc-14dd79cca03f.shp"))
dataset_chicago <- read_csv(file.path(path, "dataset_chicago.csv"))
covid <- read_csv(file.path(path, "Covid.csv"))
vulnerability <- read_csv(file.path(path, "Vulnerability.csv"))


# Covid map

new_covid <- covid %>%
  group_by(`ZIP Code`) %>%
  summarize(sum_cases = sum(`Deaths - Weekly`, na.rm = TRUE))


chicago_covid <- new_covid %>% 
  left_join(chicago, by = c("ZIP Code" = "zip"))


chicago_covd_st <- st_sf(chicago_covid)


covid_map <- ggplot() +
  geom_sf(data = chicago_covd_st, 
          aes(fill = sum_cases), 
          color = alpha("white",0.5)) +
  labs(title = "Covid-19 releated deaths in Chicago, 2020-2022",
       fill = "Cases",
       caption = "Source: Chicago Data Portal",
       x = "",
       y = "") +
  scale_fill_viridis_c(option = "mako", begin = 0.1, direction = -1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  geom_sf_text(data = chicago_covd_st,
               aes(label = `ZIP Code`),
               size = 1,
               color = "white",
               check_overlap = TRUE
  )


# Vulnerability map

vulnerability$`Community Area or ZIP Code` <- as.character(vulnerability$`Community Area or ZIP Code`)


vulnerable_chicago <- vulnerability %>% 
  left_join(chicago, by = c("Community Area or ZIP Code" = "zip"))


vulnerable_chicago_st <- st_sf(vulnerable_chicago)


vulnerability_map <- ggplot() +
  geom_sf(data = vulnerable_chicago_st, 
          aes(fill = `CCVI Score`), 
          color = alpha("white", 0.5)) +
  labs(title = "Chicago COVID-19 Community Vulnerability Index",
       fill = "Score",
       caption = "Source: Chicago Data Portal",
       x = "",
       y = "") +
  scale_fill_viridis_c(option = "rocket", begin = 0.1, direction = -1) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_sf_text(data = vulnerable_chicago_st,
    aes(label = `Community Area or ZIP Code`),
    size = 1,
    color = "white",
    check_overlap = TRUE
  )


# Comment: Both choropleths try to understand the relationship between Covid-19 deaths and the Community Vulnerability Index. Given both maps, the correlation between those variables is high. The east and south Chicago areas show critical indicators on both of them.

save_plot("Figure_1_covid_map.png", covid_map)
save_plot("Figure_2_vulnerability_map.png", vulnerability_map)


# Saving the merged dataset as a csv file

chicago_final <- vulnerable_chicago %>% 
  left_join(chicago_covid, by = c("Community Area or ZIP Code" = "ZIP Code")) %>%
  rename(ZIP = `Community Area or ZIP Code`,
         Score = `CCVI Score`,
         Category = `CCVI Category`,
         Cases = `sum_cases`) %>%
  select(ZIP, Score, Category, Cases) %>%
  arrange(ZIP) %>%
  distinct(ZIP, .keep_all = TRUE)

# write.csv(chicago_final,"C:/Users/cuadr/OneDrive/Escritorio/UCHICAGO/4. Harris classes/4th quarter/Data and Programming II/Problem sets/PS2/homework-2-cuadrosangeles/dataset_chicago.csv", row.names = FALSE)


# Creating a map with streets

new_covid <- covid %>%
  group_by(`ZIP Code`) %>%
  summarize(sum_cases = sum(`Deaths - Weekly`, na.rm = TRUE))


chicago_covid <- new_covid %>% 
  left_join(chicago, by = c("ZIP Code" = "zip"))


chicago_covd_st <- st_sf(chicago_covid)


streets <- st_read(file.path(path, "Major_Streets.shp"))


covid_streets <- ggplot() +
  geom_sf(data = chicago_covd_st, 
          aes(fill = sum_cases), 
          color = alpha("white",0.5)) +
  labs(title = "Covid-19 releated deaths in Chicago (with streets), 2020-2022",
       fill = "Cases",
       caption = "Source: Chicago Data Portal",
       x = "",
       y = "") +
  scale_fill_viridis_c(option = "mako", begin = 0.1, direction = -1) +
  geom_sf(data = streets, color = "yellow") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  geom_sf_text(data = chicago_covd_st,
               aes(label = `ZIP Code`),
               size = 1,
               color = "white",
               check_overlap = TRUE
  )

save_plot("covid map streets.png", covid_streets)

