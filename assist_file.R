test <- pathogen %>%
  select(unique_id, country, classification, pathogen_clean, assay, number, samples_tested) %>%
  filter(!grepl("tested", assay)) %>%
  select(-assay) %>%
  group_by(country, classification, pathogen_clean) %>%
  summarise(`Number of studies` = n_distinct(unique_id),
            `Number positive` = sum(number),
            `Number tested` = sum(samples_tested))



if(input$selectedspecies == "All" & input$selectedpathogen == "All") {
  pathogen
} if(input$selectedspecies == "All") {
  pathogen %>%
    filter(pathogen_clean %in% input$selectedpathogen)
} if(input$selectedpathogen == "All") {
  pathogen %>%
    filter(classification %in% input$selectedspecies)
} else {
  pathogen %>%
    filter(pathogen_clean %in% input$selectedpathogen & classification %in% input$selectedspecies)
}

wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")

level_0 <- read_rds(here("data", "level_0_admin.rds"))

level_2 <- read_rds(here("data", "level_2_admin.rds"))
list2env(level_2, envir = .GlobalEnv)
level_2_all <- do.call(rbind.SpatialPolygonsDataFrame, level_2) %>%
  st_as_sf()

studies <- read_rds(here("data_clean", "studies.rds"))

rodent_spatial <- read_rds(here("data", "rodent_spatial.rds"))

sites_2 <- st_intersection(x = level_2_all, y = rodent_spatial)
n_sites_region <- sites_2 %>%
  group_by(NAME_2) %>%
  summarise(n = n(),
            Studies = toString(unique(unique_id))) %>%
  tibble()

level_2_sites <- level_2_all %>%
  left_join(., n_sites_region %>%
              dplyr::select(-geometry),
            by = "NAME_2") %>%
  mutate(area_m2 = st_area(.),
         site_density = n/(as.numeric(area_m2)/1000000),
         site_density = ifelse(is.na(site_density), NA, site_density))

tm_shape(level_0 %>%
           filter(GID_0 %in% wa_countries)) +
  tm_polygons(alpha = 0, lwd = 1) +
  tm_shape(level_2_sites) +
  tm_polygons(col = "site_density", style = "fixed", breaks = c(0, 0.001, 0.005, 0.01, 0.05, 1, 6),
              palette = "-viridis", colorNA = NULL, border.alpha = 1, border.col = "grey", lwd = 0.1,
              title = paste("Density of trap sites per 1000 km2"),
              id = "NAME_2",
              popup.vars = c("Site density:" = "site_density",
                             "Studies trapping here:" = "Studies"))
