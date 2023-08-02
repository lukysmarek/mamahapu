library(tidyverse)
library(sf)
library(sfdep)
library(PrettyCols)
library(tmap)
library(sfdep)
library(rcartocolor)
library(patchwork)
library(ggpattern)

## just load mamahapu.RData to get started
## it loads all the raw data needed 
load("mamahapu.RData")

#### replace NA's and format the data #####
influenza_yr <- influenza_yr %>% 
  mutate(across(c(count,popn), ~replace_na(., 0)),
         across(c(rate, lower_95CI, upper_95CI), ~ifelse(is.na(.) & popn > 0, 0, .))) %>% 
  mutate(area = if_else(area == "Wanganui District", "Whanganui District", area),
         rate_CI = ifelse(!is.na(rate), paste0(sprintf(rate, fmt = '%#.1f')," (", sprintf(lower_95CI, fmt = '%#.1f'),"; ",sprintf(upper_95CI, fmt = '%#.1f'),")"), NA))

influenza_yr_eth <- influenza_yr_eth %>% 
  filter(eth != "Missing")  %>% 
  complete(area, eth, imms, year) %>% 
  mutate(across(c(count,popn), ~replace_na(., 0)),
         across(c(rate, lower_95CI, upper_95CI), ~ifelse(is.na(.) & popn > 0, 0, .))) %>% 
  mutate(area = if_else(area == "Wanganui District", "Whanganui District", area),
         eth = case_when(eth == "Pacific Peoples" ~ "Pasifika",
                         TRUE ~ eth),
         rate_CI = ifelse(!is.na(rate), paste0(sprintf(rate, fmt = '%#.1f')," (", sprintf(lower_95CI, fmt = '%#.1f'),"; ",sprintf(upper_95CI, fmt = '%#.1f'),")"), NA)) %>% 
  pivot_wider(id_cols = c(year, area, imms), names_from = eth, values_from = c(count, popn, rate, rate_CI))

influenza <- influenza_yr %>% 
  left_join(influenza_yr_eth, by = c("year", "area", "imms")) %>% 
  as_tibble()

pertusis_yr <- pertusis_yr %>% 
  mutate(across(c(count,popn), ~replace_na(., 0)),
         across(c(rate, lower_95CI, upper_95CI), ~ifelse(is.na(.) & popn > 0, 0, .))) %>% 
  mutate(area = if_else(area == "Wanganui District", "Whanganui District", area),
         rate_CI = ifelse(!is.na(rate), paste0(sprintf(rate, fmt = '%#.1f')," (", sprintf(lower_95CI, fmt = '%#.1f'),"; ",sprintf(upper_95CI, fmt = '%#.1f'),")"), NA))

pertusis_yr_eth <- pertusis_yr_eth %>% 
  filter(eth != "Missing")  %>% 
  complete(area, eth, imms, year) %>% 
  mutate(across(c(count,popn), ~replace_na(., 0)),
         across(c(rate, lower_95CI, upper_95CI), ~ifelse(is.na(.) & popn > 0, 0, .))) %>% 
  mutate(area = if_else(area == "Wanganui District", "Whanganui District", area),
         eth = case_when(eth == "Pacific Peoples" ~ "Pasifika",
                         TRUE ~ eth),
         rate_CI = ifelse(!is.na(rate), paste0(sprintf(rate, fmt = '%#.1f')," (", sprintf(lower_95CI, fmt = '%#.1f'),"; ",sprintf(upper_95CI, fmt = '%#.1f'),")"), NA)) %>% 
  pivot_wider(id_cols = c(year, area, imms), names_from = eth, values_from = c(count, popn, rate, rate_CI))

pertusis <- pertusis_yr %>% 
  left_join(pertusis_yr_eth, by = c("year", "area", "imms")) %>% 
  as_tibble()

imms <- influenza %>% 
  bind_rows(pertusis) %>% 
  mutate(imms = ifelse(imms == "influenza", "Influenza", "Pertussis")) %>% 
  rename(count_total = count, popn_total = popn, rate_total = rate, lower_95CI_total = lower_95CI, upper_95CI_total = upper_95CI, rate_CI_total = rate_CI)

# data for shiny app
imms.sf <- ta %>% 
  select(code = TA2020_V1_, name = TA2020_V_1, area = TA2020_V_2) %>% 
  left_join(imms, by = "area") %>% 
  select(-contains("lower"), -contains("upper"), -area, -code) %>% 
  st_transform(4326)

# confidentialise data with low counts
imms.sf_conf <- imms.sf %>% 
  mutate(across(c(rate_total,rate_CI_total), ~ifelse(count_total < 6 & count_total > 0, NA, .)),
         across(c(rate_Asian,rate_CI_Asian), ~ifelse(count_Asian < 6 & count_Asian > 0, NA, .)),
         across(c(rate_European,rate_CI_European), ~ifelse(count_European < 6 & count_European > 0, NA, .)),
         across(c(rate_Maori,rate_CI_Maori), ~ifelse(count_Maori < 6 & count_Maori > 0, NA, .)),
         across(c(rate_Other,rate_CI_Other), ~ifelse(count_Other < 6 & count_Other > 0, NA, .)),
         across(c(rate_Pasifika,rate_CI_Pasifika), ~ifelse(count_Pasifika < 6 & count_Pasifika > 0, NA, .)))

# write_rds(imms.sf_conf, "mamahapu/data/imms.sf.RDS")

#### DHBs ####
imms_dhb <- influenza_yr_dhb %>% 
  rbind(pertusis_yr_dhb)

imms_dhb <- imms_dhb %>% 
  mutate(imms = case_when(imms == "influenza" ~ "Influenza",
                          imms == "pertussis" ~ "Pertussis",
                          TRUE ~ imms),
         dhb = case_when(dhb == "Capital and Coast" ~ "Capital \u0026 Coast",
                         dhb == "Tairawhiti" ~ "Tair\u101whiti",
                         dhb == "Waitemata" ~ "Waitemat\u101",
                         dhb == "Hutt" ~ "Hutt Valley",
                         TRUE ~ dhb))

#### Emerging hotspot analysis ####
## geometry
ta.sf <- ta %>% select(area = TA2020_V_2)

rates_long <- imms %>%
  select(area, year, imms, contains('rate'), -contains('CI')) %>%
  pivot_longer(cols = -c(area, year, imms), names_to = c(".value", "ethnicity"), names_pattern = "(.*)_(total|European|Maori|Pasifika|Asian|Other)") %>% 
  mutate(ethnicity = factor(ethnicity, levels = c("total", "Maori", "Pasifika", "European", "Asian", "Other"), labels = c("Overall", "M\u101ori", "Pasifika", "European", "Asian", "Other"))) %>% 
  # filtering all areas with NA's in any year
  group_by(area, imms, ethnicity) %>%
  filter(!any(is.na(rate))) %>%
  ungroup()

## Emerging hotspots analysis by immunisation and ethnicity
## there may be minor shifts in classifications because of simulations
## preparing data
rates_nested <- rates_long %>% 
  filter(year < 2021) %>% 
  group_by(imms, ethnicity) %>% 
  nest() %>% 
  # preparing spacetime dataset
  mutate(unique = map(data, ~unique(.x$area)),
         geo = map(unique, ~ ta.sf %>% filter(area %in% .x)),
         geo = map(geo, ~ .x %>% mutate(nb = include_self(st_contiguity(st_geometry(.x))))),
         geo = map(geo, ~ .x %>% mutate(wt = st_weights(.x$nb))),
         spt = map2(data, geo, ~spacetime(.x, .y, .loc_col="area", .time_col="year")))

## running analysis
rates_nested <- rates_nested %>% 
  # p < 0.05
  mutate(ehsa_005 = map(spt, ~emerging_hotspot_analysis(.x, "rate", k = 1, include_gi = T, nsim = 199, threshold = 0.05, nb_col = "nb", wt_col = "wt"))) %>%
  # p < 0.01
  mutate(ehsa_001 = map(spt, ~emerging_hotspot_analysis(.x, "rate", k = 1, include_gi = T, nsim = 199, threshold = 0.01, nb_col = "nb", wt_col = "wt")))

## unnest results
ehsa <- rates_nested %>% 
  select(imms, ethnicity, ehsa_005, ehsa_001) %>% 
  unnest(c(ehsa_005, ehsa_001), names_sep = "_") %>% 
  select(imms, ethnicity, area = ehsa_001_location, ehsa_001 = ehsa_001_classification, ehsa_005 = ehsa_005_classification) %>% 
  ungroup() %>% 
  complete(imms, ethnicity, area) %>% 
  mutate(across(c(ehsa_001, ehsa_005), ~replace_na(.,"Missing data"))) %>% 
  mutate(across(c(ehsa_001, ehsa_005), ~factor(., levels = c("no pattern detected", "new coldspot", "new hotspot", "consecutive coldspot", "consecutive hotspot",  "sporadic coldspot", "sporadic hotspot", "oscilating coldspot", "oscilating hotspot", "Missing data"),
                                               labels = c("No pattern detected", "New coldspot", "New hotspot", "Consecutive coldspot", "Consecutive hotspot",  "Sporadic coldspot", "Sporadic hotspot", "Oscillating coldspot", "Oscillating hotspot", "Missing data"))))

ehsa <- ta.sf %>% left_join(ehsa)

#### Visualisation ####
theme.custom <- theme(panel.background = element_rect(fill = alpha("#17aeff", 0.1)),
                      axis.line = element_line(colour = "black"),
                      panel.ontop = F,
                      plot.title = element_text(face = "bold", size = rel(1.25)),
                      plot.subtitle = element_text(size = rel(1.15)),
                      axis.title.y = element_blank(),
                      axis.title.x = element_blank(),
                      axis.text.y = element_text(face = "bold", size = rel(1)),
                      axis.text.x = element_text(face = "bold", size = rel(1), hjust = 0.5, vjust = 0.5, angle = 0),
                      legend.title = element_text(size = rel(1.15)),
                      legend.text = element_text(size = rel(1.05)),
                      panel.grid.major = element_line(linetype = "dashed", colour = "#D6D6D6", linewidth = 0.2),
                      strip.text = element_text(face = "bold", size = rel(0.9)))

# fix dhb names
dhb_pol <- dhb_pol %>% 
  mutate(DHB_label = case_when(DHB_label == "Capital and Coast" ~ "Capital \u0026 Coast",
                               DHB_label == "Mid Central" ~ "MidCentral",
                               DHB_label == "Tairawhiti" ~ "Tair\u101whiti",
                               DHB_label == "Waitemata" ~ "Waitemat\u101",
                               DHB_label == "Area outside District Health Board" ~ "Waikato",
                               TRUE ~ DHB_label))

dhb_pol <- dhb_pol %>% 
  group_by(dhb = DHB_label) %>% 
  summarise()

dhb <- dhb_pol %>% st_cast('MULTILINESTRING')

# manual scalebar
# 200km ~ 2.65
scalebar_manual <- matrix(c(177,179.65, -47.5, -47.37), nrow = 2) %>% 
  st_linestring() %>% 
  st_sfc() %>% 
  st_sf(s = "200 km", .) %>% 
  st_set_crs(4326)

#### Overall rates by DHB
rates_overall_dhb <- imms_dhb %>% 
  filter(year < 2021) %>% 
  group_by(dhb, imms) %>% 
  summarise(across(c(count, popn), ~sum(.)),
            across(c(lower_95CI, upper_95CI), ~round(mean(.),1))) %>% 
  mutate(rate = round(count/popn*100,1),
         rate_cat = cut(rate, breaks = c(13, seq(20,40,5), 50), include.lowest = T,
                        labels = c("13.0\u201320.0", "20.1\u201325.0", "25.1\u201330.0", "30.1\u201335.0", "35.1\u201340.0", "50.1\u201360.0")))

rates_overall_dhb <- dhb_pol %>% left_join(rates_overall_dhb) 

labs_dhb <- dhb_pol %>%
  st_point_on_surface() %>% 
  st_transform(4326) %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         imms = "Influenza") %>% 
  st_drop_geometry()

map_overall_dhb <-
  ggplot(data = rates_overall_dhb) +
  geom_sf(data = water_around, fill = alpha("#17aeff", 0.2), col = NA) +
  geom_sf(aes(fill = rate_cat), col = "#E5E5E5", size = 0.1, alpha = 0.9) +
  scale_fill_manual(values = carto_pal(n = 6, "BluYl"), na.value="#f5f5f5") +
  geom_sf(data = dhb, col = "#1A1A1A", size = 0.3) +
  geom_sf(data = scalebar_manual, size = 1.5) +
  annotate("text", x = 178.3, y = -47.05, label = "200 km", size = 3) +
  geom_sf(data = lakes, fill = "#59a9e1", col = NA) +
  ggrepel::geom_text_repel(data = labs_dhb, aes(x = x, y = y, label = dhb), col = "white", bg.color = alpha("black",0.25), bg.r = 0.25, size = 3, force = 0.2, force_pull = 0.5) + #bg.color = "white", bg.r = 0.15
  guides(fill = guide_legend(nrow = 1, title.position="top")) +
  labs(title = "Maternal immunisation coverage", subtitle = "Rates (%) by District Health Board | 2013\u20132020", fill = NULL) + #"immunisation coverage (%)"
  facet_wrap(~imms) +
  scale_x_continuous(breaks = seq(166,180,2))+
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme.custom +
  theme(legend.position = "top",
        legend.justification = "left")

ggsave("Rates_overall_DHB.png", plot = map_overall_dhb, units = "px", width = 2500, height = 2000, dpi = 300)
ggsave("Rates_overall_DHB.pdf", plot = map_overall_dhb, units = "px", width = 2500, height = 2000, dpi = 300)

map_overall_dhb + rates_dhb_pts

### graphs
rates_dhb_pts <-
  rates_overall_dhb %>% 
  st_drop_geometry() %>% 
  ggplot(aes(x = reorder(dhb,rate), y = rate, ymin = lower_95CI, ymax = upper_95CI, col = rate_cat, fill = rate_cat, shape = imms)) +
  geom_linerange(size = 0.7, alpha = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = carto_pal(n = 6, "BluYl"), na.value="#f5f5f5", guide = "none") +
  scale_fill_manual(values = carto_pal(n = 6, "BluYl"), na.value="#f5f5f5", guide = "none") +
  scale_shape_manual(values = c(15, 16)) +
  labs(x = "Immunisation rate (%)", 
       shape = "Immunisation") +
  coord_flip() +
  theme.custom +
  theme(panel.background = element_rect(fill = alpha("#17aeff", 0)),
        panel.grid.major.x = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.85,0.1))

ggsave("Rates_overall_DHB_graph.png", plot = rates_dhb_pts, units = "px", width = 1500, height = 2000, dpi = 300)
ggsave("Rates_overall_DHB_graph.pdf", plot = rates_dhb_pts, units = "px", width = 1500, height = 2000, dpi = 300)

#### Overall rates by TA with overlay of DHB boundary 
rates_overall <- imms %>% 
  filter(year < 2021) %>% 
  select(area, imms, count_total, popn_total) %>% 
  group_by(area, imms) %>% 
  summarise(across(c(count_total, popn_total), ~sum(.))) %>% 
  mutate(rate = count_total/popn_total*100,
         rate_cat = cut(rate, breaks = c(seq(0,60,10), 100), include.lowest = T,
                        labels = c("0.0\u201310.0", "10.1\u201320.0", "20.1\u201330.0", "30.1\u201340.0", "40.1\u201350.0", "50.1\u201360.0", "60.1\u2013100.0")))

rates_overall <- ta.sf %>% left_join(rates_overall)

map_overall_ta <- ggplot(data = rates_overall) +
  geom_sf(data = water_around, fill = alpha("#17aeff", 0.2), col = NA) +
  geom_sf(aes(fill = rate_cat), col = "#E5E5E5", size = 0.1, alpha = 0.9) +
  scale_fill_manual(values = carto_pal(n = 7, "BluYl"), na.value="#f5f5f5") +
  geom_sf(data = dhb, col = "#1A1A1A", size = 0.3) +
  geom_sf(data = scalebar_manual, size = 1.5) +
  annotate("text", x = 178.3, y = -47.05, label = "200 km", size = 3) +
  # geom_sf(data = lakes, fill = "#59a9e1", col = NA) +
  # coord_sf(datum = sf::st_crs(2193)) +
  guides(fill = guide_legend(nrow = 1, title.position="top")) +
  labs(title = "Maternal immunisation coverage", subtitle = "Rates (%) by Territorial Authority | 2013\u20132020", fill = NULL) + #"immunisation coverage (%)"
  facet_wrap(~imms) +
  scale_x_continuous(breaks = seq(166,180,2))+
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme.custom +
  theme(legend.position = "top",
        legend.justification = "left")

ggsave("Rates_overall_TA.png", plot = map_overall_ta, units = "px", width = 2500, height = 2000, dpi = 300)

#### Overall rates by DHB and year with overlay of DHB boundary
rates_plot_DHB <- dhb_pol %>% 
  left_join(imms_dhb) %>% 
  mutate(rate_cat = cut(rate, breaks = c(seq(0,60,10), 100), include.lowest = T,
                        labels = c("0.0\u201310.0", "10.1\u201320.0", "20.1\u201330.0", "30.1\u201340.0", "40.1\u201350.0", "50.1\u201360.0", "60.1\u2013100.0")))

### Flu
map_flu_dhb <- ggplot(rates_plot_DHB %>% filter(imms == "Influenza" & year < 2021)) +
  geom_sf(data = water_around, fill = alpha("#17aeff", 0.2), col = NA) +
  geom_sf(aes(fill = rate_cat), col = "#E5E5E5", size = 0.1, alpha = 0.9) +
  scale_fill_manual(values = carto_pal(n = 7, "BluYl"), na.value="#f5f5f5") +
  geom_sf(data = dhb, col = "#1A1A1A", size = 0.3) +
  geom_sf(data = scalebar_manual, size = 1.5) +
  annotate("text", x = 178.3, y = -47.05, label = "200 km", size = 3) +
  guides(fill = guide_legend(nrow = 1, title.position="top")) +
  labs(title = "Maternal immunisation coverage - Influenza", 
       subtitle = "Rates (%) by District Health Board | 2013\u20132020", 
       fill = NULL) + #"immunisation coverage (%)"
  facet_wrap(~year, nrow = 3) +
  scale_x_continuous(breaks = seq(166,180,2))+
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme.custom +
  theme(legend.position = "top",
        legend.justification = "left")

ggsave("Influenza_rates_DHB.png", plot = map_flu_dhb, units = "px", width = 3500, height = 4000, dpi = 300)

### Pertussis
map_pertussis_dhb <- ggplot(rates_plot_DHB %>% filter(imms == "Pertussis" & year < 2021)) +
  geom_sf(data = water_around, fill = alpha("#17aeff", 0.2), col = NA) +
  geom_sf(aes(fill = rate_cat), col = "#E5E5E5", size = 0.1, alpha = 0.9) +
  scale_fill_manual(values = carto_pal(n = 7, "BluYl"), na.value="#f5f5f5") +
  geom_sf(data = dhb, col = "#1A1A1A", size = 0.3) +
  geom_sf(data = scalebar_manual, size = 1.5) +
  annotate("text", x = 178.3, y = -47.05, label = "200 km", size = 3) +
  guides(fill = guide_legend(nrow = 1, title.position="top")) +
  labs(title = "Maternal immunisation coverage - Pertussis", 
       subtitle = "Rates (%) by District Health Board | 2013\u20132020", 
       fill = NULL) + #"immunisation coverage (%)"
  facet_wrap(~year, nrow = 3) +
  scale_x_continuous(breaks = seq(166,180,2))+
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme.custom +
  theme(legend.position = "top",
        legend.justification = "left")

ggsave("Pertussis_rates_DHB.png", plot = map_pertussis_dhb, units = "px", width = 3500, height = 4000, dpi = 300)

#### Overall rates by TA and year with overlay of DHB boundary 
rates_plot <- ta.sf %>% 
  left_join(rates_long %>% filter(ethnicity == "Overall")) %>% 
  mutate(rate_cat = cut(rate, breaks = c(seq(0,60,10), 100), include.lowest = T,
                        labels = c("0.0\u201310.0", "10.1\u201320.0", "20.1\u201330.0", "30.1\u201340.0", "40.1\u201350.0", "50.1\u201360.0", "60.1\u2013100.0")))

### Flu
map_flu <- ggplot(rates_plot %>% filter(imms == "Influenza"  & year < 2021)) +
  geom_sf(data = water_around, fill = alpha("#17aeff", 0.2), col = NA) +
  geom_sf(aes(fill = rate_cat), col = "#E5E5E5", size = 0.1, alpha = 0.9) +
  scale_fill_manual(values = carto_pal(n = 7, "BluYl"), na.value="#f5f5f5") +
  geom_sf(data = dhb, col = "#1A1A1A", size = 0.3) +
  geom_sf(data = scalebar_manual, size = 1.5) +
  annotate("text", x = 178.3, y = -47.05, label = "200 km", size = 3) +
  guides(fill = guide_legend(nrow = 1, title.position="top")) +
  labs(title = "Maternal immunisation coverage - Influenza", 
       subtitle = "Rates (%) by Territorial Authority | 2013\u20132020", fill = NULL) + #"immunisation coverage (%)"
  facet_wrap(~year, nrow = 3) +
  scale_x_continuous(breaks = seq(166,180,2))+
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme.custom +
  theme(legend.position = "top",
        legend.justification = "left")

ggsave("Influenza_rates.png", plot = map_flu, units = "px", width = 3500, height = 4000, dpi = 300)
ggsave("Fig3_Influenza_rates.tiff", plot = map_flu, units = "px", width = 3500, height = 4000, dpi = 300)

### Pertussis
map_pertussis <- ggplot(rates_plot %>% filter(imms == "Pertussis" & year < 2021)) +
  geom_sf(data = water_around, fill = alpha("#17aeff", 0.2), col = NA) +
  geom_sf(aes(fill = rate_cat), col = "#E5E5E5", size = 0.1, alpha = 0.9) +
  scale_fill_manual(values = carto_pal(n = 7, "BluYl"), na.value="#f5f5f5") +
  geom_sf(data = dhb, col = "#1A1A1A", size = 0.3) +
  geom_sf(data = scalebar_manual, size = 1.5) +
  annotate("text", x = 178.3, y = -47.05, label = "200 km", size = 3) +
  guides(fill = guide_legend(nrow = 1, title.position="top")) +
  labs(title = "Maternal immunisation coverage - Pertussis", 
       subtitle = "Rates (%) by Territorial Authority | 2013\u20132020", 
       fill = NULL) + #"immunisation coverage (%)"
  facet_wrap(~year, nrow = 3) +
  scale_x_continuous(breaks = seq(166,180,2))+
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme.custom +
  theme(legend.position = "top",
        legend.justification = "left")

ggsave("Pertussis_rates.png", plot = map_pertussis, units = "px", width = 3500, height = 4000, dpi = 300)
ggsave("Fig4_Pertussis_rates.tiff", plot = map_pertussis, units = "px", width = 3500, height = 4000, dpi = 300)

## Emerging hotspots visualisation
map_flu_ehsa_v2 <- ggplot(ehsa %>% filter(imms == "Influenza")) +
  geom_sf(data = water_around, fill = alpha("#17aeff", 0.2), col = NA) +
  geom_sf_pattern(aes(fill = ehsa_001, pattern_density = ehsa_001, pattern_fill = ehsa_001),
                  pattern = "circle",
                  pattern_spacing = 0.02,
                  pattern_colour = NA,
                  linewidth = 0.0375) +
  scale_fill_manual(values = c("#B3B3B3","#33BBEE", "#EE7733", "#0077BB","#CC3311", "#0077BB","#CC3311", "#33BBEE", "#EE7733", "#f5f5f5"), drop = F) +
  scale_pattern_density_manual(values = c(rep(0,5), rep(0.35,4),0), drop = F) +
  scale_pattern_fill_manual(values = c(rep(NA,5), rep("white",2),"#CC3311","#0077BB",NA), drop = F) +
  geom_sf(data = dhb, col = "#1A1A1A", linewidth = 0.12) +
  geom_sf(data = scalebar_manual, linewidth = 1.5) +
  annotate("text", x = 178.3, y = -47.05, label = "200 km", size = 3) +
  labs(title = "Maternal immunisation coverage - Influenza", 
       subtitle = "Emerging hotspot analysis | 2013\u20132020",
       fill = "Detected pattern", pattern_fill = "Detected pattern", pattern_density = "Detected pattern") +
  facet_wrap(~ethnicity) +
  scale_x_continuous(breaks = seq(166,180,2))+
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme.custom

ggsave("Influenza_ehsa_v2.png", plot = map_flu_ehsa_v2, units = "px", width = 3500, height = 3200, dpi = 300)
ggsave("Fig5_Influenza_ehsa_v22.tiff", plot = map_flu_ehsa_v2, units = "px", width = 3500, height = 3200, dpi = 300)

map_pertussis_ehsa_v2 <- ggplot(ehsa %>% 
                                  filter(imms == "Pertussis")) + #%>% 
  # mutate(group = case_when(.default = NA, str_detect(ehsa_001, "Oscilating|New") ~ "Pattern"))) +
  geom_sf(data = water_around, fill = alpha("#17aeff", 0.2), col = NA) +
  geom_sf_pattern(aes(fill = ehsa_001, pattern_density = ehsa_001, pattern_fill = ehsa_001),
                  pattern = "circle",
                  pattern_spacing = 0.02,
                  pattern_colour = NA,
                  linewidth = 0.0375) +
  scale_fill_manual(values = c("#B3B3B3","#33BBEE", "#EE7733", "#0077BB","#CC3311", "#0077BB","#CC3311", "#33BBEE", "#EE7733", "#f5f5f5"), drop = F) +
  scale_pattern_density_manual(values = c(rep(0,5), rep(0.35,4),0), drop = F) +
  scale_pattern_fill_manual(values = c(rep(NA,5), rep("white",2),"#CC3311","#0077BB",NA), drop = F) +
  geom_sf(data = dhb, col = "#1A1A1A", linewidth = 0.12) +
  geom_sf(data = scalebar_manual, linewidth = 1.5) +
  annotate("text", x = 178.3, y = -47.05, label = "200 km", size = 3) +
  labs(title = "Maternal immunisation coverage - Pertussis", 
       subtitle = "Emerging hotspot analysis | 2013\u20132020", 
       fill = "Detected pattern", pattern_fill = "Detected pattern", pattern_density = "Detected pattern") +
  facet_wrap(~ethnicity) +
  scale_x_continuous(breaks = seq(166,180,2))+
  coord_sf(default_crs = sf::st_crs(4326)) +
  theme.custom

ggsave("Pertussis_ehsa_v2.png", plot = map_pertussis_ehsa_v2, units = "px", width = 3500, height = 3200, dpi = 300)
ggsave("Fig6_Pertussis_ehsa_v2.tiff", plot = map_pertussis_ehsa_v2, units = "px", width = 3500, height = 3200, dpi = 300)
