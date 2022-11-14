library(shiny)
library(shinyWidgets)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)

# read data
immun <- readRDS("data/imms.sf.RDS")

# transform data for second plot
immun.plot <- immun %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = -c(name, year, imms), names_to = c(".value", "eth"), names_pattern = "(.*)_(total|European|Maori|Pasifika|Asian|Other)") %>% 
  mutate(eth = factor(eth, levels = c("total", "Maori", "Pasifika", "European", "Asian", "Other"), labels = c("Overall", "M\u101ori", "Pasifika", "European", "Asian", "Other")))

#### set basemap ####
# Use leaflet() here, and only include aspects of the map that
# won't need to change dynamically (at least, not unless the
# entire map is being torn down and recreated).
# basemap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% #, maxZoom = 14
#   htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>%  
#   addMapPane("background_map", zIndex = 410) %>% 
#   addMapPane("polygons", zIndex = 420) %>% 
#   addMapPane("background_map_lines", zIndex = 430) %>% 
#   addMapPane("points", zIndex = 440) %>% 
#   addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}", attribution = "&copy; <a href=\"https://www.majortrauma.nz/\"> National Trauma Network | Te Hononga Wh&#275tuki &#x0101-Motu</a>") %>%
#   addProviderTiles(providers$Stamen.TonerLines, options = pathOptions(pane = "background_map_lines")) %>%
#   addProviderTiles(providers$Stamen.TonerLabels, options = providerTileOptions(opacity = 0.7)) %>%
#   setView(172.635, -41.52, zoom = 6)

### colours
# pal <- met.brewer("Tam", n = 10, type = "continuous")[1:10]
pal <- c("#FFD353", "#FFB945", "#F69A3B", "#E97435", "#DA4A32", "#BE2D2C", "#A82B47", "#832759", "#571D59", "#341648")
names(pal) <- levels(cut(1:100,seq(0,100,10), include.lowest = T))

pal.eth <- c("#4d4d4d", "#1D6996", "#EDAD08", "#94346E", "#0F8554", "#CC503E")
names(pal.eth) <- c("Overall", "M\u101ori", "Pasifika", "European", "Asian", "Other")

#### custom theme for graphs (plotly) ####
theme.custom.p <- theme(plot.background = element_rect(fill = NA),
                        panel.background = element_rect(fill = NA),
                        axis.line = element_line(colour = "black"),
                        panel.ontop = F,
                        text = element_text(family = "Calibri"),
                        plot.title = element_text(face = "bold", size = rel(0.95),hjust = 0),
                        plot.subtitle = element_text(size = rel(0.9)),
                        axis.title.y = element_text(face = "bold", hjust = 1.25, size = rel(0.9)),
                        axis.title.x = element_text(face = "bold", vjust = -1, size = rel(0.9)),
                        axis.text = element_text(colour = "black", size = rel(0.7)),
                        axis.ticks=element_blank(),
                        panel.grid.major.y = element_line(linetype = "dashed", colour = alpha("grey", 0.5), size = 1), 
                        panel.grid.minor.y = element_blank(),
                        panel.grid.major.x = element_line(linetype = "dashed", colour = alpha("grey", 0.5), size = 1), 
                        panel.grid.minor.x = element_blank(),
                        strip.text = element_text(face = "bold"))