library(shiny)
library(shinyWidgets)
library(htmlwidgets)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)

shinyServer(function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  # Filtering by topic based on the conditions from selectInput
  geodata <- reactive({
    if(input$category == "Total"){
      df <- immun %>% filter(imms == input$imms & year %in% input$datetime) %>% select(name, count_total:rate_total, rate_CI_total)
    } else {
     df <- immun %>% filter(imms == input$imms & year %in% input$datetime) %>% select(name, contains(input$variableeth))
    }
     names(df)[1:5] <- c("name", "count", "popn", "rate", "rate_CI")
     df
  })
  
  
  # create initial leaflet map with layers switch
  output$map <- renderLeaflet({
    leaflet(data = geodata(), options = leafletOptions(zoomControl = FALSE)) %>%
    # leaflet() %>%
      # addTiles(attribution = "&copy; <a href=\"https://www.health.govt.nz/our-work/preventative-health-wellness/immunisation/immunisation-coverage/national-and-dhb-immunisation-data\"> MoH NZ | Manatū Hauora - National and DHB Immunisation Data</a>") %>% 
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter (CartoDB)") %>%
      addProviderTiles("Stamen.Toner", group = "Stamen.Toner") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addLayersControl(baseGroups = c("DarkMatter (CartoDB)", "Stamen.Toner", "OpenStreetmap"),
                       position = "bottomright", options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      setView(176, -42, zoom = 6) %>%
      leaflet::addLegend(position = "bottomleft", colors = pal,
                         labels = c("0.0\u201210.0", "10.1\u201220.0","20.1\u201230.0","30.1\u201240.0","40.1\u201250.0","50.1\u201260.0","60.1\u201270.0","70.1\u201280.0","80.1\u201290.0","90.1\u2012100.0"),
                         values = ~(rate), title = "Maternal<br/>immunisation<br/>coverage (%)", opacity = 0.9) #%>%
    # addLogo(img = "Geohealth_Logo.png", position = "topleft", width = 293, height = 25, url = "https://www.canterbury.ac.nz/science/research/geohealth/", alpha = 1)
  })
  
  # Incremental changes to the map should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  ####
  observe({
    # colors
    pal <- colorBin(pal, bins = seq(0,100,10), reverse = F)

    # labels
    labels <- sprintf(
      # "<strong>%s</strong><br/><br/><strong>%s</strong> immunisation<br/>Rate (%%): %s<br/>Eligible population: %g<br/>Immunised: %g",
      # geodata()$name, input$imms, geodata()$rate_CI, geodata()$popn, geodata()$count) %>%
      "<strong>%s</strong><br/><strong>%s</strong> maternal immunisation<br/>Rate (%%): %s<br/>",
      geodata()$name, input$imms, geodata()$rate_CI) %>%
      lapply(htmltools::HTML)

    leafletProxy("map", data = geodata()) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~pal(rate), fillOpacity = 0.6, color = "white", opacity = 0.3, weight = 2, dashArray = "3", layerId = ~name,
                  highlight = highlightOptions(weight = 3, opacity = 1, color = "#666", bringToFront = TRUE),
                  label = labels)
  })
  
  output$taplot <- renderPlotly({
      g1 <- ggplot(data = geodata() %>% st_drop_geometry() %>% arrange(desc(name)), 
             aes(y = name, x = rate, size = popn, colour = cut(rate, breaks = seq(0,100,10), include.lowest = T),
                 # text = paste0("<b>",name, "</b> (", input$datetime,")","<br>Rate (%): <b>", rate_CI, "</b><br>Immunised: ", count,"<br>Eligible population: ", popn))) +
                 text = paste0("<b>",name, "</b> (", input$datetime,")","<br>Rate (%): <b>", rate_CI, "</b>"))) +
        geom_point(alpha = 0.5) +
        scale_colour_manual(values=pal) +
        scale_x_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
        labs(x = "Maternal immunisation rate (%)") +
        theme.custom.p +
        theme(legend.position = "none", axis.title.y = element_blank())
    
    title.g1 <- if(input$category == "Total"){paste0(input$imms, " maternal immunisation rates (", input$datetime, ")")
    } else {paste0(input$imms, " maternal immunisation rates: ", input$variableeth, " (", input$datetime, ")")}
    
    ggplotly(g1, tooltip = "text") %>% 
      layout(title = list(text = paste0("<b>",title.g1,"</b>"), y = 0.99, x = 0.5), 
             font = list(size = 11))
    
  })
  
  # select data based on the click to map
  # create a reactive value that will store the click position
  # get the data based on the click to map generate by reactive
  # records the click and stores the id of the layer clicked - need to set the layerid in the leaflet
  dclick <- reactiveValues(clickedShape=NULL)
  observeEvent(input$map_shape_click,{
    dclick$clickedShape <- input$map_shape_click
    print(dclick$clickedShape$id)
  })
  
  output$timeplot <- renderPlotly({
    siteid <- dclick$clickedShape$id
    if(is.null(siteid)){siteid = "Christchurch City"}

    tdata <- if(input$imms == "Influenza"){
      immun.plot %>% filter(imms == "Influenza" & name == siteid)
    } else {
      immun.plot %>% filter(imms == "Pertussis" & name == siteid)
    }
    
    g2 <- ggplot(tdata, aes(x = year, y = rate, group = eth, col = eth, lty = eth,
                            # text = paste0("<b>",name, "</b>"," (",year,")<br>",eth, "<br>Rate (%): <b>", rate_CI, "</b><br>Immunised: ", count,"<br>Eligible population: ", popn))) +
                            text = paste0("<b>",name, "</b>"," (",year,")<br>","Ethnicity:",eth, "<br>Rate (%): <b>", rate_CI, "</b>"))) +
      geom_point(size = 2.5, alpha = 0.8) +
      geom_line(alpha = 0.5, lwd = 0.5) +
      scale_color_manual(values = pal.eth, name = NULL) + 
      scale_linetype_manual(values = c("dashed", rep("solid",5)), name = NULL) +
      labs(x = "", y = "Maternal immunisation rate (%)") +
      scale_x_continuous(breaks = 2013:2021) +
      scale_y_continuous(limits = c(0, 100)) +
      theme.custom.p
    
    ggplotly(g2, tooltip = "text")  %>% 
      layout(title = list(text = paste0("<b>",input$imms, " maternal immunisation rate in ", siteid, "</b>"), y = 0.98, x = 0.5), 
             font = list(size = 11), legend = list(orientation = 'h', bgcolor = 'rgba(0,0,0,0)'),
             annotations = list(y = -1, text = "<b>Click on the area in the map to change TA</b>", showarrow = F, font = list(size = 13)))
  })
})