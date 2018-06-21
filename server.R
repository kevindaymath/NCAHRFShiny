options(warn=-1)
# Packages <- c("shiny", "DT", "leaflet", "datasets", "devtools", "maptools", "rgdal", "rmapshaper", "rgeos","tidyr")
# lapply(Packages, library, character.only = TRUE)
load("data\\data.RData")


function(input, output) {
  # UI Reactives
  chosenCounty <- reactive({    # County input
    input$county
  })
  chosenVariable <- reactive({    # Map's variable
    input$variable
  })
  max<- reactive({    # Slider max
    input$range[2]
  })
  min<- reactive({    # Slider min
    input$range[1] 
    })
  
  # ____________________________________________________________________________________
  # Texts
  output$Calculated <- renderText({
    paste("<center><b>Calculated ")
  })
  output$Collected <- renderText({
    paste("<center><b>Collected ")
  })
  output$NumberTracts <- renderText({
    fips <- NCCountyList[which(NCCountyList$County.Name==chosenCounty()),1]
    paste(chosenCounty(), " has ",length(which(tractdata$Id2==fips)), " Census Tracts.")
  })
  output$countySummary <- renderText({
    paste("<h4><center><b>","Summary Measures for the Census Tracts Within",chosenCounty()," County")
  })
  output$region <- renderText({
    region <<- NCCountyList[NCCountyList$County.Name==chosenCounty(),3]
    string <- paste(chosenCounty(), "is in Region ", region)
    string2 <- paste("Region ", region, " has ", length(which(NCCountyList$LHD.Region==region)), " counties.")
    paste("<h3><center><b>",string,"<h4><center><b>",string2)
  })
  output$ComparisonInfo <- renderText({
    paste("Above lists each variable's p-value obtained from doing a paired T-test to compare how alike the collected county data",
          "and the calculated data using the county's tracts are. The population's value of NA indicates that the collected and",
          "calculated are exactly the same for each county. The race variables p-value are considered insignficant as there are only",
          "minor differences due to rounding and precision of percent decimals. The >= High School, >= Bachelors, and Below Poverty Level variables",
          "had p-values below 0.05, which indicate that there is a difference between these dataset pairs.")
  })
  output$Missing <- renderText({
    paste("<center><b>Census Tracts with Missing Variables Data  & Corresponding Counties with Those Tracts<br>(n) = Total # of Missing Variables")
  })
  output$ComparisonInfo2 <- renderText({
    paste("Some tracts were missing data for certain variables, and most of the 20 counties consistently had the most differences between
          collected and calculated data for the 3 variables with low paired T-test p-values.")
  })
  output$NCInfo <- renderText({
    paste("<b>NC has 2195 tracts, 100 counties, and 10 public health regions.")
  })

  
  # ____________________________________________________________________________________
  # Data tables
  output$factfinderCo <- renderDataTable({
    row <-which(CollectedCountyVars$`County Name`==chosenCounty())
    # z <- as.data.frame(t(CollectedCountyVars[row,4:18]))
    z <- cbind.data.frame(t(CollectedCountyVars[row,4:18]),t(CalculatedCountyVars[row,4:18]))
    datatable(z,colnames=c("Variable","Values from Collected Counties", "Values Calculated from Census Tracts"),
              options = list(pageLength = -1,dom = 't',initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().column( 0 ).nodes()).css('border-right','3px solid #000');",
                "$(this.api().table().column( 1 ).header()).css({'color': '#F39C12'});", # Orange
                "$(this.api().table().column( 2 ).header()).css({'color': '#33F8FF'});", # Aqua
                "}")))
  })
  
  
  
  output$CountyTracts <- renderDataTable({
    # fips <<- NCCountyList[which(NCCountyList$County.Name==chosenCounty()),1]
    # tractNames <- as.data.frame( tractdata[which(tractdata$Id2==fips),4] )
    fips <<- NCCountyList[which(NCCountyList$County.Name==chosenCounty()),1]
    tractNames <- tractdata[which(tractdata$Id2==fips),4]
    tractNames <- as.data.frame(tractNames[order(tractNames)])
    { if((nrow(tractNames)%%3) == 1){
      tractNames[nrow(tractNames)+1,] <- NA
      tractNames[nrow(tractNames)+1,] <- NA
      } 
      else if ((nrow(tractNames)%%3) == 2) {
        tractNames[nrow(tractNames)+1,] <- NA
        }
     }
    tractNames <- as.data.frame(split(tractNames, 1:3))
    colnames(tractNames) <- c("","","")
    datatable(tractNames,rownames = FALSE,options = list(pageLength = -1,dom = 't'))
  })
  
  output$TractMSR <- renderDataTable({
    row <-which(CountyTractMeans$County==chosenCounty())
    # z <- as.data.frame(t(CollectedCountyVars[row,4:18]))
    z <- cbind.data.frame(t(CountyTractMeans[row,2:16]),t(CountyTractStdev[row,2:16]),t(CountyTractRange[row,2:16]))
    datatable(z,colnames=c("Variable (Counts of Persons)","Mean","Stdev","Range (Max-Min)"), options = list(pageLength = -1,dom = 't',initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().column( 0 ).nodes()).css('border-right','3px solid #000');",
      
      "}")))
  })
  
  output$CountyPValue <- renderDataTable({
    datatable(CountyPairedPValues,rownames = FALSE,options = list(pageLength = -1,dom = 't',initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().row( 1 ).nodes()).css({'color': '#E74C3C'});",
      "$(this.api().table().row( 2 ).nodes()).css({'color': '#E74C3C'});",
      "$(this.api().table().row( 13 ).nodes()).css({'color': '#E74C3C'});",
      "}")))
  })
  
  output$MissingTracts <- renderDataTable({
    datatable(MissingTracts,rownames = FALSE, colnames = c("",""), selection = 'none',options = list(pageLength = -1,dom = 't'))
  })
  output$MissingCounties <- renderDataTable({
    datatable(MissingCounties,rownames = FALSE,colnames = c("",""),options = list(pageLength = -1,dom = 't'))
  })
  output$RegionCounties <- renderDataTable({
    region <<- NCCountyList[NCCountyList$County.Name==chosenCounty(),3]
    countyNames <- NCCountyList[which(NCCountyList$LHD.Region==as.character(region)),c(1,2)] 
    num <- lapply(countyNames$FIPS, function(x) {length(which(tractdata$Id2==x))})
    num <- unlist(num, use.names=FALSE)
    countyNames <- cbind(countyNames,num)
    colnames(countyNames) <- c("FIPS","County Name","Number of Tracts")
    datatable(countyNames,rownames = FALSE, options = list(pageLength = -1,dom = 't'))
  })
  output$factfinderRe <- renderDataTable({
    region <<- NCCountyList[NCCountyList$County.Name==chosenCounty(),3]
    row <- which(CollectedRegionVars$Region==region)
    z <- cbind.data.frame(t(CollectedRegionVars[row,3:17]),t(CalculatedRegionVars[row,3:17]))
    datatable(z,colnames=c("Variable","Values Calculated from Collected Counties","Values Calculated from Census Tracts -> Counties"),
              options = list(pageLength = -1,dom = 't',initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().column( 0 ).nodes()).css('border-right','3px solid #000');",
                "$(this.api().table().column( 1 ).header()).css({'color': '#F39C12'});", # Orange
                "$(this.api().table().column( 2 ).header()).css({'color': '#33F8FF'});", # Aqua
                "}")))
  })
  output$factfinderSt <- renderDataTable({
    z <- as.data.frame(t(factfinderSt[-1]))
    datatable(z,options = list(pageLength = -1,dom = 't'),colnames=("Value"))
  })
  output$StateMeans <- renderDataTable({
    z <- cbind.data.frame(StateMeans,CollectedStateCountiesMSR,CalculatedStateCountiesMSR)
    colNames<-c("State Mean","Collected Mean","Collected Stdev","Collected Range","Calculated Mean",
                "Calculated Stdev","Calculated Range")
    colnames(z) <- colNames
    datatable(z,options = list(pageLength = -1,dom = 't',initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().column( 1 ).header()).css({'color': '#33FF55'});", # Green
      "$(this.api().table().column( 2 ).header()).css({'color': '#F39C12'});", # Orange
      "$(this.api().table().column( 5 ).header()).css({'color': '#33F8FF'});", # Aqua
      "$(this.api().table().column( 0 ).nodes()).css('border-right','3px solid #000');",
      "$(this.api().table().column( 1 ).nodes()).css({'background-color': '#33FF55'});",
      "$(this.api().table().column( 2 ).nodes()).css({'background-color': '#F39C12'});",
      "$(this.api().table().column( 5 ).nodes()).css({'background-color': '#33F8FF'});",
      
      "}")))
  })
  
  output$RegionList <- renderDataTable({
    datatable(RegionList,rownames = FALSE,options = list(pageLength = -1,dom = 't',
    initComplete = JS("function(settings, json) {",
    "$(this.api().table().column( 0 ).nodes()).css('border-right','3px solid #000');}"  )))
  })
  
  
  # ____________________________________________________________________________________
  # Leaflets
  output$Map <- renderLeaflet({
    cnum <- which(mapCo$NAME==chosenCounty())
    labels <- sprintf("<strong>Region %s</strong><br/>", mapRe$NAME) %>% lapply(htmltools::HTML)
    labels2 <- sprintf("<strong> %s</strong> County<br/>", mapCo$NAME) %>% lapply(htmltools::HTML)
    labels3 <- sprintf("<strong> %s</strong> <br/>", mapTracts$NAMELSAD) %>% lapply(htmltools::HTML)
    leaflet() %>%
    addPolygons(data = mapRe, weight = 1.5,color="blue", group = "Regions",label = labels) %>%
    addMarkers(lng=coordinatesCo[cnum,1], lat=coordinatesCo[cnum,2], popup=chosenCounty()) %>%
    addPolygons(data = mapCo, weight = .8,fillOpacity = 0,color="red",group = "County Overlay", label = labels2) %>%
    addPolygons(data = mapTracts, weight = .7,fillOpacity = 0,color="yellow",group = "Tract Overlay", label = labels3) %>%
    addLayersControl(baseGroups = "Regions", options = layersControlOptions(collapsed = F), overlayGroups = c("County Overlay","Tract Overlay")) %>%
    hideGroup(c("County Overlay","Tract Overlay"))%>%
    setView(-79.8, 35.5, 6.5)
 }) #END RENDERLEAFLET OUTPUT

  
  output$CalculatedMap <- renderLeaflet({
    # bins <- c(0,25,50,75,100)
    quint <- (max()-min())/5
    bins <- c(min(), min()+quint, min()+2*quint,min()+3*quint,min()+4*quint,max())
    pal <- colorBin("YlOrRd", domain = CalculatedCountyVars[,chosenVariable()], bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g %%", mapCo$NAME,
                      CalculatedCountyVars[,chosenVariable()]) %>% lapply(htmltools::HTML) 
    labels2 <- sprintf("<strong>Region %s</strong><br/>%g %%", mapRe$NAME,CalculatedRegionVars[,chosenVariable()]) %>% lapply(htmltools::HTML)
    m <- leaflet(CalculatedCountyVars) %>%
      setView(-79.8, 35.5, 6.5) %>% 
      addPolygons(data = mapCo, fillColor = ~pal(CalculatedCountyVars[,chosenVariable()]), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7,group = "Counties",
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                           padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>%
      addPolygons(data = mapRe, fillColor = ~pal(CalculatedRegionVars[,chosenVariable()]), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7,group = "Regions",
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels2, labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                           padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>%
      addPolygons(data = mapRe, weight = 1.5,fillOpacity = 0,group = "Region Overlay",color="blue", label = labels2) %>%
      addLayersControl(baseGroups = c("Counties","Regions"), options = layersControlOptions(collapsed = F), overlayGroups = "Region Overlay")%>%
      hideGroup("Region Overlay") %>%
       addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright") 
    })
  
  
  output$CollectedMap <- renderLeaflet({
    # bins <- c(0,25,50,75,100)
    quint <- (max()-min())/5
    bins <- c(min(), min()+quint, min()+2*quint,min()+3*quint,min()+4*quint, max())
    pal <- colorBin("YlOrRd", domain = CollectedCountyVars[,chosenVariable()], bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g %%", mapCo$NAME,
                      CollectedCountyVars[,chosenVariable()]) %>% lapply(htmltools::HTML) 
    labels2 <- sprintf("<strong>Region %s</strong><br/>%g %%", mapRe$NAME,CollectedRegionVars[,chosenVariable()]) %>% lapply(htmltools::HTML)
    m <- leaflet(CollectedCountyVars) %>%
      setView(-79.8, 35.5, 6.5) %>% 
      addPolygons(data = mapCo, fillColor = ~pal(CollectedCountyVars[,chosenVariable()]), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7,group = "Counties",
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                           padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>% 
      addPolygons(data = mapRe, fillColor = ~pal(CollectedRegionVars[,chosenVariable()]), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7,group = "Regions",
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels2, labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                           padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>%
      addPolygons(data = mapRe, weight = 1.5,fillOpacity = 0,group = "Region Overlay",color="blue", label = labels2) %>%
      addLayersControl(baseGroups = c("Counties","Regions"), options = layersControlOptions(collapsed = F), overlayGroups = "Region Overlay")%>%
      hideGroup("Region Overlay") %>%
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
    m
  })
  
  output$PopulationCoMap <- renderLeaflet({
    bins <- c(4000, 20000, 40000, 60000, 90000, 125000, 999999)
    pal <- colorBin("YlOrRd", domain = CollectedCountyVars$`ACS 2011-2015 Population`, bins = bins)
    labels <- sprintf("<strong>%s County</strong><br/>%g people", CollectedCountyVars$`County Name`,
                      CollectedCountyVars$`ACS 2011-2015 Population`) %>% lapply(htmltools::HTML) 
    m <- leaflet(CollectedCountyVars) %>%
      setView(-79.7, 35, 6.5) %>% 
      addPolygons(data = mapCo, fillColor = ~pal(CollectedCountyVars$`ACS 2011-2015 Population`), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px", direction = "auto")) %>% 
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
    m 
    })
  

  
  
}