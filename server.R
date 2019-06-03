################################################################################
library(shiny)
library(RMySQL)
library(lubridate)
library(ggplot2)
library(leaflet)
library(gridExtra)
library(cowplot)
library(RColorBrewer)
library(scales)
library(zoo)

shinyServer(function(input, output, session){
  shp_df <- readRDS("data/shape.rds")
  peskaDAT = RMySQL::dbConnect(RMySQL::MySQL(), user='wilko_sql1229113', password='R1fDe1QDB6', 
                               dbname='wilko_peskaDAT', host='johnny.heliohost.org', port=3306)
  lndgs <- RMySQL::dbReadTable(peskaDAT, "landings")
  trps <- RMySQL::dbReadTable(peskaDAT, "trips")
  munis <- RMySQL::dbReadTable(peskaDAT, "municipalities")
  stns <- RMySQL::dbReadTable(peskaDAT, "stations")
  spcs <-  RMySQL::dbReadTable(peskaDAT, "species")
  #geos <- RMySQL::dbReadTable(peskaDAT, "geo")
  RMySQL::dbDisconnect(peskaDAT)
  geos <- readRDS("data/geo.rds")
  geos$Time <- as.Date(geos$Time)
  
  geos$Lat <- geos$Lat * 1000
  geos$Lng <- geos$Lng * 1000
  geos$Lat <- 2 * round(geos$Lat/2)
  geos$Lng <- 2 * round(geos$Lng/2)
  geos$Lat <- geos$Lat/1000
  geos$Lng <- geos$Lng/1000
  
  geos <- geos[!duplicated(geos[c("Trip", "Lat", "Lng")]), ]
  geos$latlng <- paste0(geos$Lat, ",", geos$Lng)
  
  
  trps$gear <- match(trps$gear, c("GN", "HL", "LL", "SG", "CN", "MC", "BS", "SN", "TP"))
  trps$station <- stns$municipio[match(trps$station, stns$station)]
  trps$trip_hours[trps$trip_hours == 0] <- 3
  trps$trip_hours[trps$trip_hours > 72] <- 72
  trps$trip_effort[trps$trip_effort == 0] <- 3
  trps$trip_effort[trps$trip_effort > 144] <- 144
  ## gear match must be before discard step
  discards <- lndgs$trip_id[!(lndgs$flag %in% c(0L, 4L, 5L))]## trip ids
  trpnas <- is.na(trps$station) | is.na(trps$habitat) | is.na(trps$gear) | is.na(trps$trip_effort) #logical
  discards <- c(discards, trps$trip_id[trpnas])
  lndnas <- is.na(lndgs$weight_g) 
  discards <- c(discards, lndgs$trip_id[lndnas])
  trps$date <- as.Date(trps$date)
  datnas <- trps$date < as.Date("2016-09-01") | trps$date >= lubridate::floor_date(Sys.Date(), unit = "day")
  discards <- c(discards, trps$trip_id[datnas])
  discards <- unique(discards)
  trps <- trps[!(trps$trip_id %in% discards), ]
  lndgs <- lndgs[!(lndgs$trip_id %in% discards), ]
  lndgs$KG <- lndgs$weight_g/1000
  
  ## next 2 lines = 10x faster than aggregate
  tmp <- split(lndgs$KG, f = factor(lndgs$trip_id, levels = trps$trip_id), drop = FALSE)
  trps$KG <- vapply(tmp, sum, 0, USE.NAMES = FALSE)
  ## date landings tab (day not month)
  tmp <- structure(trps$date, names = trps$trip_id)
  lndgs$date <- tmp[lndgs$trip_id]
  sites <- c("Viqueque","Lautem","Manatuto","Liquica","Bobonaro","Covalima",
             "Manufahi","Ainaro","Baucau","Dili","Oe-Cusse", "Atauro")   
  habitats <- c("Reef/Ahu ruin","FAD/Rumpon","Deep/Tasi kle'an","Beach/Tasi ninin",
            "Traditional FAD/Rumpon bamboo","Mangrove/Aiparapa","Gleaning/Meti")
  gears <- c("Gillnet/Redi","Handline/Hakail","Longline/Hakail naruk","Spear/Kilat","Cast net/Dai",
             "Manual/Meti","Beach seine/Redi tasi ninin", "Seine net/Lampara","Trap/Bubur")
  boat_types <- c("Canoe", "Motor")
  observe({
    if(input$selectall_habitat == 0){
      return(NULL)
    }else if(input$selectall_habitat %% 2 == 0){
      updateCheckboxGroupInput(session, "habitat", choices=habitats,selected=habitats)
    }else{
      updateCheckboxGroupInput(session, "habitat", choices=habitats)
    }
  })
  observe({
    if(input$selectall_gear == 0){
      return(NULL)
    }else if(input$selectall_gear %% 2 == 0){
      updateCheckboxGroupInput(session, "gear", choices=gears,selected=gears)
    }else{
      updateCheckboxGroupInput(session, "gear", choices=gears)
    }
  })
  observe({
    if(input$selectall_boat_type == 0){
      return(NULL)
    }else if(input$selectall_boat_type %% 2 == 0){
      updateCheckboxGroupInput(session, "boat_type", choices=boat_types, selected=boat_types)
    }else{
      updateCheckboxGroupInput(session, "boat_type", choices=boat_types)
    }
  })
  observe({
    if(input$selectall_site == 0){
      return(NULL)
    }else if(input$selectall_site %% 2 == 0){
      updateCheckboxGroupInput(session,"site", choices=sites,selected=sites)
    }else{
      updateCheckboxGroupInput(session,"site", choices=sites)
    }
  })
  datasetInput <- reactive({
    ## shape file
    shp_df2 <- shp_df
    shp_df2$colr[shp_df2$id %in% input$site] <- "tomato"
    ## select date range
    trps <- trps[trps$date > input$startDate & trps$date < input$endDate, ]
    lndgs <- lndgs[lndgs$date > input$startDate & lndgs$date < input$endDate, ]
    geos <- geos[geos$Time > input$startDate & geos$Time < input$endDate, ]

    ## month should be a factor, make sure all poss levels are included
    dateseq <- seq.Date(from = input$startDate, to = input$endDate, by = 1)
    datelevels <- as.character(unique(lubridate::floor_date(dateseq, unit = "month")))
    trps$month <- factor(lubridate::floor_date(trps$date, unit = "month"), levels = datelevels)
    
    #trps$date <- as.character(trps$date)
    #lndgs$date <- as.character(lndgs$date)
    ## sites
    indices <- match(input$site, sites)
    if(length(indices) == 0) return(NULL)
    munis <- munis[indices, ]
    trps <- trps[trps$station %in% indices, ]
    ## habitats
    indices <- match(input$habitat, habitats)
    if(length(indices) == 0) return(NULL)
    trps <- trps[trps$habitat %in% indices, ]
    allhabselected <- all(1:7 %in% indices)
    ## gear types
    indices <- match(input$gear, gears)
    if(length(indices) == 0) return(NULL)
    ## indices <- unlist(list(c(1, 5, 7:11), 2:3, 4, 6)[indices], use.names = FALSE)
    trps <- trps[trps$gear %in% indices, ]
    allgearselected <- all(1:9 %in% indices)
    ## boat type
    indices <- match(input$boat_type, boat_types)
    if(length(indices) == 0) return(NULL)
    if(!(1L %in% indices)) munis$canoes <- 0
    if(!(2L %in% indices)) munis$motors <- 0
    trps <- trps[trps$boat_type %in% indices, ]
    if(nrow(trps) < 30) return(NULL)
    ## purge landings table
    lndgs <- lndgs[lndgs$trip_id %in% trps$trip_id, ]
    ###########################################################
    ## aggregate species first since not dependent on trip hours
    mysums <- split(lndgs$KG, f = factor(lndgs$species, levels = unique(lndgs$species)), drop = FALSE)
    mysums <- vapply(mysums, sum, 0, USE.NAMES = TRUE)
    mysums <- sort(mysums, decreasing = TRUE)
    if(length(mysums) > 9){
      others <- sum(mysums[seq(10, length(mysums))])
      mysums <- mysums[1:9]
      if("300" %in% names(mysums)){
        mysums["300"] <- mysums["300"] + others
      }else{
        names(others) <- "300"
        mysums <- c(mysums, others)
      }
    }
    mysums <- mysums/1000 ## convert to tonnes
    tmpnms <- paste0(spcs$category, " (", spcs$category_tetun, ")")
    names(mysums) <- tmpnms[match(as.integer(names(mysums)), spcs$species)]
    sppKG <- data.frame(species = names(mysums), Tonnes = mysums)
    ######################################################################
    ## Make dataframe for summary plot
    monthlyCPUE <- aggregate(trps[c("KG", "trip_effort")], by = trps["month"], sum, drop = FALSE) 
    monthlyCPUE$month <- as.Date(as.character(monthlyCPUE$month))
    monthlyCPUE <- monthlyCPUE[order(monthlyCPUE$month),]
    monthlyCPUE$CPUE <- monthlyCPUE$KG/monthlyCPUE$trip_effort
    monthlyCPUE$trips <- table(trps$month)
    f <- function(d) length(unique(d))
    monthlyCPUE$fishing_days <- aggregate(trps$date, by = list(trps$month), f, drop = FALSE)[[2]]
    monthlyCPUE$KG[is.na(monthlyCPUE$KG)] <- 0
    monthlyCPUE$trip_effort[is.na(monthlyCPUE$trip_effort)] <- 0
    monthlyCPUE$CPUE[is.na(monthlyCPUE$CPUE)] <- 0
    monthlyCPUE$fishing_days[is.na(monthlyCPUE$fishing_days)] <- 0
    ## calculate national catch
    uepertrip_canoe <- 4 #4
    uepertrip_motor <- 10
    uepertrip_shore <- 3
    tripspermonth_canoe <- 8.2
    tripspermonth_motor <- 15.3
    ncanoes <- sum(munis$canoes)
    nmotors <- sum(munis$motors)
    ## calculate canoe total catch
    canoe_trps <- trps[c("KG", "trip_effort", "month")][trps$boat_type == 1, ]
    if(nrow(canoe_trps) > 0 & allgearselected & allhabselected){
      monthlyCPUE_canoe <- aggregate(canoe_trps[c("KG", "trip_effort")], by = canoe_trps["month"], sum, drop = FALSE) 
      monthlyCPUE_canoe <- monthlyCPUE_canoe$KG/monthlyCPUE_canoe$trip_effort
      monthlyCPUE_canoe[is.na(monthlyCPUE_canoe)] <- 0
      natcatch_canoe <- monthlyCPUE_canoe * uepertrip_canoe * tripspermonth_canoe * ncanoes * 0.001
    }else{
      natcatch_canoe <- 0
    }
    ## calculate motor total catch
    motor_trps <- trps[c("KG", "trip_effort", "month")][trps$boat_type == 2, ]
    if(nrow(motor_trps) > 0 & allgearselected & allhabselected){
      monthlyCPUE_motor <- aggregate(motor_trps[c("KG", "trip_effort")], by = motor_trps["month"], sum, drop = FALSE) 
      monthlyCPUE_motor <- monthlyCPUE_motor$KG/monthlyCPUE_motor$trip_effort
      monthlyCPUE_motor[is.na(monthlyCPUE_motor)] <- 0
      natcatch_motor <- monthlyCPUE_motor * uepertrip_motor * tripspermonth_motor * nmotors * 0.001
    }else{
      natcatch_motor <- 0
    }
    monthlyCPUE$natcatch <- natcatch_canoe + natcatch_motor
    monthlyCPUE <- monthlyCPUE[c(1, 6, 5, 3, 2, 4, 7)]
    # c("Month", "Reported fishing days", "Reported trips",
    #   "Reported effort (hours)", "Reported catch (KG)", 
    #   "CPUE (KG/hour)", "Estimated national catch (T)")
    ######################################################################
    ## Make dataframe for station plot
    trps$ntrips <- 1L ## could scale dot size to ntrips (or just nhours)
    stationCPUE <- aggregate(trps[c("KG", "trip_effort")], by = trps[c("date", "station")], sum, drop = TRUE)
    ### remove stations with too few entries
    stationtab <- table(stationCPUE$station)
    discards <- as.integer(names(stationtab)[stationtab < 5])
    stationCPUE <- stationCPUE[!stationCPUE$station %in% discards, ]
    # stationCPUE$date <- as.Date(as.character(stationCPUE$Date)) ## factor not valid for plotting
    stationCPUE$CPUE <- stationCPUE$KG/stationCPUE$trip_effort
    stationCPUE$station <- as.factor(sites[stationCPUE$station])
    stationCPUE$fit <- stationCPUE$CPUE
    if(input$smoothen > 0){
      for(i in unique(stationCPUE$station)){
        l <- stationCPUE$station == i
        stationCPUE$fit[l] <- smooth.spline(as.integer(stationCPUE$date)[l], stationCPUE$CPUE[l], 
                                            w = stationCPUE$trip_effort[l], spar = input$smoothen)$y
      }
    }
    stationCPUE$fit[stationCPUE$fit < 0] <- 0
    ######################################################################
    ## Make dataframe for habitat plot
    habitatCPUE <- aggregate(trps[c("KG", "trip_effort")], by = trps[c("date", "habitat")], sum, drop = TRUE)
    ### remove stations with too few entries
    habitattab <- table(habitatCPUE$habitat)
    discards <- as.integer(names(habitattab)[habitattab < 5])
    habitatCPUE <- habitatCPUE[!habitatCPUE$habitat %in% discards, ]
    habitatCPUE$CPUE <- habitatCPUE$KG/habitatCPUE$trip_effort
    habitatCPUE$habitat <- as.factor(habitats[habitatCPUE$habitat])
    habitatCPUE$fit <- habitatCPUE$CPUE
    if(input$smoothen > 0){
      for(i in unique(habitatCPUE$habitat)){
        l <- habitatCPUE$habitat == i
        habitatCPUE$fit[l] <- smooth.spline(as.integer(habitatCPUE$date)[l], habitatCPUE$CPUE[l], 
                                            w = habitatCPUE$trip_effort[l], spar = input$smoothen)$y
      }
    }
    habitatCPUE$fit[habitatCPUE$fit < 0] <- 0
    ######################################################################
    ## Make dataframe for habitat plot
    gearCPUE <- aggregate(trps[c("KG", "trip_effort")], by = trps[c("date", "gear")], sum, drop = TRUE)
    geartab <- table(gearCPUE$gear)
    discards <- as.integer(names(geartab)[geartab < 5])
    gearCPUE <- gearCPUE[!gearCPUE$gear %in% discards, ]
    gearCPUE$CPUE <- gearCPUE$KG/gearCPUE$trip_effort
    gearCPUE$gear <- as.factor(gears[gearCPUE$gear])
    gearCPUE$fit <- gearCPUE$CPUE
    if(input$smoothen > 0){
      for(i in unique(gearCPUE$gear)){
        l <- gearCPUE$gear == i
        gearCPUE$fit[l] <- smooth.spline(as.integer(gearCPUE$date)[l], gearCPUE$CPUE[l], 
                                         w = gearCPUE$trip_effort[l], spar = input$smoothen)$y
      }
    }
    gearCPUE$fit[gearCPUE$fit < 0] <- 0
    ######################################################################
    ## Make heat table for map
    bring_heat <- function(tomas){
      latlng <- split(tomas$latlng, f = tomas$latlng)
      tmp <- strsplit(vapply(latlng, "[", "", 1), split = ",")
      out <- data.frame(Lat = as.numeric(vapply(tmp, "[", "", 1, USE.NAMES = FALSE)),
                        Lng = as.numeric(vapply(tmp, "[", "", 2, USE.NAMES = FALSE)),
                        counts = vapply(latlng, length, 0L, USE.NAMES = FALSE))
      return(out)
    }
    heat <- bring_heat(geos)
    ######################################################################
    ## Return object for plots
    return(list(monthlyCPUE = monthlyCPUE, stationCPUE = stationCPUE, habitatCPUE = habitatCPUE, 
                gearCPUE = gearCPUE, sppKG = sppKG, shp_df2 = shp_df2, heat = heat,
                allgearselected = allgearselected, allhabselected = allhabselected))
    ######################################################################
  })
  output$plot0 <- renderLeaflet({
    obj <- datasetInput()
    heat <- obj$heat
    heat$counts <- round(log(heat$counts) + 1)
    pal <- colorBin("YlOrRd", domain = seq(0, max(heat$counts)))
    m <- leaflet()
    #m <- addTiles(m)
    m <- setView(m, lng = 126, lat = -8.6, zoom = 9)
    m <- addMapPane(m, "background_map", zIndex = 410) 
    m <- addMapPane(m, "polygons", zIndex = 420)
    m <- addMapPane(m, "labels", zIndex = 430)
    m <- addProviderTiles(m, providers$Esri.WorldImagery, options = pathOptions(pane = "background_map"))
    m <- addRectangles(m, lng1=heat$Lng - 0.002, lat1=heat$Lat - 0.002,
                       lng2=heat$Lng + 0.002, lat2=heat$Lat + 0.002,
                       stroke = FALSE, fillOpacity = 1, color = pal(heat$counts),
                       options = pathOptions(pane = "polygons"))
    #m <- addProviderTiles(m, providers$Stamen.TonerLabels, options = pathOptions(pane = "labels"))

    
    m
  })
  output$plot1 <- renderPlot({
    obj <- datasetInput()
    shp_df2 <- obj$shp_df2
    monthlyCPUE <- obj$monthlyCPUE
    if(is.null(monthlyCPUE)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    monthlyCPUE$Tonnes <- monthlyCPUE$KG/1000
    
    #### map plot
    map <- ggplot() + 
      geom_polygon(data = shp_df2, 
                   aes(x = long, y = lat, group = group, fill = colr), # fill = id, fill = '#A4A4A4'
                   colour = "black") + 
      scale_fill_identity() +
      #theme(panel.border = element_rect(colour = "black", fill=NA, size=5))
      # opts(title = "Title", panel.background = theme_rect(fill = "grey90"))+
      theme(panel.spacing = unit(c(0,0,0,0), "cm"),
            plot.margin = unit(c(0,0,0,0), "cm"),
            axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_rect(linetype = "solid", color = "black", size=1),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            aspect.ratio=0.5)
      #theme_void()
    
    ### CPUE plot

    cpue <- ggplot() +
      geom_col(aes(month, CPUE), monthlyCPUE, fill = "cornflowerblue") +
      xlab("") +
      ylab("Monthly Catch Per Unit Effort (KG/h)") +
      ylim(c(0, max(monthlyCPUE$CPUE) * 1.5)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            #aspect.ratio=1,
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(labels=date_format("%b-%Y"), 
                   breaks = seq(as.Date(monthlyCPUE$month[1]) + 3, 
                                as.Date(monthlyCPUE$month[nrow(monthlyCPUE)])  + 3, 
                                by = "1 month"))
    
    catch <- ggplot() +
      geom_col(aes(month, Tonnes), monthlyCPUE, fill = "cornflowerblue") +
      xlab("") +
      ylab("Reported Catch (T)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            #aspect.ratio=1,
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(labels=date_format("%b-%Y"), 
                   breaks = seq(as.Date(monthlyCPUE$month[1]) + 3, 
                                as.Date(monthlyCPUE$month[nrow(monthlyCPUE)])  + 3, 
                                by = "1 month"))
    
    effort <- ggplot() +
      geom_col(aes(month, trip_effort), monthlyCPUE, fill = "cornflowerblue") +
      xlab("") +
      ylab("Reported Effort (h)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            #aspect.ratio=1,
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(labels=date_format("%b-%Y"), 
                   breaks = seq(as.Date(monthlyCPUE$month[1]) + 3, 
                                as.Date(monthlyCPUE$month[nrow(monthlyCPUE)])  + 3, 
                                by = "1 month"))
    
    plot.with.inset <- ggdraw() +
      draw_plot(cpue) +
      draw_plot(map, x = 0.55, y = 0.77, width = .4,  height = .2)
    
    grid.arrange(catch, effort, plot.with.inset, layout_matrix = rbind(c(1,3), 2:3))
    # obj <- datasetInput()

  })
  
  output$plot2 <- renderPlot({
    obj <- datasetInput()
    stationCPUE <- obj$stationCPUE
    if(is.null(stationCPUE)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    stationCPUE$fit <- stationCPUE$fit + 1
    #ggplot(data = stationCPUE, aes(x = Date, y = CPUE, colour = station)) +
    ggplot(data = stationCPUE, aes(x = date, y = fit, colour = station, size = trip_effort)) +
      geom_point() +
      xlab("") +
      ylab("Daily Catch Per Unit Effort (KG/hour)") +
      labs(colour = "Municipality", size = "Reported hours") +
      #scale_colour_brewer(palette="Set2") +
      scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 11, 21, 31, 41, 51), 
                    labels = c("0","1","2","3","4","5","10", "20","30","40","50")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
  })
  
  output$plot3 <- renderPlot({
    obj <- datasetInput()
    habitatCPUE <- obj$habitatCPUE
    if(is.null(habitatCPUE)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    habitatCPUE$fit <- habitatCPUE$fit + 1
    ggplot(data = habitatCPUE, aes(x = date, y = fit, colour = habitat, size = trip_effort)) +
      geom_point() +
      xlab("") +
      ylab("Daily Catch Per Unit Effort (KG/hour)") +
      labs(colour = "Habitat", size = "Reported hours") +
      #scale_colour_brewer(palette="Set2") +
      scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 11, 21, 31, 41, 51), 
                    labels = c("0","1","2","3","4","5","10", "20","30","40","50")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
    
    # habitatCPUE$habitat <- as.factor(habitats[habitatCPUE$habitat])
    # ggplot(data = habitatCPUE,
    #        aes(x = date, y = CPUE, colour = habitat)) +
    #   geom_line()+
    #   xlab("") +
    #   ylab("CPUE (kg/hour)") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
    #         panel.background = element_rect(fill = "white",
    #                                         colour = "black",
    #                                         size = 0.5, linetype = "solid")) +
    #   scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
  })
  
  output$plot4 <- renderPlot({
    obj <- datasetInput()
    gearCPUE <- obj$gearCPUE
    if(is.null(gearCPUE)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    gearCPUE$fit <- gearCPUE$fit + 1
    ggplot(data = gearCPUE, aes(x = date, y = fit, colour = gear, size = trip_effort)) +
      geom_point() +
      xlab("") +
      ylab("Daily Catch Per Unit Effort (KG/hour)") +
      labs(colour = "Gear type", size = "Reported hours") +
      #scale_colour_brewer(palette="Set2") +
      scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 11, 21, 31, 41, 51), 
                    labels = c("0","1","2","3","4","5","10", "20","30","40","50")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
    
    # gearCPUE$gear <- as.factor(gears[gearCPUE$gear])
    # ggplot(data = gearCPUE,
    #        aes(x = date, y = CPUE, colour = gear)) +
    #   geom_line()+
    #   xlab("") +
    #   ylab("CPUE (kg/hour)") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
    #         panel.background = element_rect(fill = "white",
    #                                         colour = "black",
    #                                         size = 0.5, linetype = "solid")) +
    #   scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
  })
  
  output$plot5 <- renderPlot({
    obj <- datasetInput()
    sppKG <- obj$sppKG
    if(is.null(sppKG)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    ggplot(sppKG, aes(x="", y=Tonnes, fill=species))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      labs(title = "Catch by species (T)", x = "", y = "")+
      scale_fill_brewer(palette="Paired") +
      theme_bw()
  })
  
  output$table1 <- renderTable({
    obj <- datasetInput()
    monthlyCPUE <- obj$monthlyCPUE
    allgearselected <- obj$allgearselected
    allhabselected <- obj$allhabselected
    colnames(monthlyCPUE) <- c("Month", "Reported fishing days", "Reported trips",
                               "Reported effort (hours)", "Reported catch (KG)", 
                               "CPUE (KG/hour)", "Estimated national catch (T)")
    monthlyCPUE$Month <- format(monthlyCPUE$Month, "%b-%Y")
    totals <- apply(monthlyCPUE[, 2:5], 2, sum)
    average <- mean(monthlyCPUE[, 6])
    natcatch <- sum(monthlyCPUE[, 7])
    brow <- as.data.frame(c(as.list("Total"), as.list(totals), as.list(average), as.list(natcatch)))
    colnames(brow) <- colnames(monthlyCPUE)
    monthlyCPUE <- rbind(monthlyCPUE, brow)
    for(i in c(2:5, 7)) monthlyCPUE[[i]] <- as.integer(monthlyCPUE[[i]])
    monthlyCPUE[6] <- round(monthlyCPUE[6], 2)
    ## Only print national catch ests if all gears and habs selected
    if(!(allgearselected & allhabselected)) monthlyCPUE <- monthlyCPUE[1:6]
    monthlyCPUE
  })
  
  output$summaryTab.csv <- downloadHandler(
    filename = "peskador.csv",
    content = function(file) {
      obj <- datasetInput()
      monthlyCPUE <- obj$monthlyCPUE
      if(allgearselected & allhabselected){
        colnames(monthlyCPUE) <- c("Month", "Reported fishing days", "Reported trips",
                                   "Reported effort (hours)", "Reported catch (KG)", 
                                   "CPUE (KG/hour)", "Estimated national catch (T)")
      }else{
        colnames(monthlyCPUE) <- c("Month", "Reported fishing days", "Reported trips",
                                   "Reported effort (hours)", "Reported catch (KG)", 
                                   "CPUE (KG/hour)")
      }

      monthlyCPUE$Month <- format(monthlyCPUE$Month, "%b-%Y")
      write.csv(monthlyCPUE, file, row.names = FALSE)
    }
  )
})




# stationCPUE <- aggregate(newx[c("KG", "hours")], by = list(newx$Month, newx$station), sum, drop = FALSE)
# colnames(stationCPUE)[1:2] <- c("Date", "station")
# stationCPUE$station <- as.factor(stationCPUE$station)
# stationCPUE$Date <- as.Date(as.character(stationCPUE$Date)) ## factor not valid for plotting
# stationCPUE$CPUE <- stationCPUE$KG/stationCPUE$hours
# stationCPUE$KG[is.na(stationCPUE$KG)] <- 0
# stationCPUE$hours[is.na(stationCPUE$hours)] <- 0
# stationCPUE$CPUE[is.na(stationCPUE$CPUE)] <- 0


# habitatCPUE <- aggregate(newx[c("KG", "hours")], by = list(newx$Month, newx$habitat), sum, drop = FALSE)
# colnames(habitatCPUE)[1:2] <- c("Date", "habitat")
# habitatCPUE$Date <- as.Date(as.character(habitatCPUE$Date)) ## factor not valid for plotting
# habitatCPUE$CPUE <- habitatCPUE$KG/habitatCPUE$hours
# habitatCPUE$KG[is.na(habitatCPUE$KG)] <- 0
# habitatCPUE$hours[is.na(habitatCPUE$hours)] <- 0
# habitatCPUE$CPUE[is.na(habitatCPUE$CPUE)] <- 0
# 
# gearCPUE <- aggregate(newx[c("KG", "hours")], by = list(newx$Month, newx$gear), sum, drop = FALSE)
# colnames(gearCPUE)[1:2] <- c("Date", "gear")
# gearCPUE$Date <- as.Date(as.character(gearCPUE$Date)) ## factor not valid for plotting
# gearCPUE$CPUE <- gearCPUE$KG/gearCPUE$hours
# gearCPUE$KG[is.na(gearCPUE$KG)] <- 0
# gearCPUE$hours[is.na(gearCPUE$hours)] <- 0
# gearCPUE$CPUE[is.na(gearCPUE$CPUE)] <- 0
