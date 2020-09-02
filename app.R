library(shiny)
library(shinyWidgets)
library(labeling)
library(RMySQL)
library(leaflet)
library(lubridate)
library(ggplot2)
library(leaflet)
library(gridExtra)
library(cowplot)
library(RColorBrewer)
library(scales)
library(zoo)


startdate <- lubridate::floor_date(Sys.Date() - lubridate::years(1), unit = "month")
if(is.na(startdate)) startdate <- lubridate::floor_date(Sys.Date() - lubridate::days(1) - lubridate::years(1), unit = "month")


server <- function(input, output, session) {
  shp_df <- readRDS("data/shape.rds")
  kobo_trips <- readRDS("data/trips.rds")
  kobo_landings <- readRDS("data/landings.rds")
  pds_trips <- readRDS("data/PDS_trips.rds")
  pds_points <- readRDS("data/PDS_points.rds")
  
  #################################################################################
  # #### Nearest neighbor method validation test for data up until 31 July 2020 ####
  # kobo_trips <- kobo_trips[as.Date(kobo_trips$date) < as.Date("2020-07-31"), ]
  # kobo_landings <- kobo_landings[(kobo_landings$trip_id %in% kobo_trips$trip_id), ]
  # pds_trips <- pds_trips[as.Date(pds_trips$date) < as.Date("2020-07-31"), ]
  # linked_trips <- pds_trips[!is.na(pds_trips$trip_id), ]
  # nrow(linked_trips) #926
  # gear_success <- numeric(100L)
  # habitat_success <- numeric(100L)
  # set.seed(999)
  # for(i in 1:100){
  #   trainingtrips <- sample(seq_len(nrow(linked_trips)), size = round(nrow(linked_trips)*0.8))
  #   trainingdata <- linked_trips[trainingtrips, ]
  #   querydata <- linked_trips[!(seq_len(nrow(linked_trips)) %in% trainingtrips), ]
  #   nnobj2 <- RANN::nn2(trainingdata[, 13:32], querydata[, 13:32], k = 1)
  #   toofar <- nnobj2$nn.dists[, 1] > 0.2
  #   nohab <- !querydata$habitat_code %in% trainingdata$habitat_code
  #   nogear <- !querydata$gear_code %in% trainingdata$gear_code
  #   keeps <- !toofar &  !nohab & !nogear ## remove 3 records
  #   sum(keeps)
  #   length(keeps)
  #   querydata <- querydata[keeps, ]
  #   nnobj2$nn.dists <- nnobj2$nn.dists[keeps, , drop = FALSE]
  #   nnobj2$nn.idx <- nnobj2$nn.idx[keeps,  ,drop = FALSE]
  #   gear_success[i] <- sum(querydata$gear_code== trainingdata$gear_code[nnobj2$nn.idx[, 1]])/nrow(querydata) #0.815
  #   habitat_success[i] <- sum(querydata$habitat_code== trainingdata$habitat_code[nnobj2$nn.idx[, 1]])/nrow(querydata) #0.896
  # }
  # hist(gear_success)
  # hist(habitat_success)
  # mean(gear_success) # 0.8344741
  # sd(gear_success) # 0.02283898
  # mean(habitat_success) # 0.9194184
  # sd(habitat_success) # 0.0214137
  ###################################################################################
  
  max_date_numeric_kobo <- max(kobo_trips$date_numeric)
  max_date_numeric_pds <- max(pds_trips$date_numeric)
  
  dbcreds <- readRDS("data/dbcreds.rds")
  peskaDAT = RMySQL::dbConnect(RMySQL::MySQL(), user=dbcreds[1], password=dbcreds[2], 
                               dbname='wildrlab_peskaDB', host='johnny.heliohost.org', port=3306)
  

  
  munis <- RMySQL::dbReadTable(peskaDAT, "municipalities")
  stns <- RMySQL::dbReadTable(peskaDAT, "stations")
  spcs <-  RMySQL::dbReadTable(peskaDAT, "species")
  
  p1 <- paste0('SELECT * FROM trips WHERE date_numeric > ', max_date_numeric_kobo)
  p2 <- paste0('SELECT * FROM landings WHERE trip_id IN ( SELECT trip_id FROM trips WHERE date_numeric > ', 
               max_date_numeric_kobo, ' )')  
  
  q1 <- paste0('SELECT * FROM PDS_trips WHERE date_numeric > ', max_date_numeric_pds)
  q2 <- paste0('SELECT * FROM PDS_points WHERE PDS_trip IN ( SELECT PDS_trip FROM PDS_trips WHERE date_numeric > ', 
               max_date_numeric_pds, ' )')
  
  kobotrps <- DBI::dbGetQuery(peskaDAT, p1)
  kobolndgs <- DBI::dbGetQuery(peskaDAT, p2)
  
  pdstrps <- DBI::dbGetQuery(peskaDAT, q1)
  pdspts <- DBI::dbGetQuery(peskaDAT, q2)



  
  trps <- rbind(kobo_trips, kobotrps)
  lndgs <- rbind(kobo_landings, kobolndgs)
  pds_trips <- rbind(pds_trips, pdstrps)
  pds_points <- rbind(pds_points, pdspts)
  
  trps <- trps[!duplicated(trps), ]
  lndgs <- lndgs[!duplicated(lndgs), ]
  pds_trips <- pds_trips[!duplicated(pds_trips), ]
  pds_points <- pds_points[!duplicated(pds_points), ]
  
  
  
  ############ EXPORT UPDATED DATASETS TO /data REGULARLY TO SPEED THINGS UP #####################
  # saveRDS(trps, file = "data/trips.rds")
  # saveRDS(lndgs, file = "data/landings.rds")
  # saveRDS(pds_trips, file = "data/PDS_trips.rds")
  # saveRDS(pds_points, file = "data/PDS_points.rds")
  # 
  # ## also create backups
  # tabs <- RMySQL::dbListTables(peskaDAT)
  # sysdate <- Sys.Date()
  # dir.create(paste0("~/Dropbox/East_Timor/Worldfish/peskAAS/backups/", sysdate))
  # for(i in tabs){
  #   tmp <- RMySQL::dbReadTable(peskaDAT, i)
  #   saveRDS(tmp, file = paste0("~/Dropbox/East_Timor/Worldfish/peskAAS/backups/", sysdate, "/", i, ".rds"))
  # }
  ########################################################################################
  
  RMySQL::dbDisconnect(peskaDAT)
  
  
  pds_trips$date <- as.Date(pds_trips$date)
  # pds_trips$gear <- match(pds_trips$gear, c("GN", "HL", "LL", "SG", "CN", "MC", "BS", "SN", "TP"))
  pds_trips$habitat_code[pds_trips$habitat_code == 5L] <- 2L # lump trad fad in with normal fad
  pds_trips$habitat_code[pds_trips$habitat_code == 6L] <- 5L # shift others back 1 to enable matching
  pds_trips$habitat_code[pds_trips$habitat_code == 7L] <- 6L
  discards <- is.na(pds_trips$boat_code) # logical
  discard_trips <- pds_trips$PDS_trip[discards] #integer
  pds_trips <- pds_trips[!discards, ]
  pds_points <- pds_points[!(pds_points$PDS_trip %in% discard_trips), ]
  
  ## apply max latitude filter
  discard_points <- grepl("^-8.11", pds_points$latlng)
  discard_trips <- unique(pds_points$PDS_trip[discard_points])
  pds_trips <- pds_trips[!(pds_trips$PDS_trip %in% discard_trips), ]
  pds_points <- pds_points[!(pds_points$PDS_trip %in% discard_trips), ]
  
  # trps$gear_code <- match(trps$gear_code, c("GN", "HL", "LL", "SG", "CN", "MC", "BS", "SN", "TP"))
  
  # convert stations to municipalities
  trps$station_code <- stns$municipality_code[match(trps$station_code, stns$station_code)]
  
  # filter by trip duration/effort
  trps$trip_hours[trps$trip_hours == 0] <- 3
  trps$trip_hours[trps$trip_hours > 72] <- 72
  trps$trip_effort[trps$trip_effort == 0] <- 3
  trps$trip_effort[trps$trip_effort > 144] <- 144
  
  trps$habitat_code[trps$habitat_code == 5L] <- 2L # lump trad fad in with normal fad
  trps$habitat_code[trps$habitat_code == 6L] <- 5L # shift others back 1 to enable matching
  trps$habitat_code[trps$habitat_code == 7L] <- 6L
  ## gear match must be before discard step
  discards <- lndgs$trip_id[is.na(lndgs$species_code) | is.na(lndgs$length) | 
                              is.na(lndgs$nfish) | is.na(lndgs$weight_g)| 
                              is.na(lndgs$flag_code)]
  trps <- trps[!(trps$trip_id %in% discards), ]
  lndgs <- lndgs[!(lndgs$trip_id %in% discards), ]
  
  #discards <- lndgs$trip_id[lndgs$nfish > 10000] ## trip ids
  discards <- lndgs$trip_id[!(lndgs$flag_code %in% c(0L, 4L, 5L))]## trip ids
  trpnas <- is.na(trps$station_code) | is.na(trps$habitat_code) | is.na(trps$gear_code) | is.na(trps$trip_effort) #logical
  discards <- c(discards, trps$trip_id[trpnas])

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
  
  ## check for crazy CPUE values
  cpues <- trps$KG/trps$trip_effort
  discards <- trps$trip_id[cpues > 100]
  trps <- trps[!(trps$trip_id %in% discards), ]
  lndgs <- lndgs[!(lndgs$trip_id %in% discards), ]

  
  ## date landings tab (day not month)
  tmp <- structure(trps$date, names = trps$trip_id)
  lndgs$date <- tmp[lndgs$trip_id]
  sites <- c("Viqueque","Lautem","Manatuto","Liquica","Bobonaro","Covalima",
             "Manufahi","Ainaro","Baucau","Dili","Oe-Cusse", "Atauro")   
  habitats <- c("Reef/Ahu ruin","FAD/Rumpon","Deep/Tasi kle'an","Beach/Tasi ninin",
                "Mangrove/Aiparapa","Gleaning/Meti")
  gears <- c("Gillnet/Redi","Handline/Hakail","Longline/Hakail naruk","Spear/Kilat","Cast net/Dai",
             "Manual/Meti","Beach seine/Redi tasi ninin", "Seine net/Lampara","Trap/Bubur")
  boat_types <- c("Canoe", "Motor")
  
  ## temp fix for Joctan 2020-02-05
  trps$IMEI[trps$IMEI == 9376082 & trps$date_numeric > 17691L] <- "6738430"
  ##
  
  # trps <- trps[trps$date < as.Date("2020-07-31"), ]
  # lndgs <- lndgs[(lndgs$trip_id %in% trps$trip_id), ]
  # pds_trips <- pds_trips[pds_trips$date < as.Date("2020-07-31"), ]
  # 
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
    
    ######## for debugging only #########
    # inpt <- list(site = input$site, startDate = input$startDate, endDate = input$endDate,
    #              habitat = input$habitat, gear = input$gear, boat_type = input$boat_type)
    # saveRDS(inpt, file = "~/Desktop/input.rds")
    # input <- readRDS(file = "~/Desktop/input.rds")
    #####################################
    
    ## shape file
    shp_df2 <- shp_df
    shp_df2$colr[shp_df2$id %in% input$site] <- "cornflowerblue"
    ## select date range
    trps <- trps[trps$date > input$startDate & trps$date < input$endDate, ]
    pds_trips <- pds_trips[pds_trips$date > input$startDate & pds_trips$date < input$endDate, ]
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
    ## trps stations have been converted to munis but not pds_trips
    trps <- trps[trps$station_code %in% indices, ]
    indices <- stns$station_code[stns$municipality_code %in% indices]
    pds_trips <- pds_trips[pds_trips$station_code %in% indices, ]
    ## habitats
    indices <- match(input$habitat, habitats)
    if(length(indices) == 0) return(NULL)
    trps <- trps[trps$habitat_code %in% indices, ]
    pds_trips <- pds_trips[pds_trips$habitat_code %in% indices, ]
    allhabselected <- all(1:6 %in% indices)
    ## gear types
    indices <- match(input$gear, gears)
    if(length(indices) == 0) return(NULL)
    ## indices <- unlist(list(c(1, 5, 7:11), 2:3, 4, 6)[indices], use.names = FALSE)
    trps <- trps[trps$gear_code %in% indices, ]
    pds_trips <- pds_trips[pds_trips$gear_code %in% indices, ]
    allgearselected <- all(1:9 %in% indices)
    ## boat type
    indices <- match(input$boat_type, boat_types)
    if(length(indices) == 0) return(NULL)
    if(!(1L %in% indices)) munis$canoes <- 0
    if(!(2L %in% indices)) munis$motors <- 0
    trps <- trps[trps$boat_code %in% indices, ]
    pds_trips <- pds_trips[pds_trips$boat_code %in% indices, ]
    if(nrow(trps) < 30) return(NULL)
    ## purge landings table
    lndgs <- lndgs[lndgs$trip_id %in% trps$trip_id, ]
    pds_points <- pds_points[pds_points$PDS_trip %in% pds_trips$PDS_trip, ]
    ###########################################################
    ## aggregate species first since not dependent on trip hours
    mysums <- split(lndgs$KG, f = factor(lndgs$species_code, levels = unique(lndgs$species_code)), drop = FALSE)
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
    names(mysums) <- tmpnms[match(as.integer(names(mysums)), spcs$species_code)]
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
    canoe_trps <- trps[c("KG", "trip_effort", "month")][trps$boat_code == 1, ]
    if(nrow(canoe_trps) > 0 & allgearselected & allhabselected){
      monthlyCPUE_canoe <- aggregate(canoe_trps[c("KG", "trip_effort")], by = canoe_trps["month"], sum, drop = FALSE) 
      monthlyCPUE_canoe <- monthlyCPUE_canoe$KG/monthlyCPUE_canoe$trip_effort
      monthlyCPUE_canoe[is.na(monthlyCPUE_canoe)] <- 0
      natcatch_canoe <- monthlyCPUE_canoe * uepertrip_canoe * tripspermonth_canoe * ncanoes * 0.001
    }else{
      natcatch_canoe <- 0
    }
    ## calculate motor total catch
    motor_trps <- trps[c("KG", "trip_effort", "month")][trps$boat_code == 2, ]
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
    ######################################################################
    ## Make dataframe for station plot
    trps$ntrips <- 1L ## could scale dot size to ntrips (or just nhours)
    stationCPUE <- aggregate(trps[c("KG", "trip_effort")], by = trps[c("date", "station_code")], sum, drop = TRUE)
    ### remove stations with too few entries
    stationtab <- table(stationCPUE$station_code)
    discards <- as.integer(names(stationtab)[stationtab < 5])
    stationCPUE <- stationCPUE[!stationCPUE$station_code %in% discards, ]
    # stationCPUE$date <- as.Date(as.character(stationCPUE$Date)) ## factor not valid for plotting
    stationCPUE$CPUE <- stationCPUE$KG/stationCPUE$trip_effort
    stationCPUE$station_code <- as.factor(sites[stationCPUE$station_code])
    stationCPUE$fit <- stationCPUE$CPUE
    
    if(input$smoothen > 0){
      for(i in unique(stationCPUE$station_code)){
        l <- stationCPUE$station_code == i
        stationCPUE$fit[l] <- smooth.spline(as.integer(stationCPUE$date)[l], stationCPUE$CPUE[l], 
                                            w = stationCPUE$trip_effort[l], spar = input$smoothen)$y
      }
    }
    stationCPUE$fit[stationCPUE$fit < 0] <- 0
    ######################################################################
    ## Make dataframe for habitat plot
    habitatCPUE <- aggregate(trps[c("KG", "trip_effort")], by = trps[c("date", "habitat_code")], sum, drop = TRUE)
    ### remove stations with too few entries
    habitattab <- table(habitatCPUE$habitat_code)
    discards <- as.integer(names(habitattab)[habitattab < 5])
    habitatCPUE <- habitatCPUE[!habitatCPUE$habitat_code %in% discards, ]
    habitatCPUE$CPUE <- habitatCPUE$KG/habitatCPUE$trip_effort
    habitatCPUE$habitat_code <- as.factor(habitats[habitatCPUE$habitat_code])
    habitatCPUE$fit <- habitatCPUE$CPUE
    
    if(input$smoothen > 0){
      for(i in unique(habitatCPUE$habitat_code)){
        l <- habitatCPUE$habitat_code == i
        habitatCPUE$fit[l] <- smooth.spline(as.integer(habitatCPUE$date)[l], habitatCPUE$CPUE[l], 
                                            w = habitatCPUE$trip_effort[l], spar = input$smoothen)$y
      }
    }
    habitatCPUE$fit[habitatCPUE$fit < 0] <- 0
    ######################################################################
    ## Make dataframe for habitat plot
    gearCPUE <- aggregate(trps[c("KG", "trip_effort")], by = trps[c("date", "gear_code")], sum, drop = TRUE)
    geartab <- table(gearCPUE$gear_code)
    discards <- as.integer(names(geartab)[geartab < 5])
    gearCPUE <- gearCPUE[!gearCPUE$gear_code %in% discards, ]
    gearCPUE$CPUE <- gearCPUE$KG/gearCPUE$trip_effort
    gearCPUE$gear_code <- as.factor(gears[gearCPUE$gear_code])
    gearCPUE$fit <- gearCPUE$CPUE
    
    if(input$smoothen > 0){
      for(i in unique(gearCPUE$gear_code)){
        l <- gearCPUE$gear_code == i
        gearCPUE$fit[l] <- smooth.spline(as.integer(gearCPUE$date)[l], gearCPUE$CPUE[l], 
                                         w = gearCPUE$trip_effort[l], spar = input$smoothen)$y
      }
    }
    gearCPUE$fit[gearCPUE$fit < 0] <- 0
    ######################################################################
    ## Make heat table for map
    bring_heat <- function(tomas){
      if(nrow(tomas) == 0) return(NULL)
      latlng <- split(tomas$latlng, f = tomas$latlng)
      tmp <- strsplit(vapply(latlng, "[", "", 1), split = ",")
      out <- data.frame(Lat = as.numeric(vapply(tmp, "[", "", 1, USE.NAMES = FALSE)),
                        Lng = as.numeric(vapply(tmp, "[", "", 2, USE.NAMES = FALSE)),
                        counts = vapply(latlng, length, 0L, USE.NAMES = FALSE))
      return(out)
    }
    heat <- bring_heat(pds_points)
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
    m <- leaflet()
    #m <- addTiles(m)
    m <- setView(m, lng = 126, lat = -8.6, zoom = 9)
    m <- addMapPane(m, "background_map", zIndex = 410) 
    m <- addMapPane(m, "polygons", zIndex = 420)
    m <- addMapPane(m, "labels", zIndex = 430)
    m <- addProviderTiles(m, providers$Esri.WorldImagery, options = pathOptions(pane = "background_map"))
    if(!is.null(heat)){
      heat$counts <- round(log(heat$counts) + 1)
      pal <- colorBin("YlOrRd", domain = seq(0, max(heat$counts)))
      m <- addRectangles(m, lng1=heat$Lng - 0.002, lat1=heat$Lat - 0.002,
                         lng2=heat$Lng + 0.002, lat2=heat$Lat + 0.002,
                         stroke = FALSE, fillOpacity = 1, color = pal(heat$counts),
                         options = pathOptions(pane = "polygons"))
    }
    
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
            #panel.border=element_rect(linetype = "solid", color = "black", size=1),
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
      draw_plot(map, x = 0.1, y = 0.6, width = .6,  height = .3)
    
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
    ggplot(data = stationCPUE, aes(x = date, y = fit, colour = station_code, size = trip_effort)) +
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
    ggplot(data = habitatCPUE, aes(x = date, y = fit, colour = habitat_code, size = trip_effort)) +
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
    ggplot(data = gearCPUE, aes(x = date, y = fit, colour = gear_code, size = trip_effort)) +
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
    brow <- as.data.frame(c(as.list("Total"), as.list(totals), as.list(totals[4]/totals[3]), as.list(natcatch)))
    colnames(brow) <- colnames(monthlyCPUE)
    monthlyCPUE <- rbind(monthlyCPUE, brow)
    for(i in c(2:5, 7)) monthlyCPUE[[i]] <- as.integer(monthlyCPUE[[i]])
    monthlyCPUE[6] <- round(monthlyCPUE[6], 2)
    ## Only print national catch ests if all gears and habs selected
    if(!(allgearselected & allhabselected)) monthlyCPUE <- monthlyCPUE[1:6]
    monthlyCPUE
  })
  
  output$summaryTab <- downloadHandler(
    filename = "peskador.csv",
    content = function(file){
      obj <- datasetInput()
      monthlyCPUE <- obj$monthlyCPUE
      allgearselected <- obj$allgearselected
      allhabselected <- obj$allhabselected
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
}




ui <- fluidPage(
  tags$head(includeScript("google-analytics.js")),
  includeCSS("www/bootstrap.css"),
  theme = "bootstrap.css",
  shinyWidgets::setBackgroundImage(src = "rect.png"),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  # ),
  titlePanel(h1("PeskAAS")),
  titlePanel(h4("Automated Analytics System for Small Scale Fisheries in Timor-Leste", 
                style="opacity:0.8; text-decoration: overline; line-height: 1.0;")),
  titlePanel(h4("nothing", style="opacity:0;")),
  
  fluidRow(
    column(1, #style = "background-color:gainsboro;",
           br(),
           dateInput('startDate',
                     label = "Start date",
                     value = startdate,
                     min = as.Date("2016-09-01"), max = lubridate::floor_date(Sys.Date(), unit = "day"),
                     format = "dd-M-yyyy",
                     startview = "year", language = "en-AU"#, weekstart = 1
           ),
           br(),
           checkboxGroupInput("site", label = "Location",
                              choices = c("Viqueque","Lautem","Manatuto","Liquica",
                                          "Bobonaro","Covalima", "Manufahi","Ainaro",
                                          "Baucau","Dili","Oe-Cusse", "Atauro"), 
                              selected = c("Viqueque","Lautem","Manatuto","Liquica",
                                           "Bobonaro","Covalima", "Manufahi","Ainaro",
                                           "Baucau","Dili","Oe-Cusse", "Atauro")),
           actionLink("selectall_site", "select/deselect all"), 
           br(),
           br(),
           checkboxGroupInput("boat_type", label = "Boat type",
                              choices = c("Canoe", "Motor"), 
                              selected = c("Canoe", "Motor")),
           actionLink("selectall_boat_type", "select/deselect all"), 
           br()
    ),
    column(2, #style = "background-color:gainsboro;",
           br(),
           dateInput('endDate',
                     label = "End date",
                     value = lubridate::floor_date(Sys.Date(), unit = "month") - lubridate::days(1),
                     min = as.Date("2016-09-01"), max = lubridate::floor_date(Sys.Date(), unit = "day"),
                     format = "dd-M-yyyy", width = "50%",
                     startview = "year", language = "en-AU"#, weekstart = 1
           ),
           br(),
           checkboxGroupInput("habitat", label = "Habitat", # note traditional fad removed 20190830
                              choices = c("Reef/Ahu ruin","FAD/Rumpon","Deep/Tasi kle'an","Beach/Tasi ninin",
                                          "Mangrove/Aiparapa","Gleaning/Meti"),
                              selected = c("Reef/Ahu ruin","FAD/Rumpon","Deep/Tasi kle'an","Beach/Tasi ninin",
                                           "Mangrove/Aiparapa","Gleaning/Meti")),
           actionLink("selectall_habitat", "select/deselect all"), 
           br(),
           br(),
           checkboxGroupInput("gear", label = "Gear type",
                              choices = c("Gillnet/Redi","Handline/Hakail",
                                          "Longline/Hakail naruk", "Spear/Kilat",
                                          "Cast net/Dai","Manual/Meti",
                                          "Beach seine/Redi tasi ninin",
                                          "Seine net/Lampara","Trap/Bubur"), 
                              selected = c("Gillnet/Redi","Handline/Hakail",
                                           "Longline/Hakail naruk","Spear/Kilat",
                                           "Cast net/Dai","Manual/Meti",
                                           "Beach seine/Redi tasi ninin",
                                           "Seine net/Lampara","Trap/Bubur")),
           actionLink("selectall_gear", "select/deselect all"), 
           br()
    ),
    column(9, 
           mainPanel(
             tabsetPanel(
               tabPanel("Tracked activity", br(), leafletOutput("plot0")),
               tabPanel("CPUE by Month", br(), plotOutput("plot1")),
               tabPanel("CPUE by site", br(), plotOutput("plot2")),
               tabPanel("CPUE by habitat", br(), plotOutput("plot3")),
               tabPanel("CPUE by gear", br(), plotOutput("plot4")),
               tabPanel("Catch by species", br(), plotOutput("plot5")),
               tabPanel("Summary", br(), tableOutput("table1"))
             ),
             width = 12
           )
    )
  ),
  fluidRow(
    column(1, 
           sliderInput("smoothen", label = "Smoothness", min = 0, max = 1, value = 0),
           br(),
           br()
    ),
    column(1, 
           br(),
           br(),
           br(),
           downloadButton("summaryTab", label = "Download (csv)")
    ),
    column(3, 
           br(),
           br(),
           br()
    ),
    column(7, 
           br(),
           img(src = "logos.png", height = 50, width = 500),
           br()
    )
    
  )
)


shinyApp(ui = ui, server = server)