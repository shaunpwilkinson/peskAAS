################################################################################
library(shiny)
library(RMySQL)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(zoo)

shinyServer(function(input, output, session){
  # tmpf <- tempfile(fileext = ".csv")
  # test <- download.file(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vSO-",
  #                              "ivIULOs5WOVwI1A0n-QPkgLiQ5bG8oTy4_NJJGQbqU15dVA5f0",
  #                              "oFXa6E7Op43dZz4j5T4-lWUTY/pub?gid=2135385965&single=",
  #                              "true&output=csv"), destfile = tmpf, quiet = TRUE)
  # if(test != 0) stop("Error 1")
  # x <- read.csv(tmpf, stringsAsFactors = FALSE)
  # peskaDAT = RMySQL::dbConnect(RMySQL::MySQL(), user='sql12291139', password='R1fDe1QDB6', 
  #                              dbname='sql12291139', host='sql12.freemysqlhosting.net', 
  #                              port=3306)
  peskaDAT = RMySQL::dbConnect(RMySQL::MySQL(), user='wilko_sql1229113', password='R1fDe1QDB6', 
                               dbname='wilko_peskaDAT', host='johnny.heliohost.org', port=3306)
  
  lndgs <- RMySQL::dbReadTable(peskaDAT, "landings")
  trps <- RMySQL::dbReadTable(peskaDAT, "trips")
  munis <- RMySQL::dbReadTable(peskaDAT, "municipalities")
  stns <- RMySQL::dbReadTable(peskaDAT, "stations")
  spcs <-  RMySQL::dbReadTable(peskaDAT, "species")
  dbDisconnect(peskaDAT)
  x <- merge(lndgs, trps, by = "trip_id")
  
  # x <- readRDS("data/backup-2019-04-03.rds")
  x$date <- as.Date(x$date)
  keeps <- x$flag.x %in% c(0L, 4L, 5L) ## weight and length discrepancies
  x <- x[keeps, ] 
  # remove all rows whose parent trips contain NAs
  x$gear <- match(x$gear, c("GN", "HL", "LL", "SG", "CN", "MC", "BS", "SN", "TP"))
  discards <- is.na(x$station) | is.na(x$habitat) | is.na(x$gear)| 
    is.na(x$weight_g) | is.na(x$trip_effort) #logical
  ## must remove entire trip's data
  discards <- x$trip_id[discards] #indices
  discards <- x$trip_id %in% discards #logical
  x <- x[!discards, ] 
  discards <- x$date >= lubridate::floor_date(Sys.Date(), unit = "day")
  x <- x[!discards, ]
  x <- x[x$date > as.Date("2016-09-01"), ]

  # sites <- c("Adara", "Beloi", "Biqueli", "Vemasse", "Adarai",
  #            "Uaroana", "Com", "Tutuala", "Ililai", "Beacou",
  #            "Tolurika","Atekru","Berao","Iliana",
  #            "Fatu'u","Doru","Raiketa","Maquer","Lore")
  
  sites <- c("Viqueque","Lautem","Manatuto","Liquica","Bobonaro","Covalima",
             "Manufahi","Ainaro","Baucau","Dili","Oe-Cusse", "Atauro")   
  # municipios <- c(10, 10, 10, 9, 1, 10, 2, 2, 2, 5, rep(10,8), 2)
  # municipios <- munis$municipio
  
  habitats <- c("Reef/Ahu ruin","FAD/Rompun","Deep/Tasi kle'an","Beach/Tasi ninin",
            "Traditional FAD/Rompun bamboo","Mangrove/Aiparapa","Gleaning/Meti")
  
  gears <- c("Gillnet/Redi","Handline/Hakail","Longline/Hakail naruk","Spear/Kilat","Cast net/Dai",
             "Manual/Meti","Beach seine/Redi tasi ninin", "Seine net/Lampara","Trap/Bubur")
  boat_types <- c("Canoe", "Motor")
  
  ## number of boats by municipio
  #totdata <- readRDS("data/totdata.rds")
  # td <- cbind(munis[1], munis[4], apply(munis[5:7], 1, sum)) 
  # colnames(td) <- c("municipio", "ncanoes", "nmotors")
  # rownames(td) <- NULL
  ## convert to stations

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
    x <- x[x$date > input$dateRange[1] & x$date < input$dateRange[2], ]
    x$date <- as.character(x$date)
    x$KG <- x$weight_g/1000
    ## hours is really manhours
    x <- x[c("trip_id", "date", "species", "rel_effort", "trip_effort", "station", "habitat", "gear", "boat_type", "KG")]
    colnames(x) <- c("ID","Date", "species", "pob", "hours", "station", "habitat", "gear", "boat_type", "KG")
    ## get rid eventually
    x$Month <- lubridate::floor_date(as.Date(x$Date), unit = "month")
    x$Month <- as.factor(x$Month)
    x$station <- stns$municipio[match(x$station, stns$station)]

    indices <- match(input$site, sites)
    munis <- munis[indices, ]
    if(length(indices) == 0) return(NULL)
    #indices[indices == 19] <- 20
    x <- x[x$station %in% indices, ]
    indices <- match(input$habitat, habitats)
    if(length(indices) == 0) return(NULL)
    x <- x[x$habitat %in% indices, ]
    indices <- match(input$gear, gears)
    if(length(indices) == 0) return(NULL)
    indices <- unlist(list(c(1, 5, 7:11), 2:3, 4, 6)[indices], use.names = FALSE)
    x <- x[x$gear %in% indices, ]
    indices <- match(input$boat_type, boat_types)
    if(length(indices) == 0) return(NULL)
    if(!(1L %in% indices)) munis$canoes <- 0
    if(!(2L %in% indices)) munis$motors <- 0
    x <- x[x$boat_type %in% indices, ]
    if(nrow(x) < 30) return(NULL)

    ## aggregate species first since not dependent on trip hours
    ## find national totals by month
    ###########################################################
    sppKG <- aggregate(x["KG"], by = list(x$species), sum)
    colnames(sppKG)[1] <- "species"
    sppKG <- sppKG[order(sppKG$KG, decreasing = TRUE), ]
    if(nrow(sppKG) > 10){
      others <- sum(sppKG[seq(11, nrow(sppKG)), 2])
      sppKG <- sppKG[1:10, ]
      tmpnms <- paste0(spcs$category, " (", spcs$category_tetun, ")")
      sppKG$species <- tmpnms[match(sppKG$species, spcs$species)]
      OTHmatch <- grep("^Other", sppKG$species)
      if(length(OTHmatch) > 0){
        others <- others + sppKG$KG[OTHmatch[1]]
        sppKG <- sppKG[-(OTHmatch[1]), ]
      }
      UNKmatch <- grep("^Unknown", sppKG$species)
      if(length(UNKmatch) > 0){
        others <- others + sppKG$KG[UNKmatch[1]]
        sppKG <- sppKG[-(UNKmatch[1]), ]
      }
      aggrow <- data.frame(species = "other", KG = others, stringsAsFactors = FALSE)
      sppKG <- rbind(sppKG, aggrow)
    }
    sppKG$species <- factor(sppKG$species, levels = sppKG$species)
    sppKG$Tonnes <- sppKG$KG/1000
    ######################################################################
    newx <- x
    ## now need to aggregate trips
    tmp <- aggregate(newx[c("Date", "pob", "hours", "station", "habitat", "gear", "boat_type", "Month")], 
                     by = list(newx$ID), "[", 1) 
    colnames(tmp)[1] <- "ID"
    tmp$KG <- aggregate(newx["KG"], by = list(newx$ID), sum)[, 2]
    tmp$hours[tmp$hours == 0] <- 3
    
    newx <- tmp

    monthlyCPUE <- aggregate(newx[c("KG", "hours")], by = list(newx$Month), sum, drop = FALSE) 
    colnames(monthlyCPUE)[1] <- "Date"
    monthlyCPUE$Date <- as.Date(as.character(monthlyCPUE$Date)) ## factor not valid for plotting
    monthlyCPUE$CPUE <- monthlyCPUE$KG/monthlyCPUE$hours
    monthlyCPUE$trips <- table(newx$Month)
    f <- function(d) length(unique(d))
    monthlyCPUE$fishing_days <- aggregate(newx$Date, by = list(newx$Month), f, drop = FALSE)[[2]]
    monthlyCPUE$KG[is.na(monthlyCPUE$KG)] <- 0
    monthlyCPUE$hours[is.na(monthlyCPUE$hours)] <- 0
    monthlyCPUE$CPUE[is.na(monthlyCPUE$CPUE)] <- 0
    monthlyCPUE$fishing_days[is.na(monthlyCPUE$fishing_days)] <- 0
    monthlyCPUE <- monthlyCPUE[c("Date", "fishing_days", "trips", "hours", "KG", "CPUE")]
    ## calculate national catch
    uepertrip_canoe <- 4 #4
    uepertrip_motor <- 10
    uepertrip_shore <- 3
    tripspermonth_canoe <- 8.2
    tripspermonth_motor <- 15.3
    ncanoes <- sum(munis$canoes)
    nmotors <- sum(munis$motors)
    ## calculate canoe total catch
    tmp_canoe <- tmp[c("KG", "hours")][tmp$boat_type == 1, ]
    if(nrow(tmp_canoe) > 0){
      monthlyCPUE_canoe <- aggregate(tmp_canoe, by = list(tmp$Month[tmp$boat_type == 1]), sum, drop = FALSE) 
      monthlyCPUE_canoe <- monthlyCPUE_canoe$KG/monthlyCPUE_canoe$hours
      monthlyCPUE_canoe[is.na(monthlyCPUE_canoe)] <- 0
      natcatch_canoe <- monthlyCPUE_canoe * uepertrip_canoe * tripspermonth_canoe * ncanoes * 0.001
    }else{
      natcatch_canoe <- 0
    }

    ## calculate motor total catch
    tmp_motor <- tmp[c("KG", "hours")][tmp$boat_type == 2, ]
    if(nrow(tmp_motor) > 0){
      monthlyCPUE_motor <- aggregate(tmp_motor, by = list(tmp$Month[tmp$boat_type == 2]), sum, drop = FALSE) 
      monthlyCPUE_motor <- monthlyCPUE_motor$KG/monthlyCPUE_motor$hours
      monthlyCPUE_motor[is.na(monthlyCPUE_motor)] <- 0
      natcatch_motor <- monthlyCPUE_motor * uepertrip_motor * tripspermonth_motor * nmotors * 0.001
    }else{
      natcatch_motor <- 0
    }
    monthlyCPUE$natcatch <- natcatch_canoe + natcatch_motor
    
    stationCPUE <- aggregate(newx[c("KG", "hours")], by = list(newx$Date, newx$station), sum, drop = TRUE)
    colnames(stationCPUE)[1:2] <- c("Date", "station")
    stationtab <- table(stationCPUE$station)
    discards <- as.integer(names(stationtab)[stationtab < 5])
    stationCPUE <- stationCPUE[!stationCPUE$station %in% discards, ]
    stationCPUE$Date <- as.Date(as.character(stationCPUE$Date)) ## factor not valid for plotting
    stationCPUE$CPUE <- stationCPUE$KG/stationCPUE$hours
    stationCPUE$station <- as.factor(sites[stationCPUE$station])
    stationCPUE$fit <- stationCPUE$CPUE
    if(input$smoothen > 0){
      for(i in unique(stationCPUE$station)){
        mylogi <- stationCPUE$station == i
        stationCPUE$fit[mylogi] <- smooth.spline(as.integer(stationCPUE$Date)[mylogi], 
                                                 stationCPUE$CPUE[mylogi], spar = input$smoothen)$y
      }
    }
    stationCPUE$fit[stationCPUE$fit < 0] <- 0
    
    
    #####newx$habitat[newx$habitat == 5] <- 2 ## merge traditional fad into fad for now
    habitatCPUE <- aggregate(newx[c("KG", "hours")], by = list(newx$Date, newx$habitat), sum, drop = TRUE)
    colnames(habitatCPUE)[1:2] <- c("Date", "habitat")
    habitattab <- table(habitatCPUE$habitat)
    discards <- as.integer(names(habitattab)[habitattab < 5])
    habitatCPUE <- habitatCPUE[!habitatCPUE$habitat %in% discards, ]
    habitatCPUE$Date <- as.Date(as.character(habitatCPUE$Date)) ## factor not valid for plotting
    habitatCPUE$CPUE <- habitatCPUE$KG/habitatCPUE$hours
    habitatCPUE$habitat <- as.factor(habitats[habitatCPUE$habitat])
    habitatCPUE$fit <- habitatCPUE$CPUE
    if(input$smoothen > 0){
      for(i in unique(habitatCPUE$habitat)){
        mylogi <- habitatCPUE$habitat == i
        habitatCPUE$fit[mylogi] <- smooth.spline(as.integer(habitatCPUE$Date)[mylogi], habitatCPUE$CPUE[mylogi], spar = input$smoothen)$y
      }
    }
    habitatCPUE$fit[habitatCPUE$fit < 0] <- 0
    
    
    gearCPUE <- aggregate(newx[c("KG", "hours")], by = list(newx$Date, newx$gear), sum, drop = TRUE)
    colnames(gearCPUE)[1:2] <- c("Date", "gear")
    geartab <- table(gearCPUE$gear)
    discards <- as.integer(names(geartab)[geartab < 5])
    gearCPUE <- gearCPUE[!gearCPUE$gear %in% discards, ]
    gearCPUE$Date <- as.Date(as.character(gearCPUE$Date)) ## factor not valid for plotting
    gearCPUE$CPUE <- gearCPUE$KG/gearCPUE$hours
    gearCPUE$gear <- as.factor(gears[gearCPUE$gear])
    gearCPUE$fit <- gearCPUE$CPUE
    if(input$smoothen > 0){
      for(i in unique(gearCPUE$gear)){
        mylogi <- gearCPUE$gear == i
        gearCPUE$fit[mylogi] <- smooth.spline(as.integer(gearCPUE$Date)[mylogi], 
                                              gearCPUE$CPUE[mylogi], spar = input$smoothen)$y
      }
    }
    gearCPUE$fit[gearCPUE$fit < 0] <- 0


    return(list(monthlyCPUE = monthlyCPUE, stationCPUE = stationCPUE, 
                habitatCPUE = habitatCPUE, gearCPUE = gearCPUE, sppKG = sppKG))
  })
  output$plot1 <- renderPlot({
    CPUE <- datasetInput()
    monthlyCPUE <- CPUE$monthlyCPUE
    #allCPUE <- CPUE$allCPUE
    if(is.null(monthlyCPUE)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    #mylow <- lowess(allCPUE$Date, allCPUE$CPUE, f = 0.1)
    ggplot() +
      geom_col(aes(Date, CPUE), monthlyCPUE, fill = "cornflowerblue") + 
      #geom_line(aes(as.Date(mylow$x), mylow$y)) +
      xlab("") +
      ylab("CPUE (kg/hour)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
  })
  
  output$plot2 <- renderPlot({
    CPUE <- datasetInput()
    stationCPUE <- CPUE$stationCPUE
    if(is.null(stationCPUE)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    
    #ggplot(data = stationCPUE, aes(x = Date, y = CPUE, colour = station)) +
    ggplot(data = stationCPUE, aes(x = Date, y = fit, colour = station)) +
      geom_point() +
      xlab("") +
      ylab("CPUE (kg/hour)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
  })
  
  output$plot3 <- renderPlot({
    CPUE <- datasetInput()
    habitatCPUE <- CPUE$habitatCPUE
    if(is.null(habitatCPUE)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    
    ggplot(data = habitatCPUE, aes(x = Date, y = fit, colour = habitat)) +
      geom_point() +
      xlab("") +
      ylab("CPUE (kg/hour)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
    
    # habitatCPUE$habitat <- as.factor(habitats[habitatCPUE$habitat])
    # ggplot(data = habitatCPUE,
    #        aes(x = Date, y = CPUE, colour = habitat)) +
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
    CPUE <- datasetInput()
    gearCPUE <- CPUE$gearCPUE
    if(is.null(gearCPUE)){
      plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
      legend("center", legend = "Insufficient data to plot\n", bty = "n")
      box()
      return(NULL)
    }
    
    ggplot(data = gearCPUE, aes(x = Date, y = fit, colour = gear)) +
      geom_point() +
      xlab("") +
      ylab("CPUE (kg/hour)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      scale_x_date(date_breaks = "1 month" , labels=date_format("%b-%Y"))
    
    # gearCPUE$gear <- as.factor(gears[gearCPUE$gear])
    # ggplot(data = gearCPUE,
    #        aes(x = Date, y = CPUE, colour = gear)) +
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
    CPUE <- datasetInput()
    sppKG <- CPUE$sppKG
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
    CPUE <- datasetInput()
    monthlyCPUE <- CPUE$monthlyCPUE
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
    monthlyCPUE
  })
  
  output$downloadData <- downloadHandler(
    filename = "peskador.csv",
    content = function(file) {
      CPUE <- datasetInput()
      monthlyCPUE <- CPUE$monthlyCPUE
      colnames(monthlyCPUE) <- c("Month", "Reported fishing days", "Reported trips",
                                 "Reported effort (hours)", "Reported catch (KG)", 
                                 "CPUE (KG/hour)", "Estimated national catch (T)")
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
