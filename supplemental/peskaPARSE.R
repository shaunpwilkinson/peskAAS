### peskaPARSE.R script to download landings records from KOBO and append to googlesheet
### at daily intervals.

### Note: need text file with KOBO credentials and .httr-oauth file for googlesheet edit access
### stored in same folder as this script

### CRON job setup instructions:
### create bash script called 'peskaPARSE' as follows (without .sh on the end as cron may not like it)
### cd /etc/cron.d
### sudo nano peskaPARSE
### paste lines:
###   #!/bin/bash
###   wget https://www.dropbox.com/s/59rwnk7jgzqz85r/peskaPARSE.R?dl=1 -O /home/shaunpwilkinson_gmail_com/peskaPARSE/peskaPARSE.R
###   Rscript /home/shaunpwilkinson_gmail_com/peskaPARSE/peskaPARSE.R
###
### note newline above
### sudo nano /etc/crontab
### add line to crontab file:
### 17 6 * * * /etc/cron.daily/peskaPARSE
###
### note newline at end
### this would execute at 06:17 every morning
### can use `sudo service cron status` to check if working

## very important to add full path to directory!
## directory should contain KoBo-auth.txt and .httr-oauth files
## also make sure bash script contains full paths and newline at end
setwd("/home/shaunpwilkinson_gmail_com/Desktop/Worldfish/") 

if(!("readxl" %in% list.files(.libPaths()))) install.packages("readxl")
if(!("googlesheets" %in% list.files(.libPaths()))) install.packages("googlesheets")
if(!("httpuv" %in% list.files(.libPaths()))) install.packages("httpuv")
if(!("httr" %in% list.files(.libPaths()))) install.packages("httr")

print(Sys.Date())
message("Accessing tables in googledrive")
## note entire sheet doc must be published to the web (not just first sheet)
## in google sheet go File > publish to the web
pesksheet <- googlesheets::gs_key("13VGyuzLVaCU9vF8wNxwsAuz-SbIN3S6yRVvuJQUTK6I")
peskador <- googlesheets::gs_read(pesksheet, ws = 1, col_types = "iiDicicicdiiiididciicdiidid")
sp_pars <- googlesheets::gs_read(pesksheet, ws = 2, col_types = "iiccccccddddicdcccc")
message("Retrieving landings data from Kobo API")
id <-  135272 # new landings data
URL <- paste0("https://kc.humanitarianresponse.info/api/v1/data/", id, ".xlsx")
kobocreds <- scan("KoBo-auth.txt", what = "", sep = "\n")
kobocreds <- kobocreds[!grepl("^#", kobocreds)]
kobocreds <- kobocreds[grep(":", kobocreds)[1]]
stopifnot(length(kobocreds) == 1L)
kobocreds <- gsub(" ", "", kobocreds)
kobocreds <- strsplit(kobocreds, split = ":")[[1]][1:2]
stopifnot(length(kobocreds) == 2L)
z <- httr::GET(URL, httr::authenticate(kobocreds[1], kobocreds[2]))
httr::stop_for_status(z)
tmpz <- tempfile(fileext = ".xlsx")
filetag <- file(tmpz, open = "wb")
writeBin(httr::content(z, type = "raw"), filetag)
close(filetag)
message("Reading Kobo tables")
cty <- c("text", "text", "date", "text", "date", rep("text", 71))
trips <- readxl::read_excel(tmpz, sheet = 1, col_types = cty)
colnames(trips) <- gsub(".+/", "", colnames(trips))
colnames(trips) <- gsub("^_", "", colnames(trips)) #only version still has undersc
trips$index <- as.integer(trips$index)
trips$id <- as.integer(trips$id)
spp <- readxl::read_excel(tmpz, sheet = 2, col_types = "text")
colnames(spp) <- gsub(".+/", "", colnames(spp))
colnames(spp) <- gsub("^_", "", colnames(spp))
spp$parent_index <- as.integer(spp$parent_index)

trips <- trips[!trips$id %in% peskador$Trip_ID, ]

spp <- spp[spp$parent_index %in% trips$index, ]
dups <- duplicated(spp[, -match("index", colnames(spp))]) 
if(any(dups)){
  message("Duplicates found for trip(s) ", unique(spp[["parent_index"]][dups]))
  spp <- spp[!dups, ]
}
if(nrow(trips) == 0 | nrow(spp) == 0){
  message("No new rows to add")
  # return(peskador[0, ])
}else{
  message("Filtering tables & tidying columns")
  ## condense fisher name cols
  narandata <- as.matrix(trips[, 11:48])
  narandata[narandata == "other"] <- NA
  trips[[11]] <- apply(narandata, 1, function(r) r[match(FALSE, is.na(r))])
  trips <- trips[, -(12:48)]
  trips$Site_name[trips$Site_name == "LOR"] <- "20"
  trips$Site_name[trips$Site_name == "COM"] <- "7"
  trips$Site_name[!trips$Site_name %in% paste(1:30)] <- NA
  trips$Site_name <- as.integer(trips$Site_name)
  if(mode(trips$Site_name) == "character") warning("Site variable failed conversion to integer")
  trips$habitat_[!trips$habitat_ %in% paste(1:7)] <- NA # 7 is gleaning
  trips$habitat_ <- as.integer(trips$habitat_)
  if(mode(trips$habitat_) == "character") warning("Habitat variable failed conversion to integer")
  ## convert other cols to integers/numeric
  
  trips$gear_type[is.na(trips$gear_type)] <- "0"
  trips$gear_type[trips$gear_type == "1"] <- "GN"
  trips$gear_type[trips$gear_type == "2"] <- "HL"
  trips$gear_type[trips$gear_type == "3"] <- "LL"
  trips$gear_type[trips$gear_type == "4"] <- "SG"
  trips$gear_type[trips$gear_type == "5"] <- "CN"
  trips$gear_type[trips$gear_type == "6"] <- "MC"
  trips$gear_type[trips$gear_type == "7"] <- "BS"
  trips$gear_type[trips$gear_type == "8"] <- "SN"
  trips$gear_type[trips$gear_type == "9"] <- "TP"
  trips$gear_type[!trips$gear_type %in% c("GN", "HL", "LL", "SG", "CN", "MC", "BS", "SN", "TP")] <- NA_character_
  #trips$gear_type <- as.integer(trips$gear_type)
  
  trips$mesh_size[!trips$mesh_size %in% c("1", "1.25", "1.5", "2", "2.5", "3")] <- NA_character_
  suppressWarnings(trips$mesh_size <- as.numeric(trips$mesh_size)) #warning ok
  trips$mane_men_ <- as.integer(trips$mane_men_)
  trips$feto_women_ <- as.integer(trips$feto_women_)
  trips$labarik_children_ <- as.integer(trips$labarik_children_)
  trips$Happiness[!trips$Happiness %in% paste(1:5)] <- NA_character_
  trips$Happiness <- as.integer(trips$Happiness)
  trips$transport_type[!trips$transport_type %in% paste(1:3)] <- NA_character_
  trips$transport_type <- as.integer(trips$transport_type)
  ## coerce dates into correct format
  trips$TOTAL_ORAS_VIAGEM_PESKA <- as.numeric(trips$TOTAL_ORAS_VIAGEM_PESKA)
  trips$TOTAL_ORAS_VIAGEM_PESKA[trips$TOTAL_ORAS_VIAGEM_PESKA > 72] <- NA
  spp$Choose_Species[spp$Choose_Species == "zero_catch"] <- "0"
  suppressWarnings(spp$Choose_Species <- as.integer(spp$Choose_Species)) # warning ok
  spp$Choose_Species[is.na(spp$Choose_Species)] <- 999 # unidentified species code
  countcols <- grep("number_of_individuals", colnames(spp))
  stopifnot(length(countcols) == 11)
  colnames(spp)[countcols[1]] <- "number_of_individuals_1"
  
  natrips <- integer(0)
  for(i in 1:11){
    invalids <- grepl("[\\.,][123456789]+", spp[[countcols[i]]])
    natrips <- c(natrips, trips$id[match(spp$parent_index[invalids], trips$index)])
    if(any(invalids)) message("Repairing invalid counts: ",  spp[[countcols[i]]][invalids])
    spp[[countcols[i]]][invalids] <- gsub("[\\.,]", "", spp[[countcols[i]]][invalids])
    spp[[countcols[i]]] <- as.integer(spp[[countcols[i]]])
  }
  spp$presu <- as.numeric(spp$presu)
  spp$numeru_ikan <- as.integer(spp$numeru_ikan)
  spp$per_Kg_hira <- as.numeric(spp$per_Kg_hira)
  spp$number_of_individuals_0 <- NA_integer_
  spp$number_of_individuals_0[spp$Choose_Species == 0L] <- 0L 
  colnames(spp)[colnames(spp) == "IKAN_NARUK_LIU_60_cm_SUKAT_NO_"] <- "length"
  colnames(spp)[colnames(spp) == "NUMERU_IKAN_LIU_60_cm"] <- "nfish"
  spp$length <- as.numeric(spp$length)
  spp$nfish <- as.numeric(spp$nfish)
  for(i in 1:nrow(spp)){
    if(!is.na(spp$length[i])){
      if(spp$length[i] <= 60){
        spp$length[i] <- spp$nfish[i] <- NA
      }else if(is.na(spp$nfish[i])){
        spp$nfish[i] <- 1L
      }else if(spp$nfish[i] == spp$length[i]){
        spp$nfish[i] <- 1L
      }
    }else if(!is.na(spp$nfish[i])){
      spp$length[i] <- 62.5 ## round down to 60 cm?
    }
    if(spp$Choose_Species[i] == 0) spp[i, countcols] <- NA
  }
  numnas <- apply(spp[c("presu", "numeru_ikan")], 1, function(v) sum(is.na(v)))
  spp[c("presu", "numeru_ikan")][numnas == 1, ] <- NA
  r1l <- vector(mode = "list", length = 13)
  len <- 0
  for(i in 1:12){
    tmp <- spp[c("Choose_Species", "other_name", paste0("number_of_individuals_", i - 1),
                 "foodsale", "presu", "numeru_ikan", "parent_index")]
    colnames(tmp)[1:4] <- c("species", "note", "nfish", "food_sale")
    tmp$length <- len
    len <- len + if(i == 1) 7.5 else 5
    r1l[[i]] <- tmp[!is.na(tmp$nfish), ]
  }
  tmp <- spp[c("Choose_Species", "other_name", "nfish", "foodsale", "presu", 
               "numeru_ikan", "parent_index", "length")]
  colnames(tmp)[1:4] <- c("species", "note", "nfish", "food_sale")
  r1l[[13]] <- tmp[!is.na(tmp$nfish), ]
  r1 <- as.data.frame(do.call("rbind", r1l))
  if(nrow(r1) == 0){
    message("No new rows to add")
    #return(peskador[0, ])
  }else{
    r1$species[r1$species == 205] <- 91 # manual change to bullet tuna
    parinds <- match(r1$parent_index, trips$index)
    r1$Trip_ID <- trips$id[parinds]
    r1$date <- trips$today[parinds]  
    r1$station <- trips$Site_name[parinds]
    r1$fisher <- trips$peskador_naran_sa_fisher_name_[parinds]
    r1$flag <- 0L
    r1$gear <- trips$gear_type[parinds]
    r1$mesh <- trips$mesh_size[parinds]
    
    r1$men <- trips$mane_men_[parinds]
    r1$women <- trips$feto_women_[parinds]
    r1$children <- trips$labarik_children_[parinds]
    r1$rel_effort <- r1$men + r1$women + r1$children
    
    # r1$rel_effort <- trips$mane_men_[parinds] + trips$feto_women_[parinds] + 
    #   trips$labarik_children_[parinds]
    r1$rel_effort[r1$rel_effort < 1] <- 3
    r1$trip_hours <- trips$TOTAL_ORAS_VIAGEM_PESKA[parinds]
    r1$trip_hours[is.na(r1$trip_hours)] <- 3
    r1$trip_hours[r1$trip_hours < 1] <- 1
    r1$trip_hours[r1$trip_hours > 48] <- 48
    r1$trip_hours <- round(r1$trip_hours, 1)
    r1$site <- trips$fishing_location[parinds]
    r1$rank <- trips$Happiness[parinds]
    r1$all_boats <- trips$No_boats[parinds]
    r1$hab <- trips$habitat_[parinds]
    r1$btype <- trips$transport_type[parinds]
    sp_rows <- match(r1$species, sp_pars$Code)
    sp_rows[is.na(sp_rows)] <- match(999L, sp_pars$Code)
    tooshort <- r1$length > 0 & r1$length < sp_pars$minlength[sp_rows] * 0.75
    tooshort[is.na(tooshort)] <- FALSE
    r1$flag[tooshort] <- 1L
    toolong <- r1$length > sp_pars$maxlength[sp_rows] * 1.25
    toolong[is.na(toolong)] <- FALSE
    r1$flag[toolong] <- 2L
    get_weight <- function(a, b, l) a * l^b
    unitwgt <- mapply(get_weight, sp_pars$a[sp_rows], sp_pars$b[sp_rows],  r1$length)
    tooheavy <- unitwgt > sp_pars$maxweight[sp_rows] * 1.5
    tooheavy[is.na(tooheavy)] <- FALSE
    r1$flag[tooheavy & !toolong] <- 3L
    r1$weight_g <- round(r1$nfish * unitwgt)
    r1$weight_g[r1$species == 0] <- 0
    r1$price_per_kg <- round((r1$nfish/r1$weight_g) * (r1$presu/r1$numeru_ikan) * 1000, 1)
    r1$price_per_kg[is.nan(r1$price_per_kg)] <- NA
    toocheap <- r1$price_per_kg < 0.05
    toocheap[is.na(toocheap)] <- FALSE
    r1$flag[toocheap] <- 4L
    toodear <- r1$price_per_kg > 6
    toodear[is.na(toodear)] <- FALSE
    r1$flag[toodear] <- 5L
    r1$flag[r1$Trip_ID %in% natrips] <- 7L
    invalidcombo <- r1$hab %in% c(2, 5) & r1$gear == "BS"
    r1$flag[invalidcombo] <- 8L
    r1$trip_effort <- round(r1$trip_hours * r1$rel_effort, 1)
    r1 <- r1[order(r1$date, r1$Trip_ID), ]
    r1$dummy_row <- as.integer(!duplicated(r1$Trip_ID)) ## do this after sort
    lastrecID <- tail(peskador$rec_id, 1)
    r1$rec_id <- seq(lastrecID + 1, lastrecID + nrow(r1))
    r1 <- r1[!is.na(r1$Trip_ID), ]
    r1$site[nchar(r1$site) > 50] <- NA
    outcols <- colnames(peskador)
    out <- r1[outcols]
    message("Appending ", nrow(out), " new records to peskaDAT google sheet")
    message("This could take a while...")
    googlesheets::gs_add_row(ss = pesksheet, ws = 1, input = out)
    message("Successfully appended ", nrow(out),  " new rows")
    message("Backing up database")
    backup <- rbind(peskador, out)
    if(!dir.exists("backups")) dir.create("backups")
    saveRDS(backup, file = paste0("backups/backup-", Sys.Date(), ".rds"))
  }
}


### sudo chmod 755 /etc/cron.daily/peskaPARSE (not sure if needed now)
### crontab -e [username]
### open in nano
### add new line and write out:
