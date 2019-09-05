library(shiny)
library(shinyWidgets)
library(leaflet)

shinyUI(fluidPage(
  tags$head(includeScript("google-analytics.js")),
  includeCSS("www/bootstrap.css"),
  theme = "bootstrap.css",
  shinyWidgets::setBackgroundImage(src = "rect.png"),
  # Application title
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  # ),
  
  fluidRow(
    column(12,
           headerPanel(h1("PeskAAS")),
           #br(),
           #headerPanel(h1("PeskAAS", style="font-weight: 750; line-height: 1.0;")), #background-color: gainsboro; 
           headerPanel(h4("Automated Analytics System for Small Scale Fisheries in Timor-Leste", 
                          style="opacity:0.8; text-decoration: overline; line-height: 1.0;")),
           headerPanel(h4("nothing", style="opacity:0;")),
           # tags$div(class="header", checked=NA,
           #          tags$style()
           #       list(
           #         h1("color:blue; PeskAAS"),
           #         h4("color:blue; Timor-Leste fisheries data exploration tool"),
           #         style("background-color: lightgrey;")
           #       )
           #       #a(href="shiny.rstudio.com/tutorial", "Click Here!")
           #   ),
           #titlePanel("Reported Monthly Fisheries Catch in Timor-Leste"),
           br()
           )
    
    # headerPanel(
    #   list(tags$head(tags$style(".span12 {background-color: black;}")), "PeskAAS", 
    #        HTML('<img src="logo.png", height="50px", style="float:right"/>', 
    #             '<p style="color:red"> Timor-Leste fisheries data exploration tool </p>' ))
    # )
    

  ),
  fluidRow(
    column(1, #style = "background-color:gainsboro;",
           br(),

           dateInput('startDate',
                     label = "Start date",
                     value = lubridate::floor_date(Sys.Date() - lubridate::years(1), unit = "month") ,
                     min = as.Date("2016-09-01"), max = lubridate::floor_date(Sys.Date(), unit = "month"),
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
           # br(),
           # br(),
           # br(),
           # br(),
           # sliderInput("smoothen", label = "Smoothness", min = 0, max = 1, value = 0),
           # br(),
           br()
    ),
    column(2, #style = "background-color:gainsboro;",
           br(),
           dateInput('endDate',
                     label = "End date",
                     value = lubridate::floor_date(Sys.Date(), unit = "month") - lubridate::days(1),
                     min = as.Date("2016-09-01"), max = lubridate::floor_date(Sys.Date(), unit = "month"),
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
           # br(),
           # br(),
           # br(),
           # downloadButton("summaryTab.csv", "Download (csv)"),
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
))
