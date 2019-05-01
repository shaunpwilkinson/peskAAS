library(shiny)

shinyUI(fluidPage(
  theme = "bootstrap.css",
  # Application title
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  # ),
  
  fluidRow(
    column(12,
           br(),
           headerPanel(h1("PeskAAS", style="background-color: gainsboro; font-weight: 700; line-height: 2.0;")),
           headerPanel(h4("Timor-Leste fisheries automated analytics system", style="opacity:0.8;")),
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
    column(2,
           br(),
           checkboxGroupInput("hab", label = "Habitat",
                              choices = c("Reef/Ahu ruin","FAD/Rompun","Deep/Tasi kle'an","Beach/Tasi ninin",
                                          "Traditional FAD/Rompun bamboo","Mangrove/Aiparapa","Gleaning/Meti"),
                              selected = c("Reef/Ahu ruin","FAD/Rompun","Deep/Tasi kle'an","Beach/Tasi ninin",
                                           "Traditional FAD/Rompun bamboo","Mangrove/Aiparapa","Gleaning/Meti")),
           actionLink("selectall_hab", "select/deselect all"), 
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
           br(),
           br(),
           dateRangeInput('dateRange',
                          label = "Date range",
                          start = lubridate::floor_date(Sys.Date() - lubridate::years(1), unit = "month") , 
                          end = lubridate::floor_date(Sys.Date(), unit = "month") - lubridate::days(1),
                          min = as.Date("2016-09-01"), max = lubridate::floor_date(Sys.Date(), unit = "month"),
                          separator = " - ", format = "dd-M-yyyy",
                          startview = "year", language = "en-AU"#, weekstart = 1
           ),
           downloadButton("downloadData", "Download (csv)"),
           br(),
           br()
           
    ),
    column(2,
           br(),
           checkboxGroupInput("site", label = "Location",
                              choices = c("Adara","Beloi","Biqueli","Vemasse","Adarai",
                                          "Uaroana","Com","Tutuala","Ililai","Beacou",
                                          "Tolurika","Atekru","Berao","Iliana",
                                          "Fatu'u","Doru","Raiketa","Maquer","Lore"), 
                              selected = c("Adara","Beloi","Biqueli","Vemasse","Adarai",
                                           "Uaroana","Com","Tutuala","Ililai","Beacou",
                                           "Tolurika","Atekru","Berao","Iliana",
                                           "Fatu'u","Doru","Raiketa","Maquer","Lore")),
           actionLink("selectall_site", "select/deselect all"), 
           br(),
           br(),
           sliderInput("smoothen", label = "Smoothness", min = 0, max = 1, value = 0), 
           br()
    ),
    column(8, 
           mainPanel(
             tabsetPanel(
               tabPanel("Combined CPUE", br(), plotOutput("plot1")),
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
    column(5, br()),
    column(2, br(), img(src = "logo.png", height = 50, width = 100),br()),
    column(2, br(), img(src = "MAF.png", height = 50, width = 120), br()),
    column(3, br(), img(src = "GDGP.png", height = 50, width = 160), br())
  ),
  fluidRow(
    column(12, br())
  )
))
