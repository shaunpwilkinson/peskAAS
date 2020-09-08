FROM rocker/tidyverse:4.0.2

RUN install2.r shiny shinyWidgets cowplot gridExtra leaflet RMySQL zoo