# Load packages
library(shiny)
library(tidyverse)
library(feather)
library(plotly)
library(purrr)

fluidPage(
  
  # Application title
  titlePanel("City Weather Data"),
  
  # Script to get width of browser window
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
  
  # Select city
  uiOutput("citySelect"),
  
  # Plots
  uiOutput("myPlots")

)
