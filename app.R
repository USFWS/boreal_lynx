#' @title Collar viewer
#'
#' @description  An R shiny app for visualizing and summarizing GPS collar data.
#' @author McCrea Cobb \email{mccrea_cobb@@fws.gov}
#'
#' @example shiny::runGitHub( "collar-viewer", "USFWS", launch.browser=T)


# Install the required packages if they aren't already installed
packages <- c("shiny", "shinyjs", "tidyverse", "leaflet", "shinyWidgets", "DT", "sp",
              "rgdal","adehabitatLT", "adehabitatHR", "lubridate", "geojsonio",
              "maptools", "dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

# Load the required packages
library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(DT)
library(sp)
library(rgdal)
library(adehabitatLT)
library(adehabitatHR)
library(lubridate)
library(geojsonio)
library(maptools)
library(dplyr)

# Source custom functions
source("code/functions/global.R")

# Define UI for application that draws a histogram
ui <- navbarPage("Collar-Viewer v.0.2.0-alpha", id="nav",

                 # Tab 1: Load data
                 tabPanel("Load data",
                          # Sidebar layout
                          fluidPage(
                            fileInput(inputId = "file",
                                      label = "Select collar data:",
                                      multiple = FALSE,
                                      accept = ".RData")), # Get the file upload control option

                          DT::DTOutput("sum.tbl")
                 ),

                 # Tab 2: Interactive map
                 tabPanel("Interactive map",
                          div(class = "outer",

                              tags$head(includeCSS("css/style.css")),  # Add custom css style

                              leafletOutput("map", width="100%", height="100%"),  # Add the leaflet map

                              absolutePanel(id = "controls",
                                            class = "panel panel-default",
                                            fixed = TRUE,
                                            draggable = TRUE,
                                            top = 60,
                                            left = 20,
                                            right = "auto",
                                            bottom = "auto",
                                            width = "20%",
                                            height = "auto",

                                            htmlOutput("dataInfo"),  # Add a header of summaries of # of fixes, first/last fix, etc.

                                            shinyWidgets::selectizeGroupUI(  # Add the data filters
                                              id = "my-filters",
                                              inline = FALSE,
                                              params = list(
                                                animal_id = list(inputId = "animal_id", title = "Lynx:", placeholder = 'select'),
                                                collar_id = list(inputId = "collar_id", title = "Collar:", placeholder = 'select'),
                                                site = list(inputId = "site", title = "Site:", placeholder = 'select'),
                                                sex = list(inputId = "sex", title = "Sex:", placeholder = 'select'),
                                                age = list(inputId = "age", title = "Age class:", placeholder = 'select')
                                              )
                                            )
                              )
                          )
                 ),

                 # Tab 3: Home Ranges
                 tabPanel("Home Ranges",
                          div(class = "outer",

                              tags$head(includeCSS("css/style.css")),

                              leafletOutput("map_hrs", width = "100%", height = "100%"),  # Add the map

                              absolutePanel(id = "controls",
                                            class = "panel panel-default",
                                            fixed = TRUE,
                                            draggable = TRUE,
                                            top = 60,
                                            left = 20,
                                            right = "auto",
                                            bottom = "auto",
                                            width = "20%",
                                            height = "auto",

                                            HTML("<br>"),

                                            selectizeInput("sl_HomeRange", "Make a selection:",  # Add the inputs to select a home range method
                                                           choices = c('Display Points', "Minimum Convex Polygon", "Kernel Density", "Brownian Bridge"),
                                                           selected = 'Display Points'),
                                            textInput('tx_Contour', 'Contour Percentages:', placeholder = '50, 95', value = '50, 95'),
                                            actionButton("ac_UpdateMap", "Update Map")
                              )
                          )
                 )
)


#-------------------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {

  options(shiny.maxRequestSize=30*1024^2)

  #----
  ## Tab 1: Load data

  # Define the uploaded .Rdata file
  dat <- reactive({
    req(input$file)
    if (is.null(input$file))
      return(NULL)
    inFile <- input$file
    file <- inFile$datapath
    # Load the file into new environment and get it from there
    e = new.env()
    name <- load(file, envir = e)
    dat <- e[[name]]
    dat <- dat %>%
      filter(!is.na(lat))  # Filter out rows that are missing latitude values
    dat
  })

  # Summarize data for the summary table
  dat.tbl <- reactive({
    req(input$file)
    dat() %>%
      group_by("Lynx ID" = animal_id,
               "Study site" = site,
               "Age class" = age,
               "Sex" = sex) %>%
      summarise(
        "Capture date" = as.Date(first(capture_date)),
        "Fix schedule" = last(fix_sched),
        "Current fix rate" = last(fix_rate),
        "First fix" = min(as.Date(fix_time)),
        "Last fix" = max(as.Date(fix_time)))
  })

  # Create a DT table of the summarized data
  output$sum.tbl <- DT::renderDT(dat.tbl(), options = list(pageLength = 20))


  #----
  ## Tab 2: Map

  # Subset the data based on user input from selectizeGroupUI:
  dat.sub <- callModule(
    id = "my-filters",
    module = selectizeGroupServer,
    data = dat,
    vars = c("site", "sex", "age", "animal_id", "collar_id")
  )

  # Create a map of the subsetted data:
  output$map <- renderLeaflet({
    collar_map(dat.sub())
  })

  # Output info for fixes selected in the map
  output$dataInfo <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<br>"),
            paste("<b>Total Lynx:</b> ", length(unique(dat.sub()$animal_id))),
            paste("<b>Total Collars:</b> ", length(unique(dat.sub()$collar_id))),
            paste("<b>Total Fixes:</b> ", nrow(dat.sub())),
            paste("<b>Min. Date:</b> ", as.Date(min(dat.sub()$fix_time))),
            paste("<b>Max. Date:</b> ", as.Date(max(dat.sub()$fix_time))),
            paste("<br>")
      ))
  })

  #----
  ## Tab 3: Home Ranges

  dat.move <- eventReactive(input$ac_UpdateMap, {
    ## Create a dataframe of movement parameters for analysis

    df <- xy_conv(dat.sub())  # Add projected x and y coordinates to the df used for distance and speed calculations

    move <- df %>%
      arrange(fix_time) %>%
      group_by(animal_id) %>%
      mutate(Distance = move_dist(x, y),  # Sourced from global.R
             sigDist = cumsum(Distance),
             NSD = move_nsd(x, y), # Sourced from global.R
             dTime = move_dt(fix_time),  # Sourced from global.R
             Speed = move_speed(Distance, dTime),  # Sourced from global.R
             Year = lubridate::year(fix_time),
             Month = lubridate::month(fix_time),
             Day = lubridate::day(fix_time),
             Hour = lubridate::hour(fix_time)) %>%
      ungroup()
    return(move)
  })

  ## Create a comma separated list of values for the home range contours based on user input
  pct_contour <- reactive({
    return(as.numeric(strsplit(input$tx_Contour, ', ')[[1]]))
  })

  ## Home range estimation
  hr_ud <- eventReactive(input$ac_UpdateMap, {
    df <- as.data.frame(dat.move())
    if (input$sl_HomeRange == 'Minimum Convex Polygon') {  # If user selects Minimum Convex Polygon home range

      spdf <- sp::SpatialPointsDataFrame(coordinates(cbind(df$x, df$y)),
                                         data = df, proj4string = CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      cp <- adehabitatHR::mcp(spdf[,"animal_id"], percent = 99)
      cp <- sp::spTransform(cp, CRS('+proj=longlat'))
      ids <- cp$animal_id
      hr <- vector("list", length(cp$animal_id))
      for (i in seq_along(ids)) {
        poly <- cp[cp$animal_id == ids[i], ]
        poly <- geojson_json(poly)
        hr[[i]] <- poly
      }
      names(hr) <- ids

    } else if (input$sl_HomeRange == 'Kernel Density') {  # If user selects Kernel home range

      kd <- sp::SpatialPointsDataFrame(coordinates(cbind(df$lon, df$lat)), data = df,
                                       proj4string = CRS('+proj=longlat'))
      kd <- adehabitatHR::kernelUD(kd[, "animal_id"])
      hr <- lapply(kd, function(x) get_contours(x, pct_contour()))  # get_contours() sourced from global.R

    } else if (input$sl_HomeRange == 'Brownian Bridge') {  # If user selects Brownian Bridge home range

      bb <- to_ltraj(df)  # Sourced from global.R
      bb <- estimate_bbmm(bb)  # Sourced from global.R
      #bb <- bb_fix(bb)  # Sourced from global.R, not needed, but kept just in case
      hr <- lapply(bb, function(x) get_contours(x, pct_contour()))  # get_contours() sourced from global.R
      for (i in seq_along(bb)) {
        hr[[i]]@proj4string <- sp::CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
        hr[[i]] <- sp::spTransform(hr[[i]], CRS('+proj=longlat'))
      }
    }

    return(hr)
  })


  lfMap <- eventReactive(input$ac_UpdateMap, {  # Create map of home ranges

    hr <- hr_ud()
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      hr <- lapply(hr, function(x) geojson_json(x))
    }

    lflt <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap',
                                           options = providerTileOptions(attribution = NA)) %>%
      addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers")

    if (input$sl_HomeRange == 'Display Points') {
      lflt <- lflt %>%
        map_pts(dat.move())
    } else {
      shinyjs::logjs(paste(sep = ' - ', 'homerange', input$sl_HomeRange, dput(hr)))
      lflt <- lflt %>%
        map_polygons(hr) %>%
        map_pts(dat.move())
    }
  })

  # Map Output
  output$map_hrs <- renderLeaflet({
    shinyjs::logjs(paste(dput(lfMap())))
    lfMap()
  })

  # Hide Polygon output if MCP is selected
  observeEvent(input$sl_HomeRange, {
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      shinyjs::show('dl_Shape')
    } else {
      shinyjs::hide('dl_Shape')
    }
  })

}


# Run the application
shinyApp(ui = ui, server = server)
