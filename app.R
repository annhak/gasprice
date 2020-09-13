#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fillPage(
    # Application title
    titlePanel("Gas prices scraped from Tankille.fi"),
    plotlyOutput("plotlyplot")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Get the maximum unix time from the filenames and get the data from the latest file
    filenames <- list.files()
    max_time <- max(as.numeric(gsub("[^0-9]", "",  filenames)), na.rm = TRUE)
    last_file_name <- filenames[grep(max_time, filenames)]
    
    file_content <- read.csv(last_file_name, stringsAsFactors = FALSE)
    file_content$time <- lubridate::ymd_hms(file_content$time)
    
    # Discard stations with less than 2 points as they are not shown in plotly
    counts <- aggregate(cbind(count = price) ~ station, data = file_content, FUN = function(x){NROW(x)})
    station_names <- sort(counts$station[counts$count > 1])
    file_content <- file_content[file_content$station %in% station_names,]

    # Create color palette for stations    
    Palette <- data.frame(station = unique(file_content$station), color = colorRampPalette(brewer.pal(8, "Set2"))(length(unique(file_content$station))), stringsAsFactors = FALSE)
    file_content <- merge(x = file_content, y = Palette, by = "station")

    output$plotlyplot <- renderPlotly({
        g <- file_content %>% 
            group_by(station) %>%
            # initiate a plotly object with date on x and price on y
            plot_ly(x = ~time, y = ~price) %>%
            # add a gray line plot for all stations
            add_lines(name = "All stations", hoverinfo = "station", 
                      line = list(color = 'rgba(150,150,150,0.5)')) %>%
            # plot separate lines for the selected stations
            add_lines(
                name = ~station,
                data = file_content,
                hoverinfo = "station",
                line = list(color = ~color, width = 4) #, visible = "legendonly"
                )  
        g
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
