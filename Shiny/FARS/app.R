# A new Shiny App to help me with data visualization on the FARS data
# The version as it is does a poor job of displaying the data because the plot
# is not large enough to show all the x labels, and overall there are often
# too many x labels. Also, in the case of the HOURS plot, I want 99 to come
# immediately after 24 so as not to leave a giant gap.

library(tidyverse)
library(shiny)
library(ggplot2)

# Load data ----
crash_data <- read.csv("../accident.csv")

# Clean data ----
cr.df <- crash_data %>%
  select("STATENAME", "ST_CASE", "VE_TOTAL", "PEDS", "PERSONS",
         "PERMVIT", "PERNOTMVIT", "COUNTYNAME", "CITYNAME", "DAY", "MONTH",
         "MONTHNAME", "YEAR", "DAY_WEEK", "DAY_WEEKNAME", "HOUR", "MINUTE",
         "ROUTENAME", "RUR_URBNAME", "FUNC_SYSNAME", "HARM_EVNAME",
         "RELJCT2NAME", "TYP_INTNAME", "REL_ROADNAME", "LGT_CONDNAME",
         "WEATHERNAME", "FATALS", "DRUNK_DR")

cr.df$MONTHNAME <- reorder(cr.df$MONTHNAME, cr.df$MONTH)
cr.df$DAY_WEEKNAME <- reorder(cr.df$DAY_WEEKNAME, cr.df$DAY_WEEK)

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("selection",
                  label = h3("Variable to display"),
                  choices = list("State" = "STATENAME",
                                 "Pedestrians involved" = "PEDS",
                                 "Month" = "MONTHNAME",
                                 "Day of the week" = "DAY_WEEKNAME",
                                 "Hour" = "HOUR",
                                 "Route Type" = "ROUTENAME",
                                 "Rural/Urban" = "RUR_URBNAME",
                                 "Functional class" = "FUNC_SYSNAME",
                                 "Harmful event" = "HARM_EVNAME",
                                 "RELJCT2" = "RELJCT2NAME",
                                 "Intersection" = "TYP_INTNAME",
                                 "Relation to trafficway" = "REL_ROADNAME",
                                 "Light conditions" = "LGT_CONDNAME",
                                 "Weather" = "WEATHERNAME",
                                 "Fatalities" = "FATALS",
                                 "Drunk drivers" = "DRUNK_DR"),
                  selected = "PEDS")
      
    ),
    
    mainPanel(
      
      plotOutput("barplot") # Got an error when I forgot the quote );
      
    )
    
  )
)

server <- function(input, output) {
  
  dataSelection <- reactive(cr.df %>% count(!!sym(input$selection)))
  
  output$barplot <- renderPlot({
    dataSelection() %>%
      ggplot(aes_string(x = input$selection, "n"))+
      geom_col()
  })
  
}

shinyApp(ui=ui, server=server)