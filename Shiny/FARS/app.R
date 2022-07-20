# A new Shiny App to help me with data visualization on the FARS data
# Things to do: consolidate any categories beyond 7 into "other", except for
# HOURS and MONTHNAME.

library(tidyverse)
library(shiny)
library(ggplot2)

# Load data ----
crash_data <- read.csv("accident.csv")

# Clean data ----
cr.df <- crash_data %>%
  select("STATENAME", "ST_CASE", "VE_TOTAL", "PEDS", "PERSONS",
         "PERMVIT", "PERNOTMVIT", "COUNTYNAME", "CITYNAME", "DAY", "MONTH",
         "MONTHNAME", "YEAR", "DAY_WEEK", "DAY_WEEKNAME", "HOUR", "MINUTE",
         "ROUTENAME", "RUR_URBNAME", "FUNC_SYSNAME", "HARM_EVNAME",
         "RELJCT2NAME", "TYP_INTNAME", "REL_ROADNAME", "LGT_CONDNAME",
         "WEATHERNAME", "FATALS", "DRUNK_DR")

cr.df[, !names(cr.df) %in% c("MONTH", "DAY_WEEK")] <- 
  data.frame(lapply(cr.df[, !names(cr.df) %in% c("MONTH", "DAY_WEEK")], as.factor))

cr.df$MONTHNAME <- reorder(cr.df$MONTHNAME, cr.df$MONTH)
cr.df$DAY_WEEKNAME <- reorder(cr.df$DAY_WEEKNAME, cr.df$DAY_WEEK)

ui <- fluidPage(
  fluidRow(
    column(4,
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
    column(4,
           offset = 4,
           tableOutput("table"))
  ),
  fluidRow(
    column(12,
           plotOutput("barplot"))
  ),
)

count_top <- function(df, var, n = 7) {
  df %>%
    mutate({{ var }} := fct_lump_n({{ var }}, n = n)) %>%
    count({{ var }}, sort = TRUE)
}

server <- function(input, output) {
  
  dataSelection <- reactive(count_top(cr.df, !!sym(input$selection)))
  
  output$table <- renderTable(
    dataSelection()
  )
  
  output$barplot <- renderPlot({
    dataSelection() %>%
      ggplot(aes_string(x = input$selection, "n"))+
      geom_col(fill = "blue")
  })
  
}

shinyApp(ui=ui, server=server)