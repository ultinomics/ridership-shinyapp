library(shiny)
library(dplyr)
library(readr)
library(magrittr)
library(dygraphs)
library(xts)
library(tidyr)
library(lubridate)
library(lazyeval)

df <- readRDS('data/ntdb.rds')

ui <- fluidPage(
   titlePanel("National Transit Database: Ridership"),

   br(),

   fluidRow(
      column(4,
         verbatimTextOutput('out1'),
         selectInput(inputId = "agency",
            label = "Agency",
            choices = c(Choose=NULL, df$agency %>% table %>% names),
            selectize = TRUE
         ),
         hr()
      )
      ,
      column(4,
         uiOutput("MODES"),
         hr()
      )
      ,
      column(4,
         uiOutput("TOS"),
         hr()
      )
   ),

   textOutput('check'),
   verbatimTextOutput('checking')
   # dygraphOutput('dygraph')
)

server <- function(input, output, rds = TRUE) {
   output$MODES <- renderUI({
      sub_df <- df %>% slice(which(agency %in% input$agency))
      modes <- sub_df$modes %>% table %>% names
      checkboxGroupInput('modes', 'Choose Modes', modes)
   })

   output$TOS <- renderUI({
      sub_df <- df %>% slice(which(agency %in% input$agency))
      tos <- sub_df$tos %>% table %>% names
      checkboxGroupInput('tos', 'Choose Service Type', tos)
   })


   output$check <- renderText({input$modes})
   output$checking <- renderPrint({

      sub_df <- df %>% slice(which(agency %in% input$agency))

      if(!is.null(input$modes)) {
         sub_df %<>% slice(which(modes %in% input$modes))
      }

      if(!is.null(input$tos)) {
         sub_df %<>% slice(which(tos %in% input$tos))
      }

      sub_df %<>% select(agency, ymd, tos, modes, upt)

      xts_df <- as.xts(sub_df, order.by = sub_df$ymd)

      print(
         list(
            input$agency,
            input$modes,
            input$tos,
            xts_df
         )
      )
   })

   # output$DYGRAPH <- renderDygraph({

   #    sub_df <- df %>%
   #       slice(which(agency %in% input$agency)) %>%
   #       slice(which(modes %in% input$mode)) %>%
   #       slice(which(tos %in% input$tos)) %>%
   #       select(agency, ymd, tos, modes, upt)
   #    xts_df <- as.xts(sub_df, sub_df$ymd)

   # })

}

shinyApp(ui = ui, server = server)

