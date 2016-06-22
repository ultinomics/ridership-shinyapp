library(shiny)
library(dplyr)
library(readr)
library(scales)
library(magrittr)
library(dygraphs)
library(xts)
library(tidyr)
library(lubridate)
library(lazyeval)
library(scales)

df <- readRDS('data/ntdb.rds')

assign_colors <- function(x) {
   set.seed(1789)
   modes <- df$modes %>% unique %>% na.omit %>% c %>% sample
   clrs <- hue_pal()(length(modes))
   indx <- which(modes %in% x)
   clrs[indx]
}

ui <- fluidPage(
   titlePanel("National Transit Database: Ridership"),

   fluidRow(
      column(3,
         selectInput(inputId = "agency",
            label = "Agency",
            choices = c(Choose="", df$agency %>% table %>% names),
            selected = "Durham Area Transit Authority"
            selectize = TRUE
         )
      ),

      column(3,
         uiOutput("TOS")
      ),

      column(3,
         uiOutput("UZA")
      ),

      column(3,
         uiOutput("MODES")
      )
   ),

   dygraphOutput('DYGRAPH')
)

server <- function(input, output, rds = TRUE) {

   output$TOS <- renderUI({
      sub_df <- df %>% slice(which(agency %in% input$agency))
      tos <- sub_df$tos %>% unique %>% na.omit() %>% c
      selectInput('tos', 'Choose Service Type', tos, selectize = FALSE)
   })
   output$UZA <- renderUI({
      sub_df <- df %>% slice(which(agency %in% input$agency))
      uza <- sub_df$uza %>% unique %>% na.omit() %>% c
      selectInput('uza', 'Choose Service Type', uza, selectize = FALSE)
   })

   output$MODES <- renderUI({
      sub_df <- df %>% slice(which(agency %in% input$agency))
      modes <- sub_df$modes %>% unique %>% na.omit() %>% c
      checkboxGroupInput('modes', 'Choose Modes', modes)
   })

   output$DYGRAPH <- renderDygraph({
      sub_df <- df %>% slice(which(agency %in% input$agency)) %>%
         slice(which(uza %in% input$uza)) %>%
         slice(which(tos %in% input$tos)) %>%
         select(ymd, agency, modes, upt)

      # only plot if inputs exists
      if(!is.null(input$modes)) {
         sub_df %<>% slice(which(modes %in% input$modes))

         # get colors
         clrs <- assign_colors(input$modes)

         # get xts frame
         DF <- sub_df %>%
            spread(modes, upt) %>%
            select(-agency)
         xts_df <- as.xts(DF %>% select(-ymd), order.by = DF$ymd)

         dygraph(xts_df,
             xlab = 'MM-YYYY',
             ylab = 'Unlinked Passenger Trips',
             main = sprintf('Type of Service: %s', input$tos)) %>%
             dyRangeSelector() %>%
             dyOptions(colors = clrs, fillGraph = TRUE, fillAlpha = 0.4)
      } else {
         sub_df %<>% slice(which(modes %in% input$modes))
         empty <- sub_df %>% select(ymd) %>%
            distinct() %>% cbind(NA)
         xts_df <- as.xts(empty %>% select(-ymd), order.by = empty$ymd)
         dygraph(xts_df)
      }
   })

}

shinyApp(ui = ui, server = server)

