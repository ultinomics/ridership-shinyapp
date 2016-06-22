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
options(scipen=999)

df <- readRDS('data/ntdb.rds')

assign_colors <- function(x) {
   modes <- df %>%
      na.omit %>%
      select(modes) %>%
      table %>%
      sort(decreasing=TRUE) %>%
      names
   n <- 5
   m <- 5
   top <- modes %>% head(n)
   middle <- modes[c((n+1):(length(modes)-m))]
   bottom <- modes %>% tail(m)
   clrs1 <- brewer_pal("qual", "Set1")(length(top))
   clrs2 <- brewer_pal("qual", "Set3")(length(middle))
   clrs3 <- grey_pal()(length(bottom))
   clrs <- c(clrs1, clrs2, clrs3)
   indx <- match(x, modes)
   clrs[indx]
}

clrs <- assign_colors

ui <- fluidPage(
   titlePanel("National Transit Database: Ridership"),

   fluidRow(
      column(3,
         selectInput(inputId = "agency",
            label = "Agency",
            choices = c(Choose="", df$agency %>% table %>% names),
            selected = "Durham Area Transit Authority",
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
      checkboxGroupInput('modes', 'Choose Modes', modes, selected = modes)
   })

   output$DYGRAPH <- renderDygraph({
      sub_df <- df %>% slice(which(agency %in% input$agency)) %>%
         slice(which(uza %in% input$uza)) %>%
         slice(which(tos %in% input$tos)) %>%
         select(ymd, agency, modes, upt)

      # only plot if inputs exists
      if(!is.null(input$modes)) {
         sub_df %<>% slice(which(modes %in% input$modes))

         # get xts frame
         DF <- sub_df %>%
            spread(modes, upt) %>%
            select(-agency)
         col_modes <- DF %>% select(-ymd) %>% names # get mode names
         xts_df <- as.xts(DF %>% select(-ymd), order.by = DF$ymd)

         # note: `clrs(col_modes)` ensure consistent mapping by
         dygraph(xts_df,
             xlab = 'MM-YYYY',
             ylab = 'Unlinked Passenger Trips',
             main = sprintf('Type of Service: %s', input$tos)) %>%
             dyRangeSelector() %>%
             dyOptions(colors = clrs(col_modes), fillGraph = TRUE, fillAlpha = 0.4)
      } else {
         NULL
      }
   })

}

shinyApp(ui = ui, server = server)

