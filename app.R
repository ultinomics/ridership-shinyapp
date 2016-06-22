library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(scales)
library(magrittr)
library(dygraphs)
library(xts)
library(tidyr)
library(lubridate)
library(scales)
options(scipen=999)

df <- readRDS('data/ntdb.rds')

assign_colors <- function(x) {
   modes_desc <- df %>%
      na.omit %>%
      select(modes_desc) %>%
      table %>%
      sort(decreasing=TRUE) %>%
      names
   n <- 5
   m <- 5
   top <- modes_desc %>% head(n)
   middle <- modes_desc[c((n+1):(length(modes_desc)-m))]
   bottom <- modes_desc %>% tail(m)
   clrs1 <- brewer_pal("qual", "Set1")(length(top))
   clrs2 <- brewer_pal("qual", "Set3")(length(middle))
   clrs3 <- grey_pal()(length(bottom))
   clrs <- c(clrs1, clrs2, clrs3)
   indx <- match(x, modes_desc)
   clrs[indx]
}

# save function envirs
clrs <- assign_colors

## UI SET UP
header <- dashboardHeader(disable=TRUE)

body <- dashboardBody(

   fluidRow(
      column(12,
         box(width = NULL, solidHeader = TRUE,
            dygraphOutput('DYGRAPH')
         )
      )
   ),

   fluidRow(
      box(width = 9, solidHeader = TRUE, status = "warning", title = "Inputs",
         column(width = 6,
            selectInput(inputId = "agency",
               label = "Agency",
               choices = c(Choose="", df$agency %>% table %>% names),
               selected = "Durham Area Transit Authority",
               selectize = TRUE)
         ),

         column(width = 2,
            uiOutput("TOS")
         ),

         column(width = 2,
            uiOutput("UZA")
         ),

         column(width = 2,
            uiOutput("MODES")
         )
      ),

      box(width = 3, solidHeader = TRUE, status = 'info', title = "Legend",
         column(width = 12,
            textOutput("legendDivID")
         )
      )
   )
   # , textOutput("out1")
)

ui <- dashboardPage(
   header,
   dashboardSidebar(disable=TRUE),
   body,
   title = "National Transit Database: Ridership"
)

server <- function(input, output, rds = TRUE) {


   # output$out1 <- renderPrint({
   #    list(input$agency,
   #       input$tos_desc,
   #       input$uza,
   #       input$modes_desc
   #    )
   # })

   output$TOS <- renderUI({
      sub_df <- df %>% slice(which(agency %in% input$agency))
      tos_desc <- sub_df$tos_desc %>% unique %>% c
      selectInput('tos_desc', 'Select Service Type', tos_desc, selectize = FALSE)
      if(length(tos_desc) != sum(is.na(tos_desc))) {
         selectInput('tos_desc', 'Select Urbanized Area Number', tos_desc, selectize = FALSE)
      } else {
         selectInput('tos_desc', 'Select Urbanized Area Number', NULL, selectize = FALSE)
      }
   })

   output$UZA <- renderUI({
      sub_df <- df %>% slice(which(agency %in% input$agency))
      uza <- sub_df$uza %>% unique %>% c
      if(length(uza) != sum(is.na(uza))) {
         selectInput('uza', 'Select Urbanized Area Number', uza, selectize = FALSE)
      } else {
         selectInput('uza', 'Select Urbanized Area Number', NULL, selectize = FALSE)
      }
   })

   output$MODES <- renderUI({
      sub_df <- df %>% slice(which(agency %in% input$agency))
      modes_desc <- sub_df$modes_desc %>% unique %>% na.omit %>% c
      checkboxGroupInput('modes_desc', 'Choose Modes', modes_desc, selected = modes_desc)
   })

   output$DYGRAPH <- renderDygraph({
      withProgress(message = "Loading...", {
         sub_df <- df %>% slice(which(agency %in% input$agency))

         # reduce dataframe ONLY if we have inputs. if NULL, reduce and remove duplicates
         if(any(is.null(input$uza), is.null(input$tos_desc))) {
            sub_df %<>% arrange(ymd, upt) %>% distinct(ymd, agency, uza, tos_desc, modes_desc)
         } else {
            if(!is.null(input$uza)) sub_df %<>% slice(which(uza %in% input$uza))
            if(!is.null(input$tos_desc)) sub_df %<>% slice(which(tos_desc %in% input$tos_desc))
         }

         # remove data that is not used to plot
         sub_df %<>% select(ymd, agency, modes_desc, upt)

         # only plot if inputs exists
         if(!is.null(input$modes_desc)) {
            sub_df %<>% slice(which(modes_desc %in% input$modes_desc))

            # get xts frame
            DF <- sub_df %>%
               spread(modes_desc, upt) %>%
               select(-agency)
            col_modes <- DF %>% select(-ymd) %>% names # get mode names (order matters; do before renaming)
            xts_df <- as.xts(DF %>% select(-ymd), order.by = DF$ymd)

            # note: `clrs(col_modes)` ensure consistent mapping by
            dygraph(xts_df,
                xlab = 'MM-YYYY',
                ylab = 'Unlinked Passenger Trips',
                main = sprintf('Type of Service: %s', input$tos_desc)) %>%
                dyRangeSelector() %>%
                dyOptions(colors = clrs(col_modes), fillGraph = TRUE, fillAlpha = 0.4) %>%
                dyLegend(labelsDiv = "legendDivID", labelsSeparateLines = TRUE)
         } else {
            NULL
         }
      })
   })

}

shinyApp(ui = ui, server = server)

