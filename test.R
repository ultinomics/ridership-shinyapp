library(shiny)
library(dplyr)
library(readr)
library(magrittr)
library(dygraphs)
library(xts)
library(tidyr)
library(lubridate)
library(lazyeval)
library(scales)

setwd("/Users/danton/GitHub/ridership-shinyapp")
df <- readRDS('data/ntdb.rds')

extract_agency_df <- function(v, df) {

  input <- list()
  input$agency <- v

  input$modes_desc <- df %>%
     slice(which(agency %in% input$agency)) %>%
     extract2('modes_desc')%>%
     table %>%
     names

  input$tos_desc <- df %>%
     slice(which(agency %in% input$agency)) %>%
     extract2('tos_desc')%>%
     table %>%
     names

  input$uza <- df %>%
     slice(which(agency %in% input$agency)) %>%
     extract2('uza') %>%
     unique

  sub_df <- df %>% slice(which(agency %in% input$agency))

  # reduce dataframe ONLY if we have inputs
  if(!is.null(input$uza)) sub_df %<>% slice(which(uza %in% input$uza))
  if(!is.null(input$tos_desc)) sub_df %<>% slice(which(tos_desc %in% input$tos_desc))

  sub_df %<>% select(ymd, agency, uza, tos_desc, modes_desc, upt)
  sub_df
}

sub_df <- extract_agency_df("Durham Area Transit Authority", df)
sub_df

# # only plot if inputs exists
# if(!is.null(input$modes_desc)) {
#   sub_df %<>% slice(which(modes_desc %in% input$modes_desc))
#   sub_df %<>% select(ymd, agency, modes_desc, upt)

#   # get xts frame
#   DF <- sub_df %>%
#      spread(modes_desc, upt) %>%
#      select(-agency)
#   col_modes <- DF %>% select(-ymd) %>% names # get mode names (order matters; do before renaming)
#   xts_df <- as.xts(DF %>% select(-ymd), order.by = DF$ymd)

#   # note: `clrs(col_modes)` ensure consistent mapping by
#   dygraph(xts_df,
#       xlab = 'MM-YYYY',
#       ylab = 'Unlinked Passenger Trips',
#       main = sprintf('Type of Service: %s', input$tos_desc)) %>%
#       dyRangeSelector() %>%
#       dyOptions(colors = clrs(col_modes), fillGraph = TRUE, fillAlpha = 0.4) %>%
#       dyLegend(labelsDiv = "legendDivID", labelsSeparateLines = TRUE)
# }

# label_modes <- function(x) {
#    modes_desc <- c("Motorbus", "Demand Response", "Vanpool", "Demand Response-Taxi", "Commuter Bus", "Heavy Rail", "Light Rail", "Bus Rapid Transit", "Streetcar", "Monorail/Automated Guideway", "Hybrid Rail", "Double Decker Buses")
#    indx <- match(x, modes_desc)
#    modes_desc[indx]
# }

# label_tos <- function(x) {
#    tos_desc <- c("Purchased Transportation", "Directly Operated")
#    indx <- match(x, tos_desc)
#    tos_desc[indx]
# }
