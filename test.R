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

get_agency <- function(df, i) df$agency %>% table %>% names %>% extract2(i)

# get inputs
input <- list()
input$agency <- get_agency(df, 2)

sub_df <- df %>% slice(which(agency %in% input$agency))

input$modes <- df %>%
   slice(which(agency %in% input$agency)) %>%
   extract2('modes')%>%
   table %>%
   names

input$tos <- df %>%
   slice(which(agency %in% input$agency)) %>%
   extract2('tos')%>%
   table %>%
   names

input$uza <- df %>%
   slice(which(agency %in% input$agency)) %>%
   extract2('uza') %>%
   unique


if(!is.null(input$modes)) {
 sub_df %<>% slice(which(modes %in% input$modes))
}

if(!is.null(input$uza)) {
 sub_df %<>% slice(which(uza %in% input$uza))
}

if(!is.null(input$tos)) {
 sub_df %<>% slice(which(tos %in% input$tos))
}

sub_df %<>% select(ymd, agency, tos, modes, upt)


empty <- sub_df %>% select(ymd) %>%
            distinct() %>% cbind(NA)
         xts_df <- as.xts(empty %>% select(-ymd), order.by = empty$ymd)
dygraph(xts_df)

# get xts frame
xts_dfs <- lapply(input$tos, function(x) {
	DF <- sub_df %>%
		slice(which(tos %in% x)) %>%
    spread(modes, upt) %>%
    select(-agency, -tos)
	as.xts(DF %>% select(-ymd), order.by = DF$ymd)
})

dys <- lapply(seq_along(xts_dfs), function(i) {
   dygraph(xts_dfs[[i]],
    xlab = 'MM-YYYY',
    ylab = 'Unlinked Passenger Trips',
    main = sprintf('Type of Service: %s', input$tos[i])) %>%
    dyRangeSelector() %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.4)
})
