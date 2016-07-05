library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(lubridate)
library(stringr)
library(stringi)
library(lazyeval)
options(scipen=999)

# Purpose -----------------------------------------------------------------

# Get ridership time series data from the NTDB for our customers and
#  plot it over time

# Setup -------------------------------------------------------------------
#' @param maindir Character. Path to main directory containing 'app.R' script
#' @param merge_tloc Logical. Where or not to merge TransLoc data
#'
#' @return tbl_df. A 'long' version of the ridership data.

import_ridership_data <- function(maindir = '~/GitHub/ridership-shinyapp', merge_tloc = FALSE) {

	files <- list.files(file.path(maindir, 'data-raw'), full.names=TRUE)
	csvs <- sapply(files, readr::read_csv, simplify = FALSE)
	nms <- names(csvs) %>% basename

	# Read in data ------------------------------------------------------------
	ntdb_df <- csvs[[grep('raw database', nms, ignore.case=TRUE)]] %>%
	  select(-`4 digit NTDID`) %>%
	  rename(NTDID = `5 digit NTDID`)

	## transloc customer data
	tl_df <- csvs[[grep('transloc', nms, ignore.case=TRUE)]]

	# There are a bunch of messy columns at the end - get rid of them
	na_ind <- which(!is.na(names(tl_df)))
	tl_df <- tl_df[, na_ind]

	# Join ridership and TransLoc data ----------------------------------------

	if(merge_tloc) {
		# Join just for customers with NTDID filled in
		cust_df <- tl_df %>%
		  filter(!is.na(NTDID)) %>%
		  mutate(NTDID = as.character(NTDID))

		# Note: there is one duplicated ID
		n_distinct(cust_df$NTDID) == nrow(cust_df)

		# This joins database numbers for ALL modes for these agencies
		id_df <- left_join(cust_df, ntdb_df, by = 'NTDID')
	} else {
		id_df <- ntdb_df
	}

	# Look at modes per agency
	mode_sum_df <- id_df %>%
	  group_by(NTDID) %>%
	  summarize(n_modes = n_distinct(Modes))

	# Make long version
	long_df <- id_df %>%
	  gather(key = 'month_key', value = 'UPT', JAN02:MAR16) # BEWARE - this is hardcoded
	# lowercase
	names(long_df) <- names(long_df) %>% tolower
	names(long_df) <- str_replace_all(names(long_df), '[[:punct:]]|[ ]', '_')

	# Check that conversion has expected dimensions
	n_months <- 171
	nrow(long_df) == n_months * nrow(id_df)

	long_df

}

clean_ridership_data <- function(DF) {

	long_df <- DF

	# clean up and create variables -------------------------------------------

	long_df$month <- substr(long_df$month_key, start = 1, stop = 3)
	long_df$year <- substr(long_df$month_key, start = 4, stop = 6)
	long_df$date <- paste0(long_df$month, ' 01 20', long_df$year)
	long_df$dt <- mdy(long_df$date)
	long_df %<>% rename(ymd = dt)

	# trim agency names
	long_df$agency <- long_df %>%
		select(agency) %>%
		separate(agency, paste(1:2), ",", extra = "merge") %>%
		extract2('1')

	long_df	%<>%
		mutate(agency = stri_trans_totitle(agency)) %>%
		attach_tos_desc %>%
		attach_modes_desc

	# remove missing agencies
	long_df %<>% slice(which(!is.na(agency)))

	# remove missing modes
	long_df %<>% slice(which(!is.na(modes)))

	# remove if all upt are missing
	long_df %<>% remove_if_all_na(., 'upt')

	long_df
}



#' Exports the output of `import_ridership_data()`
#'
#' @param maindir Character. Path to main directory containing 'app.R' script
#' @param rds Logical. To save as RDS or csv file. Default is RDS.
#'

export_ridership_data <- function(maindir = '~/GitHub/ridership-shinyapp/', rds = TRUE) {

		df <- import_ridership_data(maindir = '~/GitHub/ridership-shinyapp/') %>%
			clean_ridership_data
		if(rds) {
			saveRDS(df, file.path(maindir, 'data', 'ntdb.rds'))
		} else {
			write.csv(df, file.path(maindir, 'data', 'ntdb.csv'), row.names = FALSE)
		}
}

# UTILITY FUNCTIONS ------------------------------------------------

attach_modes_desc <- function(DF) {
	modes_df <- data_frame(
		modes = c("MB", "DR", "VP", "DT", "CB", "HR", "LR", "RB", "SR", "MG", "YR", "DB", "TB", "CR", "FB", "AR", "TR", "OR", "IP", "PB", "CC", "AG", "MO"),
		modes_desc = c("Motorbus", "Demand Response", "Vanpool", "Demand Response-Taxi", "Commuter Bus", "Heavy Rail", "Light Rail", "Bus Rapid Transit", "Streetcar", "Monorail/Automated Guideway", "Hybrid Rail", "Double Decker Buses", "Trolleybus", "Commuter Rail", "Ferryboat", "Alaska Railroad", "Aerial Tramway", "Over-the-Road Bus", "Inclined Plane", "Publico (non-rail mode)", "Cable Car", "Automated Guideway", "Monorail")
	)
	DF %>%
	  left_join(., modes_df, by = 'modes')
}

attach_tos_desc <- function(DF) {
	tos_df <- data_frame(tos = c("PT", "DO"),
			tos_desc = c("Purchased Transportation", "Directly Operated"))
	DF %>%
	  left_join(., tos_df, by = 'tos')
}


#' drops the agency, mode, uza rows if ALL `var` are missing
remove_if_all_na <- function(DF, var) {

	# Look for missing values
	missing_df <- DF %>%
	  group_by(ntdid, tos, modes, uza) %>%
	  summarize_(n_missing = interp("sum(is.na(x))", x = as.name(var)), len = ~n()) %>%
	  mutate(drop = (len == n_missing)) %>%
	  select(ntdid, tos, modes, uza, drop) %>%
	  distinct

	DF2 <- DF %>%
		inner_join(missing_df, by = c('ntdid', 'tos', 'modes', 'uza')) %>%
		slice(which(!drop)) %>%
		select(-drop)

	DF2
}
