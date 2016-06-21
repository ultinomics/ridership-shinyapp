library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(lubridate)
library(stringr)
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

	# Check that conversion has expected dimensions
	n_months <- 171
	nrow(long_df) == n_months * nrow(id_df)

	# Look for missing values

	missing_df <- long_df %>%
	  group_by(NTDID, Modes) %>%
	  summarize(n_missing = sum(is.na(UPT)))

	# Clean up and create variables -------------------------------------------

	long_df$month <- substr(long_df$month_key, start = 1, stop = 3)
	long_df$year <- substr(long_df$month_key, start = 4, stop = 6)
	long_df$date <- paste0(long_df$month, ' 01 20', long_df$year)
	long_df$dt <- mdy(long_df$date)

	names(long_df) <- names(long_df) %>% tolower
	names(long_df) <- str_replace_all(names(long_df), '[[:punct:]]|[ ]', '_')
	long_df %<>% rename(ymd = dt)

	long_df
}

#' Exports the output of `import_ridership_data()`
#'
#' @param maindir Character. Path to main directory containing 'app.R' script
#' @param rds Logical. To save as RDS or csv file. Default is RDS.
#'

export_ridership_data <- function(maindir = '~/GitHub/ridership-shinyapp/', rds = TRUE) {

		df <- import_ridership_data(maindir = '~/GitHub/ridership-shinyapp/')
		if(rds) {
			saveRDS(df, file.path(maindir, 'data', 'ntdb.rds'))
		} else {
			write.csv(df, file.path(maindir, 'data', 'ntdb.csv'), row.names = FALSE)
		}
}