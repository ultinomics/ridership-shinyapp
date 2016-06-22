# init.R --- set up data for app

maindir <- normalizePath("~/GitHub/ridership-shinyapp/")
source(file.path(maindir, "import_data.R"))

export_ridership_data(rds=TRUE)
# export_ridership_data(rds=FALSE)
