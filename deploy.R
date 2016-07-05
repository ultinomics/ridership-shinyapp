library(rsconnect)
deployApp(appDir = "/Users/danton/GitHub/ridership-shinyapp",
	appFileManifest = "manifest.txt",
	account = "transloc")