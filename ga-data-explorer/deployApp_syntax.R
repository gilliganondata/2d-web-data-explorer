library(rsconnect)

deployApp(appDir = "ga-data-explorer",
          appFiles = c("app.R","gtm.js",".Renviron"),
          appName = "ga-data-explorer")