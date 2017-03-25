# Demo script to pull some data from Yandex Metrics
#
# App to explore Yandex Metrica metrics across two dimensions. The only REQUIRED
# setup to get this to run is the either the hardcoding of various IDs
# of a .Renviron file with the IDs defined. This is detailed in the first lengthy
# comment below.
#
# This requires setting up an OAuth access token, which you can do
# by clicking on "API" in the global navigation once logged into Yandex Metrics.

# Set the callback URL to: http://localhost:1410/

# Add the ID and password to a .Renviron file that looks like this (without the
# "# " on each line, and without the "[]" brackets):

# YANDEX_APP_ID=[the client ID for your app]
# YANDEX_APP_PWD=[the password for your app]
# YANDEX_COUNTER_ID=[the ID for your counter (site), this will be 
#                    in your reporting interface and will be a ~8-digit #]

# We're only using a couple of libraries. 
library(httr)               # For accessing the API data
library(shiny)              # We must web-enable this whole thing
library(tidyverse)          # For data transformations -- primarily just uses dplyr commands
library(scales)             # Cuz, ya' know, commas in displayed numbers

# # Get the Yandex Counter ID from the .Renviron file. This is just a text file
# # that has one line:
# # YANDEX_COUNTER_ID="[the actual counter ID]"
# # Don't include the brackets, but do include the quotation marks. You can also simply 
# # hardcode the counter ID rather than using Sys.getenv below, but that's less GitHub-
# # open-sharing-friendly.
# counter_id <- Sys.getenv("YANDEX_COUNTER_ID")
# # counter_id <- "XXXXXXXX"  # Option for hardcoding the counter ID
# 
# # Read the values from the .Renviron file
# client_id <- Sys.getenv("YANDEX_APP_ID")
# pwd <- Sys.getenv("YANDEX_APP_PWD")

# Set the preferred language for the results. I assume this is some ISO 2-letter code.
lang <- "en"

counter_id <- "42846864"
client_id <- "62cef6b1598d450ba306960aea621060"
pwd <- "fa81c235e1ce4e969df5b2bf09fcb51f"


####################
# Set up the different options for interaction
####################

# DATE OPTIONS
# This could also be set as date selectors easily enough, but, for now, it's just set
# as some preset options. As a note, even though the values are being set as numerics 
# here, they actually get treated as characters, so they have to be converted to 
# back to numerics when setting start_date in the get_data() function.
daterange_options <- list("Last 7 Days" = 7,
                          "Last 30 Days" = 30,
                          "Last 60 Days" = 60,
                          "Last 90 Days" = 90)

# METRIC OPTIONS
# As currently written, these can only be # summable metrics, as the data gets pulled
# once as daily data and # then gets aggregated (and would require much more 
# hoop-jumping to then do weighted averages to get 'rates' of any sort).
metric_options <- list("Sessions" = "ym:s:visits",
                       "Pageviews" = "ym:s:pageviews")

# DIMENSION OPTIONS
# The first value can be anything, but the second value needs to be the Yandex 
# Metrica API value.
dimension_options <- list("Traffic Source (Last Touch)" = "ym:s:lastTrafficSourceName",
                          # "Traffic Source (First)" = "ym:s:firsstTrafficSourceName",
                          # "Referral (Last)" = "ym:s:lastReferalSource",
                          # "Referral (First)" = "ym:s:firstReferalSource",
                          "Country" = "ym:s:regionCountry",
                          "Device Category" = "ym:s:deviceCategory",
                          "Operating System" = "ym:s:operatingSystemRoot",
                          "Browser" = "ym:s:browser")

####################
# Define base theme You have an option of tweaking settings
# here OR in the actual functions/output that use default_theme
####################

default_theme <-   theme_bw() +
  theme(axis.text = element_text(face = "bold", size = 14, colour = "grey10"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        legend.justification = "center",
        strip.text.x = element_text(face = "bold", size = 14, colour = "grey10"),
        strip.text.y = element_text(face = "bold", size = 14, colour = "grey10", angle = 180),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0,"in"),
        panel.background = element_rect(fill = NA, color = "gray90"))

################
# Define non-reactive functions (used by reactive conductors). This should keep
# from needlessly querying/crunching the data multiple times when an update is
# made. The raw data is needed for both visualizations, but we only need to pull
# it once.
################

# Get the values to be used for the X dimension. This is based on the *totals* for
# the selected metric *just* for the X dimension values (ignores the Y-dimension)
# values -- sorting all of the results and then taking the top values based on
# the slider selection. 
calc_dim_x_includes <- function(data, dim_count){
  group_by(data, dim_x) %>% summarise(total = sum(metric)) %>%
    arrange(-total) %>% top_n(dim_count, total) %>% select(dim_x)
}

# This does the same as the above, but for the Y dimension. These probably could
# have been a single function, as they're very similar, but it seemed a little
# more readable just to do them separately.
calc_dim_y_includes <- function(data, dim_count){
  group_by(data, dim_y) %>% summarise(total = sum(metric)) %>%
    arrange(total) %>% top_n(dim_count, total) %>% select(dim_y) 
}

get_data <- function(daterange, metric, x_dim, y_dim){
  
#  browser()
  
  # Calculate the start and end dates.
  start_date <- as.character(Sys.Date()-as.numeric(daterange)-1)
  end_date <- as.character(Sys.Date()-1)
  
  # Set up the dimensions being used. If you trace this back, x_dim and y_dim
  # are fed by a reactive conductor.
  dimensions <- c("ym:s:date",x_dim, y_dim)
  
  metric <- metric
  
  # Pull the data.
  ym_data <- GET("https://api-metrika.yandex.ru/stat/v1/data",
                  query = list (ids = counter_id,
                                metrics = metric,
                                date1 = start_date,
                                date2 = end_date,
                                dimensions = paste(dimensions, collapse = ","),
                                lang = lang,
                                oauth_token = token))
 
  # What gets returned has more in it than we really want, so we need to get
  # just the *content* of the request and then pretty that up a bit
  ym_data <- content(ym_data)
  ym_data <- ym_data$data 
  

  # Depending on the dimensions being brought back, the columns remaining will vary how many values.
  # So, this is a bit of legerdemaine to name them so we can then go in and strip out what we want.
  new_names <- c(paste0("dim0_",names(ym_data[[1]]$dimensions[[1]])),
                 paste0("dim1_",names(ym_data[[1]]$dimensions[[2]])),
                 paste0("dim2_",names(ym_data[[1]]$dimensions[[3]])),
                 "metric")
  
  # Convert that data to a data frame. 
  ym_data <- data.frame(matrix(unlist(ym_data), 
                                   nrow=length(ym_data), byrow=T),stringsAsFactors=FALSE) 
  
  # Rename the columns to be slightly more descriptive
  names(ym_data) <- new_names
  
  # Drop a few columns that are meta data that we're not going to use (for now)
  ym_data <- select(ym_data, dim0_name, dim1_name, dim2_name, metric)
  
  # Rename the columns to be generic names -- that just makes it easier
  # for all future transformations
  colnames(ym_data) <- c("date","dim_x","dim_y","metric")
  
  ym_data$date <- as.Date(ym_data$date)
  ym_data$metric <- as.numeric(ym_data$metric)
  
  ym_data <- arrange(ym_data, date)
  
  # We want to return the entire data frame -- not just the column names,
  # so throw the data frame here as the last object in the function.
  # ym_data
  
}

###################
# Authorize Yandex Metrica
# Lot's o' Greek to me.

# This is fine -- not really *used* for OAuth, part of the oauth_app() call
app_name <- "yandex_metrica"

# I'm not sure if this is actually getting used in the right place... but it seems to work
resource_uri <- "https://oauth.yandex.com/authorize?response_type=code"

# Generate the "endpoint." This should prompt as to whether to generate
# a .httr-oauth file to use as a future reference.
yandex_endpoint <- oauth_endpoint(authorize = "https://oauth.yandex.com/authorize?response_type=code",
                                  access = "https://oauth.yandex.com/token?response_type=code")

# Create the "app?" Maybe?
myapp <- oauth_app(app_name,
                   key = client_id,
                   secret = pwd)

# Get the full token.
mytoken <- oauth2.0_token(yandex_endpoint, myapp,
                          user_params = list(resource = resource_uri),
                          use_oob = FALSE)

# Get the actual token string to be used in the query(ies)
token <- mytoken$credentials$access_token

# token <- "AQAAAAAb3ZbmAAQXSn__Q5fuT0IHoO1gqRKJZEY"

######################
# Define the UI
######################

ui <- fluidPage(
  
  theme = "cosmo",   # Change to a different theme to slightly alter the look and feel as desired
  
  # Application title
  titlePanel("Yandex Metrica 2-D Data Explorer"),
  
  # Sidebar with the user-controllable inputs 
  sidebarLayout(
    sidebarPanel(
      
      # The date range dropdown, including a default value
      selectInput("daterange", label = "Select a date range:", 
                  choices = daterange_options, 
                  selected = 30),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # The metric dropdown
      selectInput("metric", label = "Select a metric:", 
                  choices = metric_options, 
                  selected = "ym:s:visits"),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # The dimension selector (dropdown) for the X-dimension
      selectInput("x_dim", label = "Select the X dimension and how many values to show:", 
                  choices = dimension_options, 
                  selected = "ym:s:browser"),
      
      # Select the max number of values to show in the X dimension
      sliderInput("dim_x_count",
                  label = NULL,
                  min = 1,
                  max = 10,
                  value = 3),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # The dimension selector (dropdown) for the X-dimension
      selectInput("y_dim", label = "Select the Y dimension and how many values to show:", 
                  choices = dimension_options, 
                  selected = "ym:s:lastTrafficSourceName"),
      
      # Select the max number of values to show in the Y dimension
      sliderInput("dim_y_count",
                  label = NULL,
                  min = 1,
                  max = 10,
                  value = 5)
    ),
    
    # Show the heatmap and sparklines
    mainPanel(
      tags$h4("Heatmap of Metric Totals"),
      plotOutput("heatmap"),
      tags$hr(),
      tags$h4("Daily Trendlines for the Metric"),
      plotOutput("sparklines")
    )
  )
)

################
# Define server logic
################

server <- function(input, output) {
  
  # Reactive conductor to get the base data for use elsewhere
  base_data <- reactive({get_data(input$daterange, input$metric, input$x_dim, input$y_dim)})
  
  # We want to get the "top X" values for each dimension based on the dim_x_count 
  # settings. We're going to use these values in a few places, so we're setting them
  # up as reactive conductors
  dim_x_includes <- reactive({calc_dim_x_includes(base_data(), input$dim_x_count)})
  dim_y_includes <- reactive({calc_dim_y_includes(base_data(), input$dim_y_count)})
  
  # Build the heatmap
  output$heatmap <- renderPlot({
    
    x_includes <- dim_x_includes()
    y_includes <- dim_y_includes()
    
    # Filter the data down to just include the "top X" dimensions (for both values) and
    # then total them up.
    plot_totals <- base_data() %>% as.data.frame() %>% 
      filter(dim_x %in% x_includes$dim_x, dim_y %in% y_includes$dim_y) %>%
      group_by(dim_x, dim_y) %>% 
      summarise(total = sum(metric))
    
    # Make the totals heatmap
    ggplot(plot_totals, aes(dim_x, dim_y)) + 
      geom_tile(aes(fill = total), color="white", size = 1) +
      scale_fill_gradient(low = "white", high = "green") +
      scale_x_discrete(limits = x_includes$dim_x) +
      scale_y_discrete(limits = y_includes$dim_y) +
      geom_text(aes(label = comma(total)), size = 7) +
      default_theme +
      theme(panel.border = element_rect(fill=NA, colour = "white"))
  })
  
  output$sparklines <- renderPlot({
    
    x_includes <- dim_x_includes()
    y_includes <- dim_y_includes()
    
    plot_trends <- filter(base_data(), dim_x %in% x_includes$dim_x, dim_y %in% y_includes$dim_y)
    
    # To control the facet order, we need to change dim1 and dim2 to be factors. The
    # order comes from the dim_x_includes$dimx values
    plot_trends$dim_x <- factor(plot_trends$dim_x,
                                levels = as.character(x_includes$dim_x))
    
    plot_trends$dim_y <- factor(plot_trends$dim_y,
                                levels = as.character(rev(y_includes$dim_y)))
    
    # Generate the actual plot. Ideally, the strip labels on the y-axis would be
    # right-justified, but this turns out to be trickier than you would think, and
    # the solutions I found to make that happen...I couldn't get to work.
    ggplot(plot_trends, aes(date, metric)) +
      geom_line() +
      facet_grid(dim_y~dim_x,
                 switch = "both") +
      default_theme +
      theme(axis.text = element_blank())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

