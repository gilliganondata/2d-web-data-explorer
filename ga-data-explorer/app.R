#
# App to explore Google Analytics metrics across two dimensions. The user has to authenticate
# and choose a view.
#

# Two things required to set this up yourself:
#
# 1. You'll need to create your own Google Developer Console project. Donal Phipps has put
#    together a nice video with instructions here: https://www.youtube.com/watch?v=4B88dRbwNfc
#
# 2. Set up three variables in a .Renviron file: GAR_WEB_CLIENTID and GAR_WEB_CLIENT_SECRET
#    come from the project you set up in the previous step. Then, set GAR_SCOPES as
#    "https://www.googleapis.com/auth/analytics.readonly"

# If running locally and want to specify a port so that you don't have to keep updating
# the Google Developer Console project credentials, uncomment.
# options(shiny.port = 5893)

library(shiny)              # We must web-enable this whole thing

# There's a wrinkle for running this on shinyapps.io that requires using the Github
# version of googleAuthR. See: https://twitter.com/ryanpraski/status/783754506681155584
library(googleAuthR)        # To prompt for authentication by the user

library(googleAnalyticsR)   # For the pulling of the data
library(tidyverse)          # For data transformations -- primarily just uses dplyr commands
library(scales)             # Cuz, ya' know, commas in displayed numbers

####################
# Set up the different options for interaction
####################

# DATE OPTIONS
# This could also be set as date selectors easily enough, but, for now, it's just set
# as some preset options. As a note, even though the values are being set as numerics 
# here, they actually get treated as characters, so they have to be converted 
# back to numerics when setting start_date in the get_data() function.
daterange_options <- list("Last 7 Days" = 7,
                          "Last 30 Days" = 30,
                          "Last 60 Days" = 60,
                          "Last 90 Days" = 90)

# METRIC OPTIONS
# As currently written, these can only be summable metrics, as the data gets pulled
# once as daily data and then gets aggregated (and would require much more hoop-
# jumping to then do weighted averages to get 'rates' of any sort).
metric_options <- list("Sessions" = "sessions",
                       "Pageviews" = "pageviews",
                       "Total Events" = "totalEvents",
                       "Bounces" = "bounces",
                       "New Users" = "newUsers")

# DIMENSION OPTIONS
# The first value can be anything, but the second value needs to be the Google 
# Analytics API value.
dimension_options <- list("New vs. Returning" = "userType",
                          "Device Category" = "deviceCategory",
                          "Mobile Device Brand" = "mobileDeviceBranding",
                          "Country" = "country",
                          "Browser" = "browser",
                          "Operating System" = "operatingSystem",
                          "Default Channel Grouping" = "channelGrouping",
                          "Source" = "source",
                          "Medium" = "medium",
                          "Campaign" = "campaign")

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

######################
# Define the UI
######################

ui <- fluidPage(
  
  # Include Google Tag Manager container (put the file in the same
  # folder as app.R). Remove the <script> tags and the comments from the GTM script.
  tags$head(includeScript("gtm.js")),
  
  theme = "cosmo",   # Change to a different theme to slightly alter the look and feel as desired
  
  # Application title
  titlePanel("Google Analytics 2-D Data Explorer"),
  "Log in and select a view to explore how a metric breaks down across two dimensions.",
  "Adjust the sliders to vary the number of values to include in the display, and adjust",
  "the dropdowns to change the dimensions and metric being used. The source code for this",
  "app is available on ",
  tags$a(href="https://github.com/gilliganondata/2d-web-data-explorer","Github"),
  ".",
  
  tags$hr(),
  
  # Sidebar with the user-controllable inputs 
  sidebarLayout(
    sidebarPanel(
      
      # A bit of a hack, I suspect, but just centering the login button
      tags$div(style="text-align: center;",
               
               # Get the user to log in and then select a view. This uses the JS version
               # because it seems to be less finicky.
               gar_auth_jsUI("auth_module", login_text = "Login with Google")
      ),
      
      # Get the account/property/view
      authDropdownUI("auth_menu"),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # The date range dropdown, including a default value
      selectInput("daterange", label = "Select a date range:", 
                  choices = daterange_options, 
                  selected = 30),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # The metric dropdown
      selectInput("metric", label = "Select a metric:", 
                  choices = metric_options, 
                  selected = "sessions"),
      
      # Horizontal line just to break up the settings a bit.
      tags$hr(style="border-color: #777777;"),
      
      # The dimension selector (dropdown) for the X-dimension
      selectInput("x_dim", label = "Select the X dimension and how many values to show:", 
                  choices = dimension_options, 
                  selected = "deviceCategory"),
      
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
                  selected = "channelGrouping"),
      
      # Select the max number of values to show in the Y dimension
      sliderInput("dim_y_count",
                  label = NULL,
                  min = 1,
                  max = 10,
                  value = 6)),
    
    # Show the heatmap and sparklines
    mainPanel(
      tags$h4("Heatmap of Metric Totals"),
      plotOutput("heatmap"),
      tags$br(),
      tags$em(textOutput("chi_square")),
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
  
  # Get the view ID (user-selected). I'd be lying if I said I fully understood this
  # piece -- pretty much lifted it straight from Mark Edmondson's example at:
  # http://code.markedmondson.me/googleAnalyticsR/shiny.html. Except... used the
  # JS option: https://mark.shinyapps.io/googleAuthRMarkdown/
  access_token <- callModule(gar_auth_js, "auth_module")
  
  # Get the accounts list
  ga_account <- reactive({
    validate(
      need(access_token(), "Authenticate")
    )
    
    with_shiny(ga_account_list, shiny_access_token = access_token())
  })
  
  view_id <- callModule(authDropdown, "auth_menu", ga.table = ga_account)
  
  # Function to actually pull the data. I'm a little concerned that this may be
  # running twice every time a new API call is triggered -- had it working
  # as a reactive conductor function originally so this wouldn't be the case,
  # but I couldn't manage to keep that structure once I introduced the Google
  # login.
  base_data <- reactive({
    
    # Calculate the start and end dates.
    start_date <- as.character(Sys.Date()-as.numeric(input$daterange))
    end_date <- as.character(Sys.Date()-1)
    
    # Set up the dimensions being used. If you trace this back, x_dim and y_dim
    # are fed by a reactive conductor.
    dimensions <- c("date",input$x_dim, input$y_dim)
    
    # Pull the data.
    ga_data <- with_shiny(google_analytics_4,
                          viewId = view_id(),
                          date_range = c(start_date, end_date),
                          metrics = input$metric,
                          dimensions = dimensions,
                          anti_sample = TRUE,
                          shiny_access_token = access_token())
    
    # Rename the columns to be generic names -- that just makes it easier
    # for all future transformations
    colnames(ga_data) <- c("date","dim_x","dim_y","metric")
    
    # We want to return the entire data frame -- not just the column names,
    # so throw the data frame here as the last object in the function.
    ga_data
    
  })
  
  # We want to get the "top X" values for each dimension based on the dim_x_count 
  # settings. We're going to use these values in a few places, so we're setting them
  # up as reactive conductors
  dim_x_includes <- reactive({calc_dim_x_includes(base_data(), input$dim_x_count)})
  dim_y_includes <- reactive({calc_dim_y_includes(base_data(), input$dim_y_count)})
  
  # Build the heatmap
  output$heatmap <- renderPlot({
    
    # Make sure an access token is present before trying to render anything
    req(access_token())
    
    x_includes <- dim_x_includes()
    y_includes <- dim_y_includes()
    
    plot_data <- base_data()
    
    # Filter the data down to just include the "top X" dimensions (for both values) and
    # then total them up. The spread -> gather move at the end is to
    # get 0s in the empty cells.
    plot_totals <- plot_data %>% as.data.frame() %>% 
      filter(dim_x %in% x_includes$dim_x, dim_y %in% y_includes$dim_y) %>%
      group_by(dim_x, dim_y) %>% 
      summarise(total = sum(metric)) %>% 
      spread(dim_x, total, fill=0) %>%
      gather(dim_x, total, -dim_y)
      
    
    # Make the totals heatmap. 
    ggplot(plot_totals, aes(dim_x, dim_y)) + 
      geom_tile(aes(fill = total), color="white", size = 1) +
      scale_fill_gradient(low = "white", high = "green") +
      scale_x_discrete(limits = x_includes$dim_x) +
      scale_y_discrete(limits = y_includes$dim_y) +
      geom_text(aes(label = comma(total)), size = 7) +
      default_theme +
      theme(panel.border = element_rect(fill=NA, colour = "white"))
  })
  
  # Run a Chi-square test for independence to determine if the two factors (dimensions)
  # have any apparent relationship
  output$chi_square <- renderText({
    
    # Make sure an access token is present before trying to render anything
    req(access_token())
    
    x_includes <- dim_x_includes()
    y_includes <- dim_y_includes()
    
    plot_data <- base_data()
    
    # Filter the data down to just include the "top X" dimensions (for both values) and
    # then total them up. Then, spread them out to be an actual cross-tab so we
    # can run the Chi-square test
    plot_totals <- plot_data %>% as.data.frame() %>% 
      filter(dim_x %in% x_includes$dim_x, dim_y %in% y_includes$dim_y) %>%
      group_by(dim_x, dim_y) %>% 
      summarise(total = sum(metric)) %>%
      spread(dim_x, total, fill=0)
    
    chi_sq <- chisq.test(plot_totals[,-1])
    
    if(chi_sq$p.value < 0.000005){
      result <- "are NOT independent of each other (p-value < 0.000005). "
    } else {
      if(chi_sq$p.value < 0.05){
        result <- paste0("are NOT independent of each other (p-value = ",round(chi_sq$p.value,5),"). ")
      } else {
        result <- paste0("ARE independent of each other (p-value = ",round(chi_sq$p.value,5),"). ")
      }
    }
        
    paste("Based on a Pearson Chi-square test with an alpha level of 0.05, it appears", input$x_dim, "and", input$y_dim, result,
          "Keep in mind that, with large values and a high number of dimensions (levels) it can be very hard to discard the ",
          "null hypothesis of independence (I'm still wrestling with this!).")
    
  })
  
  # Build the sparklines to see the data trended
  output$sparklines <- renderPlot({
    
    # Make sure an access token is present before trying to render anything
    req(access_token())
    
    x_includes <- dim_x_includes()
    y_includes <- dim_y_includes()
    
    plot_data <- base_data()
    
    # Get the desired data and then convert date to numeric so ggplot doesn't
    # get too fancy with the x-axis (the values aren't displayed, so a simple
    # numeric means it will just show them all)
    plot_trends <- filter(plot_data, dim_x %in% x_includes$dim_x, dim_y %in% y_includes$dim_y)
    
    # To control the facet order, we need to change dim1 and dim2 to be factors. The
    # order comes from the dim_x_includes$dimx values
    plot_trends$dim_x <- factor(plot_trends$dim_x,
                                levels = as.character(x_includes$dim_x))
    
    plot_trends$dim_y <- factor(plot_trends$dim_y,
                                levels = as.character(rev(y_includes$dim_y)))
    
    # Generate the actual plot. Ideally, the strip labels on the y-axis would be
    # right-justified, but this turns out to be trickier than you would think, and
    # the solutions I found to make that happen...I couldn't get to work.
    ggplot(plot_trends, aes(x = date, y = metric)) +
      geom_line() +
      facet_grid(dim_y~dim_x,
                 switch = "both") +
      default_theme
    # +
    #   theme(axis.text = element_blank())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

