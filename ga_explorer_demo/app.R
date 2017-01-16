#
# App to explore Google Analytics metrics across two dimensions
#

library(shiny)
library(googleAnalyticsR)
library(tidyverse)
library(scales) # Cuz, ya' know, commas in displayed numbers
library(plotly)

# Get the Google Analytics View ID from the .Renviron file. This is
# just a text file that has one line:
# GA_VIEW_ID="[the actual view ID]"
# Don't include the brackets, but do include the quotation marks.
# You can also simply hardcode the view ID rather than using Sys.getenv
# below, but that's less GitHub-open-sharing-friendly.
view_id <- Sys.getenv("GA_VIEW_ID")

# Hardcode a bunch of settings for now

# Settings
start_date <- as.character(Sys.Date()-31)
end_date <- as.character(Sys.Date()-1)

# List of metrics that can be used
metric_options <- c("sessions","pageviews","totalEvents")

# List of dimensions that can be used
dimension_options <- list("New vs. Returning" = "userType",
                          "Device Category" = "deviceCategory",
                          "Mobile Device" = "mobileDeviceBranding",
                          "Browser" = "browser",
                          "Operating System" = "operatingSystem",
                          "Default Channel Grouping" = "channelGrouping",
                          "Source" = "source",
                          "Medium" = "medium",
                          "Campaign" = "campaign")


metric <- metric_options[2]

####################
# Define base theme You have an option of tweaking settings
# here OR in the actual functions/output that use default_theme
####################

default_theme <-   theme_bw() +
  theme(axis.text = element_text(face = "bold", size = 11, colour = "grey10"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        legend.justification = "center",
        strip.text.x = element_text(face = "bold", size = 11, colour = "grey10"),
        strip.text.y = element_text(face = "bold", size = 11, colour = "grey10", angle = 180),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0,"in"),
        panel.background = element_rect(fill = NA, color = "gray90"))

################
# Define non-reactive functions (used by reactive conductors)
################

# We want to get the "top X" values for each dimension based on the dim_x_count 
# settings.
calc_dim_x_includes <- function(data, dim_count){
  group_by(data, dim_x) %>% summarise(total = sum(metric)) %>%
    arrange(-total) %>% top_n(dim_count, total) %>% select(dim_x)
}

# We want to get the "top X" values for each dimension based on the dim_y_count 
# settings.
calc_dim_y_includes <- function(data, dim_count){
  group_by(data, dim_y) %>% summarise(total = sum(metric)) %>%
    arrange(total) %>% top_n(dim_count, total) %>% select(dim_y) 
}

get_data <- function(x_dim,y_dim){
  
  # Set up the dimensions being used. If you trace this back, x_dim and y_dim
  # are fed by a reactive conductor.
  dimensions <- c("date",x_dim, y_dim)
  
  # Pull the data. For now, this is hardcoded. Ultimately, it will need to be a
  # reactive function in server.ui that triggers when a dropdown gets changed.
  ga_data <- google_analytics_4(viewId = view_id,
                                date_range = c(start_date,end_date),
                                metrics = metric,
                                dimensions = dimensions,
                                anti_sample = TRUE)
  
  # Rename the columns to be generic names -- that just makes it easier
  # for all future transformations
  colnames(ga_data) <- c("date","dim_x","dim_y","metric")
  
  ga_data
  
}

# Authorize Google Analytics
ga_auth()

######################
# Define the UI
######################

ui <- fluidPage(
  
  # Application title
  titlePanel("Google Analytics 2-D Data Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("x_dim", label = "Select the X dimension and how many values to show:", 
                  choices = dimension_options, 
                  selected = "deviceCategory"),
      
      # Select the max number of values to show in the X dimension
      sliderInput("dim_x_count",
                  "",
                  min = 1,
                  max = 5,
                  value = 3),
    
      selectInput("y_dim", label = "Select the Y dimension and how many values to show:", 
                  choices = dimension_options, 
                  selected = "userType"),
      
      # Select the max number of values to show in the Y dimension
      sliderInput("dim_y_count",
                  "",
                  min = 1,
                  max = 8,
                  value = 2)
    ),
    
    # Show the heatmap and sparklines
    mainPanel(
      plotOutput("heatmap"),
      tags$hr(),
      plotOutput("sparklines")
    )
  )
)

################
# Define server logic
################

server <- function(input, output) {
  
  # Reactive conductor to get the base data for use elsewhere
  base_data <- reactive({get_data(input$x_dim,input$y_dim)})
  
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
      geom_tile(aes(fill = total), color="white") +
      scale_fill_gradient(low = "white", high = "green") +
      scale_x_discrete(limits = x_includes$dim_x) +
      scale_y_discrete(limits = y_includes$dim_y) +
      geom_text(aes(label = comma(total))) +
      default_theme
    
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

