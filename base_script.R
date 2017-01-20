library(googleAnalyticsR)
library(tidyverse)
library(scales) # Cuz, ya' know, commas in displayed numbers
library(plotly)

# Settings
start_date <- as.character(Sys.Date()-31)
end_date <- as.character(Sys.Date()-1)

# View ID
view_id <- "ga:66876757"

# List of metrics that can be used
metric_options <- c("sessions","pageviews","totalEvents")

# List of dimensions that can be used
dimension_options <- data.frame(name = c("Default Channel Grouping",
                                         "Source",
                                         "Medium",
                                         "Campaign",
                                         "Device Category"),
                                api_name = c("channelGrouping",
                                              "source",
                                              "medium",
                                              "campaign",
                                              "deviceCategory"),
                                stringsAsFactors = FALSE)

dim_1 <- dimension_options$api_name[5]
dim_2 <- dimension_options$api_name[1]

# Set the number of unique values to include for each dimensions
dim_1_count <- 8
dim_2_count <- 5

dimensions <- c("date",dim_1,dim_2)
metric <- metric_options[2]


# ---- default-theme ----
####################
# Define base theme You have an option of tweaking settings
# here OR in the actual functions/output that use default_theme
####################

default_theme <-   theme_bw() +
  theme(axis.text = element_text(face = "bold", size = 10, colour = "grey10"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        legend.justification = "center",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(face = "bold", size = 10, colour = "grey10"),
        strip.text.y = element_text(face = "bold", size = 10, colour = "grey10", angle = 180),
        strip.background = element_blank(),
        panel.spacing = unit(0,"in"),
        panel.background = element_rect(fill = NA, color = "gray90"))

# Authorize Google Analytics
ga_auth()

# Pull the data
ga_data <- google_analytics_4(viewId = view_id,
                              date_range = c(start_date,end_date),
                              metrics = metric,
                              dimensions = dimensions,
                              anti_sample = TRUE)

# Rename the columns to be generic names -- that just makes it easier
# for all future transformations
colnames(ga_data) <- c("date","dim1","dim2","metric")

# We want to get the "top X" values for each dimension based on the dim_x_count 
# settings.
dim_1_includes <- group_by(ga_data, dim1) %>% summarise(total = sum(metric)) %>%
  arrange(-total) %>% top_n(dim_1_count, total) %>% select(dim1) 

dim_2_includes <- group_by(ga_data, dim2) %>% summarise(total = sum(metric)) %>%
  arrange(total) %>% top_n(dim_2_count, total) %>% select(dim2) 

# Filter ga_data to only include rows where both dim_1 and dim_2 made the cut.
# This is the data that will be plotted as trendlines.
plot_trends <- filter(ga_data, dim1 %in% dim_1_includes$dim1, dim2 %in% dim_2_includes$dim2)

# Total up the metrics. This is what is used for the totals heatmap
plot_totals <- group_by(plot_trends, dim1, dim2) %>% summarise(total = sum(metric))

# Make the totals heatmap
heatmap <- ggplot(plot_totals, aes(dim1, dim2)) + 
  geom_tile(aes(fill = total), color="white") +
  scale_fill_gradient(low = "white", high = "green") +
  scale_x_discrete(limits = dim_1_includes$dim1) +
  scale_y_discrete(limits = dim_2_includes$dim2) +
  geom_text(aes(label = comma(total))) +
  default_theme

# Make the sparklines

# To control the facet order, we need to change dim1 and dim2 to be factors. The
# order comes from the dim_x_includes$dimx values
plot_trends$dim1 <- factor(plot_trends$dim1,
                           levels = as.character(dim_1_includes$dim1))

plot_trends$dim2 <- factor(plot_trends$dim2,
                           levels = as.character(rev(dim_2_includes$dim2)))


sparklines <- ggplot(plot_trends, aes(date, metric)) +
  geom_line() +
  facet_grid(dim2~dim1,
             switch = "both") +
  default_theme +
  theme(axis.text = element_blank())
