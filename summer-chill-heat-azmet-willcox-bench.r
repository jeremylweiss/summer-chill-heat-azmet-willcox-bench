

# This code generates an exploratory analysis of minimum and maximum hourly
# temperature values based on historical observations at the AZMET Willcox Bench 
# station. Focus will be on temperatures below 60F and above 95F.

# AZMET data are at: https://cals.arizona.edu/azmet/

# Author:
# Jeremy Weiss, Climate and Geospatial Extension Scientist
# School of Natural Resources and the Environment
# University of Arizona
# 520-626-8063, jlweiss@email.arizona.edu

# Ideas for development: add yearly bar graph of precipitation events / amounts
# to gauge coincidence of summer chill and rainfall


# SETUP --------------------


# Load needed libraries
library("dplyr")
library("reshape2")
library("ggplot2")
library("lubridate")
library("extrafont")
#library("grid")
#library("png")

# Load additional font options for plotting
font_import()
y
loadfonts(device = "postscript")


# DOWNLOAD AND TRANSFORM HOURLY AZMET DATA --------------------


# Load AZMET station list
stn_list <- read.csv("azmet-station-list.csv", sep = ",")

# Set the AZMET station name and years of interest
stn_name <- "Willcox Bench"
yr_start <- stn_list$start_yr[which(stn_list$stn == stn_name)]
yr_end <- stn_list$end_yr[which(stn_list$stn == stn_name)]

# Load function to download and transform daily AZMET data
source("azmet.hourly.data.download.R")

stn_data_hourly <- azmet.hourly.data.download(stn_name)

# Retain necessary variables
stn_data_hourly <- select(stn_data_hourly, DateTime, Date, Year, Month, Day, 
                          JDay, Hour, Temp)

# Filter hourly data to the months of June - September
stn_data_hourly <- filter(stn_data_hourly, Month >=6 & Month <= 9)

# Convert temperature from Celsius to Fahrenheit
stn_data_hourly$Temp <- (1.8 * stn_data_hourly$Temp) + 32

# Set and define temperature categories
stn_data_hourly["below55"] <- NA
stn_data_hourly["bet5560"] <- NA
stn_data_hourly["bet6065"] <- NA
stn_data_hourly["bet95100"] <- NA
stn_data_hourly["bet100105"] <- NA
stn_data_hourly["above105"] <- NA

measurements <- which(is.na(stn_data_hourly$Temp) == FALSE)

stn_data_hourly$below55[measurements] <- 0
stn_data_hourly$bet5560[measurements] <- 0
stn_data_hourly$bet6065[measurements] <- 0
stn_data_hourly$bet95100[measurements] <- 0
stn_data_hourly$bet100105[measurements] <- 0
stn_data_hourly$above105[measurements] <- 0

rm(measurements)

below55 <- which(stn_data_hourly$Temp < 55)

stn_data_hourly$below55[below55] <- 1

rm(below55)

bet5560 <- which(stn_data_hourly$Temp >= 55 & stn_data_hourly$Temp < 60)

stn_data_hourly$bet5560[bet5560] <- 1

rm(bet5560)

bet6065 <- which(stn_data_hourly$Temp >= 60 & stn_data_hourly$Temp < 65)

stn_data_hourly$bet6065[bet6065] <- 1

rm(bet6065)

bet95100 <- which(stn_data_hourly$Temp >= 95 & stn_data_hourly$Temp < 100)

stn_data_hourly$bet95100[bet95100] <- 1

rm(bet95100)

bet100105 <- which(stn_data_hourly$Temp >= 100 & stn_data_hourly$Temp < 105)

stn_data_hourly$bet100105[bet100105] <- 1

rm(bet100105)

above105 <- which(stn_data_hourly$Temp >= 105)

stn_data_hourly$above105[above105] <- 1

rm(above105)

# Aggregate hourly data to daily data, summing the temperature categories
stn_data_daily <- stn_data_hourly %>%
  group_by(Year, JDay, Date, Month, Day) %>%
  summarize(below55 = sum(below55, na.rm = TRUE), 
            bet5560 = sum(bet5560, na.rm = TRUE),
            bet6065 = sum(bet6065, na.rm = TRUE),
            bet95100 = sum(bet95100, na.rm = TRUE),
            bet100105 = sum(bet100105, na.rm = TRUE),
            above105 = sum(above105, na.rm = TRUE))

# Transform daily data from wide to long
stn_data_daily <- melt(data = stn_data_daily,
                       id.vars = colnames(stn_data_daily)[1:5],
                       measure.vars = colnames(stn_data_daily[6:11]),
                       variable.name = "TempCode",
                       value.name = "Hours",
                       na.rm = FALSE)


# PLOT FOR DAILY VALUES OF TEMPERATURE CATEGORIES  --------------------


# To facilitate graphing interannual data that includes leap years, subtract 1
# from 'JDay' values in a leap year. The graph focus in on summer months, so 
# there won't be an issue with plotting data in January and February.
stn_data_daily$JDay[which(leap_year(stn_data_daily$Year) == TRUE)] <- 
  stn_data_daily$JDay[which(leap_year(stn_data_daily$Year) == TRUE)] - 1

stn_data_daily$TempCode <- factor(stn_data_daily$TempCode,
                                  levels = c(
                                    "above105", "bet100105", "bet95100",
                                    "bet6065", "bet5560", "below55"
                                  ),
                                  labels = c(
                                    "> 105°F", "100-105°F", "95-100°F",
                                    "60-65°F", "55-60°F", "< 55°F"
                                  ))

category_colors <- c(
  "> 105°F" = "#e31a1c", "100-105°F" = "#fd8d3c", "95-100°F" = "#fecc5c",
  "60-65°F" = "#b3cde3", "55-60°F" = "#8c96c6", "< 55°F" = "#88419d"
)

p <- ggplot() +
  geom_col(
    data = stn_data_daily,
    mapping = aes(x = JDay, y = Hours, fill = TempCode),
    position = "stack") +
  
  scale_color_manual(values = category_colors,
                     aesthetics = c("color", "fill"),
                     name = "Temperature \nRange") +
  
  facet_wrap(~ Year, ncol = 1) +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(
    breaks = c(152, 166, 182, 196, 213, 227, 244, 258),
    labels = c("Jun 1", "Jun 15","Jul 1", "Jul 15",
               "Aug 1", "Aug 15", "Sep 1", "Sep 15"),
    limits = c(181, 274),
    expand = c(0.0, 0.0)
  ) +
  
  scale_y_continuous(
    breaks = seq(from = 0, to = 24, by = 6),
    #limits = c(0, max(stn_data_daily$Hours, na.rm = TRUE)),
    #minor_breaks = seq(from = 0, to = 24, 
    #                   #to = max(stn_data_daily$Hours, na.rm = TRUE), 
    #                   by = 1),
    expand = c(0.06, 0.0)
  ) +
  
  # Add the graph title, subtitle, and axis labels
  ggtitle("Hours per Day in Temperature Ranges,  July - September") +
  labs(subtitle = "AZMET Willcox Bench Station, Cochise County, Arizona",
    x = "\nDate",
    y = "Hours\n",
    caption = "\nWillcox Bench data from Arizona Meteorological Network (cals.arizona.edu/azmet)") +
  
  # Further customize the figure appearance
  theme_light(base_family = "Source Sans Pro") +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(color = "gray40", size = 10),
        axis.text.y = element_text(color = "gray40", size = 10),
        axis.ticks.x.bottom = element_line(color = "gray80", size = 0.25),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(2.0, "mm"),
        axis.ticks.length.y = unit(0.0, "mm"),
        axis.title.x = element_text(color = "gray40", size = 10),
        axis.title.y = element_text(color = "gray40", size = 10),
        legend.direction = "vertical",
        legend.text = element_text(color = "gray40", size = 10),
        legend.title = element_text(color = "gray40", size = 10, face = "bold"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.25),
        panel.grid.major.y = element_line(color = "gray80", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(color = "gray40", hjust = 0.0, size = 7),
        plot.caption.position = "plot",
        plot.margin = unit(c(1, 1 ,1, 1), "mm"),
        plot.subtitle = element_text(family = "Source Serif Pro", size = 12), 
        plot.title = element_text(
          face = "bold", family = "Source Serif Pro", size = 16
        ),
        plot.title.position = "plot",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(color = "gray40", size = 12, face = "bold")
  )
p

# Add logo to plot (https://www.markhw.com/blog/logos)
#get_png <- function(filename) {
#  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
#}

#logo <- get_png("Climate-Geospatial-Environmental-Modeling_PRIMARY_EMAIL.png")

#l <- ggplot(mapping = aes(x = 0:1, y = 1)) +
#  theme_void() +
#  annotation_custom(logo, xmin = .67, xmax = 1)
#l

#gridExtra::grid.arrange(p, l, heights = c(.93, .07))

# Save the figure
ggsave(file = paste0("summer-chill-heat-azmet-willcox-bench-", 
                     Sys.Date(),
                     ".eps"),
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 9, units = "in", dpi = 300) 

