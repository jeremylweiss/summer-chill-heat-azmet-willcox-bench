

# This code generates an exploratory analysis of minimum and maximum hourly
# temperature values based on historical observations at the AZMET Willcox Bench 
# station. Focus will be on temperatures below 60F and above 95F.

# AZMET data are at: https://cals.arizona.edu/azmet/

# Author:
# Jeremy Weiss, Climate and Geospatial Extension Scientist
# School of Natural Resources and the Environment
# University of Arizona
# 520-626-8063, jlweiss@email.arizona.edu


# SETUP --------------------


# Load needed libraries
library("dplyr")
library("reshape2")
library("ggplot2")
#library("lubridate")
library("extrafont")

# Load additional font options for plotting
font_import()
y
loadfonts(device = "postscript")

# Load AZMET station list
stn_list <- read.csv("azmet-station-list.csv", sep = ",")

# Set the AZMET station name and years of interest (if different from actual
# station data coverage)
stn_name <- "Willcox Bench"
yr_start <- stn_list$start_yr[which(stn_list$stn == stn_name)]
yr_end <- stn_list$end_yr[which(stn_list$stn == stn_name)]

# Load function to download and transform daily AZMET data
source("azmet.hourly.data.download.R")


# DOWNLOAD AND TRANSFORM DAILY AZMET DATA --------------------


#stn_data_hourly <- azmet.daily.data.download(stn_name)

# Retain necessary variables and data
#stn_data_hourly <- select(stn_data_hourly, date, year, month, day, doy, Tmax, Tmin, PRCtot)
#stn_data_hourly <- filter(stn_data_hourly, year >= yr_start & year <= yr_end)

# Convert temperatures from Celsius to Fahrenheit, and precipitation from 
# millimeters to inches
#stn_data_hourly$Tmax <- (1.8 * stn_data_hourly$Tmax) + 32
#stn_data_hourly$Tmin <- (1.8 * stn_data_hourly$Tmin) + 32
#stn_data_hourly$PRCtot <- stn_data_hourly$PRCtot / 25.4


# BEFORE FUNCTION IS DEVELOPED -----


# Data from: https://cals.arizona.edu/azmet/az-data.htm. The following is set 
# for data from the Willcox Bench station.
data_dir <- 
  "C:/Users/jlweiss/OneDrive - University of Arizona/Documents/R/datastore/azmet/"

# Data collection at the Willcox Bench station started in middle 2016 and 
# continues at present. 'obs_yrs' format matches data filename convention.
obs_yrs <- seq(16, 20)

# Data column names are from: https://cals.arizona.edu/azmet/raw2003.htm
col_names <- 
  c("Year", "JDay", "Hour", "Temp", "rh", "vpd", "sr", "prcp", "4sm", "20sm",
    "wavg", "wvm", "wvd", "wdsd", "mws", "reto", "avp", "dp")

# Load data, which is in files for individual years
for (iyr in 1:length(obs_yrs)) {
  obs <- read.table(paste0(data_dir, "39", obs_yrs[iyr], "rh.txt"),
                    header = FALSE,
                    sep = ',',
                    fill = TRUE)
  colnames(obs) <- col_names
  obs <- select(obs, Year, JDay, Hour, Temp)
  obs_date <- as.Date(obs$JDay-1, origin = paste0("20", obs_yrs[iyr], "-01-01"))
  obs["Date"] <- as.Date(obs_date)
  obs["Month"] <- as.numeric(format(obs_date, "%m"))
  obs["Day"]  <- as.numeric(format(obs_date, "%d"))
  rm(obs_date)
  
  # Concatenate station data from different years
  if (iyr == 1) {
    stn_data_hourly <- obs
  }
  else {
    stn_data_hourly <- rbind(stn_data_hourly, obs)
  }
  
  rm(obs)
}

rm(col_names, data_dir, iyr)

# Filter hourly data to the months of June - August
stn_data_hourly <- filter(stn_data_hourly, Month >=6 & Month <= 8)

# Convert daily average temperature data from degrees C to degrees F
stn_data_hourly$Temp <- (1.8 * stn_data_hourly$Temp) + 32

# Assign temperature categories
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


# PLOT FOR HOURLY TEMPERATURE CATEGORIES  --------------------

category_colors <- c("below55" = "#4575b4",
                     "bet5560" = "#74add1",
                     "bet6065" = "#abd9e9",
                     "bet95100" = "#fdae61",
                     "bet100105" = "#f46d43",
                     "above105" = "#d73027")

p <- ggplot(data = stn_data_daily,
            mapping = aes(x = Date, y = Hours, color = TempCode)) +
  
  geom_line(size = 1) +
  
  facet_wrap(~ Year, ncol = 2, scales = "free_x") +
  
  scale_color_manual(values = category_colors) +
  
  
  # Specify axis breaks, gridlines, and limits
  scale_x_date(
    #limits = c(as.Date("06-01", format = "%m-%d"), as.Date("September 30"))),
    #date_labels = "%b",
    #breaks = c(1, 15, 32, 46, 60, 74, 
    #           91, 105, 121, 135, 152, 166,
    #           182, 196, 213, 227, 244),
    #labels = c("Jan 1", "Jan 15", "Feb 1", "Feb 15", "Mar 1", "Mar 15",
    #           "Apr 1", "Apr 15", "May 1", "May 15", "Jun 1", "Jun 15",
    #           "Jul 1", "Jul 15", "Aug 1", "Aug 15", "Sep 1"),
    #breaks = c(1, 8, 15, 22, 32, 39, 46, 53, 
    #           60, 67, 74, 81, 91, 98, 105, 112,
    #           121, 128, 135, 142, 152, 159, 166, 173,
    #           182, 189, 196, 203, 213, 220, 227, 234, 244),
    #labels = c("1/1", "", "1/15", "", "2/1", "", "2/15", "", 
    #           "3/1", "", "3/15", "", "4/1", "", "4/15", "",
    #           "5/1", "", "5/15", "", "6/1", "", "6/15", "",
    #           "7/1", "", "7/15", "", "8/1", "", "8/15", "", "9/1"),
    #limits = c((min(stn_data_daily$Date), max(stn_data_daily$Date)),
    #minor_breaks = c(1, 8, 15, 22, 32, 39, 46, 53, 60, 67, 74, 81, 
    #                 91, 98, 105, 112, 121, 128, 135, 142, 152, 159, 166, 173,
    #                 182, 189, 196, 203, 213, 220, 227, 234, 244),
    #date_labels = "%b %d",
    expand = c(0.0, 0.0)
  ) +

  scale_y_continuous(
    breaks = seq(from = 0, to = max(stn_data_daily$Hours, na.rm = TRUE), 
                 by = 2),
    limits = c(0, max(stn_data_daily$Hours, na.rm = TRUE)),
    minor_breaks = seq(from = 0, to = max(stn_data_daily$Hours, na.rm = TRUE), 
                       by = 1),
    expand = c(0.06, 0.0)
  ) +
  
  # Add the graph title, subtitle, and axis labels
  ggtitle("Hours within Key Temperature Ranges, June-August, 2017-2020") +
  labs(subtitle = "AZMET Willcox Bench Station, Cochise County, Arizona",
       x = "\nDate",
       y = "Hours per Day\n",
       caption = "\nmeteorological data: AZMET Willcox Bench station (cals.arizona.edu/azmet)") +
  
  # Further customize the figure appearance
  theme_light(base_family = "Source Sans Pro") +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(color = "gray40", size = 10),
        axis.text.y = element_text(color = "gray40", size = 10),
        axis.ticks.x.bottom = element_line(color = "gray80", size = 0.25),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.0, "mm"),
        axis.ticks.length.y = unit(0.0, "mm"),
        axis.title.x = element_text(color = "gray40", size = 10),
        axis.title.y = element_text(color = "gray40", size = 10),
        legend.title = element_text(color = "gray40", size = 10),
        legend.position = "bottom",
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.25),
        panel.grid.major.y = element_line(color = "gray80", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(color = "gray80", size = 0.25),
        plot.caption = element_text(color = "gray40", hjust = 0.0, size = 7),
        plot.caption.position = "plot",
        plot.margin = unit(c(1, 1 ,1, 1), "mm"),
        plot.subtitle = (element_text(family = "Source Serif Pro", size = 12)), 
        plot.title = (element_text(face = "bold", family = "Source Serif Pro", size = 16)),
        plot.title.position = "plot",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(color = "gray40", size = 12, face = "bold")
  )

#  Save the figure
ggsave(file = "summer-chill-heat-azmet-willcox-bench-202006.eps",
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 9, units = "in", dpi = 300) 

