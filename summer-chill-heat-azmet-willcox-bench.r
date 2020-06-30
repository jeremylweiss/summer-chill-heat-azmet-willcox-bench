

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

# Set the AZMET station name and years of interest
stn_name <- "Willcox Bench"
yr_start <- stn_list$start_yr[which(stn_list$stn == stn_name)]
yr_end <- stn_list$end_yr[which(stn_list$stn == stn_name)]

# Load function to download and transform daily AZMET data
source("azmet.hourly.data.download.R")


# DOWNLOAD AND TRANSFORM HOURLY AZMET DATA --------------------


stn_data_hourly <- azmet.hourly.data.download(stn_name)

# Retain necessary variables
stn_data_hourly <- select(stn_data_hourly, DateTime, Date, Year, Month, Day, 
                          JDay, Hour, Temp)

# Convert temperature from Celsius to Fahrenheit
stn_data_hourly$Temp <- (1.8 * stn_data_hourly$Temp) + 32

# Filter hourly data to the months of June - September
stn_data_hourly <- filter(stn_data_hourly, Month >=6 & Month <= 9)

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


# DERIVE ADDITIONAL DATA FOR SCATTER & LINE PLOTS --------------------


# Calculate seven-day (weekly) rolling averages for temperature categories by
# year
stn_data_daily["below55wk"] <- NA
stn_data_daily["bet5560wk"] <- NA
stn_data_daily["bet6065wk"] <- NA
stn_data_daily["bet95100wk"] <- NA
stn_data_daily["bet100105wk"] <- NA
stn_data_daily["above105wk"] <- NA

for (yr in yr_start:yr_end) {
  iyr_data <- which(stn_data_daily$Year == yr)
  
  for (i in 7:length(iyr_data)) {
    stn_data_daily$below55wk[iyr_data[i]] <- mean(
      stn_data_daily$below55[(iyr_data[i] - 6):iyr_data[i]], na.rm = TRUE
      )
    stn_data_daily$bet5560wk[iyr_data[i]] <- mean(
      stn_data_daily$bet5560[(iyr_data[i] - 6):iyr_data[i]], na.rm = TRUE
    )
    stn_data_daily$bet6065wk[iyr_data[i]] <- mean(
      stn_data_daily$bet6065[(iyr_data[i] - 6):iyr_data[i]], na.rm = TRUE
    )
    stn_data_daily$bet95100wk[iyr_data[i]] <- mean(
      stn_data_daily$bet95100[(iyr_data[i] - 6):iyr_data[i]], na.rm = TRUE
    )
    stn_data_daily$bet100105wk[iyr_data[i]] <- mean(
      stn_data_daily$bet100105[(iyr_data[i] - 6):iyr_data[i]], na.rm = TRUE
    )
    stn_data_daily$above105wk[iyr_data[i]] <- mean(
      stn_data_daily$above105[(iyr_data[i] - 6):iyr_data[i]], na.rm = TRUE
    )
    
  }
  rm(i)
}
rm(yr)

# Transform daily data from wide to long
stn_data_daily <- melt(data = stn_data_daily,
                       id.vars = colnames(stn_data_daily)[1:5],
                       measure.vars = colnames(stn_data_daily[6:17]),
                       variable.name = "TempCode",
                       value.name = "Hours",
                       na.rm = FALSE)


# PLOT FOR DAILY VALUES OF TEMPERATURE CATEGORIES  --------------------


category_colors <- c(
  "below55" = "#88419d", "bet5560" = "#8c96c6", "bet6065" = "#b3cde3",
  "bet95100" = "#fecc5c", "bet100105" = "#fd8d3c", "above105" = "#e31a1c",
  "below55wk" = "#88419d", "bet5560wk" = "#8c96c6", "bet6065wk" = "#b3cde3",
  "bet95100wk" = "#fecc5c", "bet100105wk" = "#fd8d3c", "above105wk" = "#e31a1c"
  )

p <- ggplot() +
  geom_point(
    data = filter(stn_data_daily,
                    TempCode == 'below55' | TempCode == 'bet5560' |
                    TempCode == 'bet6065' | TempCode == 'bet95100' |
                    TempCode == 'bet100105' | TempCode == 'above105'),
    mapping = aes(x = JDay, y = Hours, color = TempCode),
    alpha = 1.0) +
  
  geom_line(
    data = filter(stn_data_daily,
                  TempCode == 'below55wk' | TempCode == 'bet5560wk' |
                    TempCode == 'bet6065wk' | TempCode == 'bet95100wk' |
                    TempCode == 'bet100105wk' | TempCode == 'above105wk'),
    mapping = aes(x = JDay, y = Hours, color = TempCode),
    size = 1.5) +
  
  scale_color_manual(values = category_colors,
                     #aesthetics = c("color", "fill"),
                     labels = c("x","y","z","a","b","c",
                                "x","y","z","a","b","c")) +
  
  facet_wrap(~ Year, ncol = 1) +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(
    breaks = c(1, 15, 32, 46,
               60, 74, 91, 105,
               121, 135, 152, 166,
               182, 196, 213, 227,
               244, 258, 274),
    #labels = c("1/1", "", "1/15", "", "2/1", "", "2/15", "", 
    #           "3/1", "", "3/15", "", "4/1", "", "4/15", "",
    #           "5/1", "", "5/15", "", "6/1", "", "6/15", "",
    #           "7/1", "", "7/15", "", "8/1", "", "8/15", ""),
    limits = c(182, 274),
    #minor_breaks = c(1, 8, 15, 22, 32, 39, 46, 53, 60, 67, 74, 81, 
    #                 91, 98, 105, 112, 121, 128, 135, 142, 152, 159, 166, 173,
    #                 182, 189, 196, 203, 213, 220, 227, 234, 244),
    expand = c(0.0, 0.0)
  ) +
  
  scale_y_continuous(
    breaks = seq(from = 0, to = max(stn_data_daily$Hours, na.rm = TRUE), 
                 by = 3),
    limits = c(0, 9), 
               #max(stn_data_daily$Hours, na.rm = TRUE)),
    minor_breaks = seq(from = 0, to = 9, 
                       #to = max(stn_data_daily$Hours, na.rm = TRUE), 
                       by = 1),
    expand = c(0.06, 0.0)
  ) +
  
  # Add the graph title, subtitle, and axis labels
  ggtitle("Hours in Key Summer Temperature Ranges, 2016-2020") +
  labs(subtitle = "AZMET Willcox Bench Station, Cochise County, Arizona",
       x = "\nDate",
       y = "Hours per Day\n",
       caption = paste0(
         "\ndata from Arizona Meteorological Network (cals.arizona.edu/azmet)",
          "\nDATA NOT SHOWN DUE TO SCALE")) +
  
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
        legend.direction = "horizontal",
        legend.text = element_text(color = "gray40", size = 10),
        legend.title = element_text(color = "gray40", size = 10, face = "bold"),
        legend.position = "bottom",
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.25),
        panel.grid.major.y = element_line(color = "gray80", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
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
ggsave(file = paste0("summer-chill-heat-azmet-willcox-bench-", 
                     Sys.Date(),
                     ".eps"),
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 9, units = "in", dpi = 300) 

