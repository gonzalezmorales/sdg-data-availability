# Load necessary libraries
library(tidyr)
library(data.table)
library(plotly)
library(RColorBrewer)
library(stringr)
library(optiRum)
library(dplyr)
library("rjson")


# set your working directory - change as needed
setwd("C:/Users/L.GonzalezMorales/OneDrive - United Nations/UNCT - Toolkit/Data availability/")


# load functions
source('code/f_read.tab2dataTable.r')
source('code/f_writeTable2tab.r')
source('code/f_h_barplot_stacked.r')
source('code/f_h_barplot.r')
source('code/f_v_barplot.r')
source('code/f_doughnut.r')
source('code/f_doughnut2.r')


#----------------------------------
# Read data
#----------------------------------

availability_data <- read.tab2dataTable("data/IndicatorAvailability20190306.txt")
cr_countries <- read.tab2dataTable("data/RC-List-Merged.txt")
json_colors <- fromJSON(file="data/sdg_colors.json")

colors <- lapply(json_colors, function(sdg) # Loop through each "play"
{
  # Convert each group to a data frame.
  # This assumes you have 6 elements each time
  data.frame(matrix(unlist(sdg), ncol=15, byrow=T))
})

# Now you have a list of data frames, connect them together in
# one single dataframe
colors <- do.call(rbind, colors)

# Make column names nicer, remove row names
colnames(colors) <- c("GoalCode", "hex", "rgb.1", "rgb.2", "rgb.3", "iconUrl", 
                  "color1","color2","color3","color4","color5","color6","color7","color8","credits")
  
rownames(colors) <- NULL



