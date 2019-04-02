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

availability_data <- read.tab2dataTable("data/IndicatorAvailability195_20190306.txt")
cr_countries <- read.tab2dataTable("data/RC-List-Merged.txt")
json_colors <- fromJSON(file="data/sdg_colors.json")



wrap_labels <- function(label, line.length = 30) 
{
  l <- paste(strwrap(label, line.length), collapse = "\n")
  return(l)
}





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

#==========================================================

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
availability_data <- gather(availability_data, country, dataPoints, X4_Afghanistan:X894_Zambia, factor_key=FALSE)

availability_data <- separate(availability_data, country, c("countryCode", "countryName"), sep = "_")

availability_data <- as.data.table(availability_data)

availability_data[,countryCode := gsub("X","", countryCode)]

availability_data[,countryName := gsub("\\."," ", countryName)]

availability_data <- availability_data[, countryCode:=as.numeric(countryCode)]

#===========================================================

# merge cr countries

availability_data <- merge(availability_data, cr_countries, by.x = "countryCode", by.y = "M49", all = TRUE, 
      suffixes = c("",".CR"))

availability_data[,isRC := !is.na(List_final)]



#===========================================================

availability_data[,TierX := ifelse(Tier%in%c("Tier I",
                                             "Tier I/II/III depending on indice",
                                             "Tier I (a)/Tier II (b)",
                                             "Tier I (ODA)/Tier II (FDI)",
                                             "Tier I/III"),"Tier I*", 
                                   Tier)]
availability_data[, hasData := ifelse(dataPoints > 0 , 1, 0)]

writeTable2tab(availability_data, "availability_data.txt")

#===========================================================


T1 <- availability_data[, .(indicatorsAvailable = sum(hasData, na.rm = TRUE), .N),
                  by=list(countryCode, countryName,Country,ISO, isRC, TierX)]

T1[,percent := indicatorsAvailable / N * 100]

T1[,N2 := ifelse(TierX == "Tier I*", 107, 
                 ifelse(TierX == "Tier II", 84, 41))]


T1[,percent2 := indicatorsAvailable / N2 * 100]



writeTable2tab(T1, "T1-availability-by-country-and-tier.txt")



#===========================================================

T2 <- availability_data[, .(countriesAvailable = sum(hasData, na.rm = TRUE), .N),
                                 by=list(Goal, Indicator, IndicatorCode, IndicatorDescription, TierX)]

T2[,percent := countriesAvailable / N * 100]
T2[,Has75percentPlus := ifelse(percent>=75,1,0)]


writeTable2tab(T2, "T2-availability-by-indicator-195.txt")


#===========================================================

T2.rc <- availability_data[, .(countriesAvailable = sum(hasData, na.rm = TRUE), .N),
                                 by=list(isRC, Goal, Indicator, IndicatorCode, IndicatorDescription, TierX)]

T2.rc[,percent := countriesAvailable / N * 100]
T2.rc[,Has75percentPlus := ifelse(percent>=75,1,0)]


writeTable2tab(T2.rc, "T2-availability-by-indicator-and-rc-131.txt")

#==========================================================



#---- By series ----

wide_files <- c("country_1.1.1-SI_POV_DAY1_wide.csv",
                "country_1.1.1-SI_POV_EMP1_wide.csv",
                "country_1.2.1-SI_POV_NAHC_wide.csv",
                "country_1.3.1-SI_COV_BENFTS_wide.csv",
                "country_1.3.1-SI_COV_CHLD_wide.csv",
                "country_1.3.1-SI_COV_DISAB_wide.csv",
                "country_1.3.1-SI_COV_LMKT_wide.csv",
                "country_1.3.1-SI_COV_LMKTPQ_wide.csv",
                "country_1.3.1-SI_COV_MATNL_wide.csv",
                "country_1.3.1-SI_COV_PENSN_wide.csv",
                "country_1.3.1-SI_COV_POOR_wide.csv",
                "country_1.3.1-SI_COV_SOCAST_wide.csv",
                "country_1.3.1-SI_COV_SOCASTPQ_wide.csv",
                "country_1.3.1-SI_COV_SOCINS_wide.csv",
                "country_1.3.1-SI_COV_SOCINSPQ_wide.csv",
                "country_1.3.1-SI_COV_UEMP_wide.csv",
                "country_1.3.1-SI_COV_VULN_wide.csv",
                "country_1.3.1-SI_COV_WKINJRY_wide.csv")

for(wf in wide_files) {
  x <- read.tab2dataTable(paste("https://raw.githubusercontent.com/UNStats/FIS4SDGs/master/unsd/data/csv/",wf,sep=""),
                          sep = ",", 
                          quote = "\""
  )
  
  x <- merge(cr_countries[,list(M49)], x, by.x = c("M49"), by.y = c("GeoArea_Code"))
  
  x <- as.data.table(x)
  
  x<- x %>%
    group_by(M49, GeoArea_Desc, ISO3CD, IndicatorCode, SeriesDesc,SeriesCode) %>%
    summarize(Latest_Year = mean(Latest_Year, na.rm=TRUE))
  
  x <- as.data.table(x)
  
  indicator <- x$IndicatorCode[1]
  series <- x$SeriesDesc[1]
  seriesCode <- x$SeriesCode[1]
  
  x[!is.na(Latest_Year),Latest_Year]
  
  svg(filename=paste("output/latest-year-dist-", indicator, "-",seriesCode,".svg"), 
                     width=8, height=6)
  
  xx <- hist(x$Latest_Year,
            main = paste("Latest available year for Indicator ",indicator, " (RC Countries)",
                   "\nSeries: \"",wrap_labels(series, line.length = 50),"\"", sep = ""),
            xlab = "Year",
            col="#E5233D",
            breaks = 1990:2017,
            xaxt='n')
  
  axis(side=1, at=1990:2017-0.4, tick = FALSE,lwd=0,las=2,
       labels=1990:2017, cex.axis=0.6, padj=0)
  
  text(1995,
       max(xx$counts)*.8,
       paste("(",length(x[!is.na(Latest_Year),Latest_Year]), " data points)", sep=""),
       cex = 0.6)
  
  
  cat("finisthed ",seriesCode,"\n")
  
  
  dev.off()
}



latest_year_pov.txt




pov.1 <- hist(poverty_latest$SI_POV_NAHC,
              main = "Latest available year for Indicator 1.2.1\nProportion of population living below the national poverty line \n (RC Countries)",
              xlab = "Year",
              col="#E5233D",
              breaks = 1990:2017)
