# File:   plot1.R
# Date:   2015-05-23
# Author: Rick Wargo
#
# Description
# Given a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
# For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
#
# Explore the data and answer the following question:
#   Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#   Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each 
#   of the years 1999, 2002, 2005, and 2008.
#
# Modification History
# RCW  2015-05-23  New today

# It takes a while to read in the NEI data - if it does not exist in the environment, read it in.
if (!exists('nei.data')) {
  nei.data <- readRDS('data/summarySCC_PM25.rds')
}
if (!exists('scc.data'))
  scc.data <- readRDS('data/Source_Classification_Code.rds')

# Data validation checks
if ((nrow(nei.data) != sum(complete.cases(nei.data))) | (nrow(nei.data) != 6497651))
  stop("NEI data is incomplete. Cannot continue.")

if (!exists('nei.total.emissions.by.year')) {
  nei.total.emissions.by.year <- aggregate(nei.data$Emissions, by=list(year=nei.data$year), FUN=sum)
  names(nei.total.emissions.by.year)[names(nei.total.emissions.by.year) == 'x'] <- 'Emissions'
}

# Redirect plot to 480x480 PNG file
png(file='plot1.png', width=480, height=480)

# Draw a bar plot of emissions by year, emissions in millions of tons
barplot(  nei.total.emissions.by.year$Emissions/1000000,
  names = nei.total.emissions.by.year$year,
  main  = expression('Total PM'[2.5]*' Emissions by Year'),
  xlab  = 'Year',
  ylab  = expression('PM'[2.5]*' Emissions (tons, millions)')
  )

# Close&save the PNG file
dev.off()