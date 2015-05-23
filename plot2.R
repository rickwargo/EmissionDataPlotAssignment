# File:   plot1.R
# Date:   2015-05-23
# Author: Rick Wargo
#
# Description
# Given a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
# For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
#
# Explore the data and answer the following question:
#   Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#   Use the base plotting system to make a plot answering this question.
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

if (!exists('emissions.by.year')) {
  # Define subset of NEI data to investigate question
  baltimore.emissions <- nei.data[nei.data=='24510', c('Emissions', 'year')]
  emissions.by.year <- aggregate(baltimore.emissions$Emissions, by=list(year=baltimore.emissions$year), FUN=sum)
  names(emissions.by.year)[names(emissions.by.year) == 'x'] <- 'Emissions'
}

# Redirect plot to 480x480 PNG file
png(file='plot2.png', width=480, height=480)

# Draw a bar plot of emissions by year, emissions in thousands of tons
bp <- barplot(  emissions.by.year$Emissions/1000,
                names = emissions.by.year$year,
                main  = expression('Total PM'[2.5]*' Emissions by Year in Baltimore City, MD'),
                xlab  = 'Year',
                ylab  = expression('PM'[2.5]*' Emissions (tons, thousands)')
)

# Close&save the PNG file
dev.off()