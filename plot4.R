# File:   plot4.R
# Date:   2015-05-23
# Author: Rick Wargo
#
# Description
# Given a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
# For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
#
# Explore the data and answer the following question:
#   Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#
# Modification History
# RCW  2015-05-23  New today

library(data.table)

# It takes a while to read in the NEI data - if it does not exist in the environment, read it in.
if (!exists('nei.data')) {
  nei.data <- readRDS('data/summarySCC_PM25.rds')
}
if (!exists('scc.data'))
  scc.data <- readRDS('data/Source_Classification_Code.rds')

# Data validation checks
if ((nrow(nei.data) != sum(complete.cases(nei.data))) | (nrow(nei.data) != 6497651))
  stop("NEI data is incomplete. Cannot continue.")

# Using the spreadsheet and directions from http://www.mass.gov/eea/agencies/massdep/service/online/sccs-emission-factors-and-naics-codes.html,
# identify SCC coal combustion codes by searching for Combustion in SCC.Level.One and Coal in SCC.Level.Four.
coal.combustion.scc <- scc.data[grep('Combustion', scc.data$SCC.Level.One, ignore.case=TRUE), ]
coal.combustion.scc <- scc.data[grep('Coal', coal.combustion.scc$SCC.Level.Four, ignore.case=TRUE), ]

# Using the shortened list of coal combustion SCC's, filter the NEI data
# Use data.table for speed
nei.dt <- data.table(nei.data)
coal.cumbustion.scc.dt <- data.table(coal.combustion.scc)
setkey(nei.dt, SCC)
setkey(coal.cumbustion.scc.dt, SCC)

# inner join the data sets with the common key (SCC)
emission.data <- nei.dt[coal.cumbustion.scc.dt, nomatch=0]

# aggregate the data across the US by year
emissions.by.year <- aggregate(emission.data$Emissions, by=list(year=emission.data$year), FUN=sum)
names(emissions.by.year)[names(emissions.by.year) == 'x'] <- 'Emissions'

# Redirect plot to 480x480 PNG file
png(file='plot4.png', width=480, height=480)

barplot(  emissions.by.year$Emissions/1000,
  names = emissions.by.year$year,
  main  = expression('Total Coal Combustion PM'[2.5]*' Emissions by Year'),
  xlab  = 'Year',
  ylab  = expression('PM'[2.5]*' Emissions (tons, thousands)')
)


# Close&save the PNG file
dev.off()