# File:   plot5.R
# Date:   2015-05-23
# Author: Rick Wargo
#
# Description
# Given a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
# For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
#
# Explore the data and answer the following question:
#   How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#
# Data Selection
#   As per the US definition of a motor vehicle, it appears the best 
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

# Define subset of NEI data to investigate question
# To determine the Mobile Vehicles, investigation of data shows that type == 'ON-ROAD' has the following EI.Sector values:
# -> Mobile - On-Road Gasoline Light Duty Vehicles
# -> Mobile - On-Road Gasoline Heavy Duty Vehicles
# -> Mobile - On-Road Diesel Light Duty Vehicles
# -> Mobile - On-Road Diesel Heavy Duty Vehicles
# Considering these are the only EI.Sectors for ON-ROAD, using type == 'ON-ROAD' complies with the US definition of a mobile vehicle
nei.dt <- data.table(nei.data[nei.data$fips=='24510' & nei.data$type=='ON-ROAD', ])
scc.data.dt <- data.table(scc.data)
setkey(nei.dt, SCC)
setkey(scc.data.dt, SCC)

# inner join the data sets with the common key (SCC)
emission.data <- nei.dt[scc.data.dt, nomatch=0]

# aggregate the mobile vehicle emission data by year for Baltimore City, MD
emissions.by.year <- aggregate(emission.data$Emissions, by=list(year=emission.data$year), FUN=sum)
names(emissions.by.year)[names(emissions.by.year) == 'x'] <- 'Emissions'

# Redirect plot to 480x480 PNG file
png(file='plot5.png', width=480, height=480)

barplot(  emissions.by.year$Emissions,
  names = emissions.by.year$year,
  main  = expression('Total PM'[2.5]*' Mobile Vehicle Emissions by Year in Baltimore City, MD'),
  xlab  = 'Year',
  ylab  = expression('PM'[2.5]*' Emissions (tons)')
)

# Close&save the PNG file
dev.off()