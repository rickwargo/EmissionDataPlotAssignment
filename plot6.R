# File:   plot6.R
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
#   As per the US definition of a motor vehicle, it appears the best selection for a motor vehicle is type == 'ON-ROAD'
#   This selection surfaces the following EI.Sector factors, the only ones that are Mobile - On-Road
#     -> Mobile - On-Road Gasoline Light Duty Vehicles
#     -> Mobile - On-Road Gasoline Heavy Duty Vehicles
#     -> Mobile - On-Road Diesel Light Duty Vehicles
#     -> Mobile - On-Road Diesel Heavy Duty Vehicles
#
# Modification History
# RCW  2015-05-23  New today

library(data.table)
library(ggplot2)
library(grid)

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
# To determine the Motor Vehicles, investigation of data shows that type == 'ON-ROAD' has the following EI.Sector values:
# -> Mobile - On-Road Gasoline Light Duty Vehicles
# -> Mobile - On-Road Gasoline Heavy Duty Vehicles
# -> Mobile - On-Road Diesel Light Duty Vehicles
# -> Mobile - On-Road Diesel Heavy Duty Vehicles
# Considering these are the only EI.Sectors for ON-ROAD, using type == 'ON-ROAD' complies with the US definition of a motor vehicle
nei.dt <- data.table(nei.data[nei.data$fips %in% c('24510','06037') & nei.data$type=='ON-ROAD', ])
scc.data.dt <- data.table(scc.data)
setkey(nei.dt, SCC)
setkey(scc.data.dt, SCC)

# inner join the data sets with the common key (SCC)
emission.data <- nei.dt[scc.data.dt, nomatch=0]
emission.data$year <- as.factor(emission.data$year)

# aggregate the motor vehicle emission data by year for Baltimore City, MD
emissions.by.year <- aggregate(emission.data$Emissions, by=list(year=emission.data$year,fips=emission.data$fips), FUN=sum)
names(emissions.by.year)[names(emissions.by.year) == 'x'] <- 'Emissions'

# Redirect plot to 640x480 PNG file
png(file='plot6.png', width=720, height=480)

# Generate a plot for Point PM2.5 emissions by year for Baltimore
p1 <- ggplot(data=subset(emissions.by.year, fips=='24510'), aes(x=year, y=Emissions)) + 
  geom_bar(stat='identity') +
  xlab('Year') +
  ylab(expression('PM'[2.5]*' Emissions (tons)')) +
  ggtitle(expression('PM'[2.5]*' Emissions - Baltimore City')) 

# Generate a plot for Point PM2.5 emissions by year for Los Angeles
p2 <- ggplot(data=subset(emissions.by.year, fips=='06037'), aes(x=year, y=Emissions)) + 
  geom_bar(stat='identity') +
  xlab('Year') +
  ylab(expression('PM'[2.5]*' Emissions (tons)')) +
  ggtitle(expression('PM'[2.5]*' Emissions - Los Angeles County')) 

# Print the stored plots manipulating the viewport to show a main title in the first row, followed by x 2x2 set of plots
pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 4, 4), "null"))))
grid.text(expression('Motor Vehicle PM'[2.5]*' Emissions by Year Comparison'), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# Close&save the PNG file
dev.off()