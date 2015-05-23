# File:   plot3.R
# Date:   2015-05-23
# Author: Rick Wargo
#
# Description
# Given a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
# For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
#
# Explore the data and answer the following question:
#   Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these 
#   four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions 
#   from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
#
# Modification History
# RCW  2015-05-23  New today

library(ggplot2)

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
  baltimore.emissions <- nei.data[nei.data=='24510', c('Emissions', 'year', 'type')]
  baltimore.emissions$type <- as.factor(baltimore.emissions$type)
  emissions.by.year <- aggregate(baltimore.emissions$Emissions, by=list(year=baltimore.emissions$year, type=baltimore.emissions$type), FUN=sum)
  names(emissions.by.year)[names(emissions.by.year) == 'x'] <- 'Emissions'
}

# Redirect plot to 480x480 PNG file
png(file='plot3.png', width=480, height=480)

# Generate a plot for Point PM2.5 emissions by year for Baltimore
p1 <- ggplot(data=subset(emissions.by.year, type=='POINT'), aes(x=year, y=Emissions)) + 
        geom_bar(stat='identity') +
        xlab('Year') +
        ylab(expression('PM'[2.5]*' Emissions (tons, thousands)')) +
        ggtitle(expression('Point PM'[2.5]*' Emissions'))

# Generate a plot for Non-Point PM2.5 emissions by year for Baltimore
p2 <- ggplot(data=subset(emissions.by.year, type=='NONPOINT'), aes(x=year, y=Emissions)) +
        geom_bar(stat='identity') +
        xlab('Year') +
        ylab(expression('PM'[2.5]*' Emissions (tons, thousands)')) +
        ggtitle(expression('Non-Point PM'[2.5]*' Emissions'))

# Generate a plot for On-Road PM2.5 emissions by year for Baltimore
p3 <- ggplot(data=subset(emissions.by.year, type=='ON-ROAD'), aes(x=year, y=Emissions)) +
        geom_bar(stat='identity') +
        xlab('Year') +
        ylab(expression('PM'[2.5]*' Emissions (tons, thousands)')) +
        ggtitle(expression('On-Road PM'[2.5]*' Emissions'))

# Generate a plot for Non-Road PM2.5 emissions by year for Baltimore
p4 <- ggplot(data=subset(emissions.by.year, type=='NON-ROAD'), aes(x=year, y=Emissions)) +
        geom_bar(stat='identity') +
        xlab('Year') +
        ylab(expression('PM'[2.5]*' Emissions (tons, thousands)')) +
        ggtitle(expression('Non-Road PM'[2.5]*' Emissions'))

# Print the stored plots manipulating the viewport to show a main title in the first row, followed by x 2x2 set of plots
pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(1, 4, 4), "null"))))
grid.text(expression('Total PM'[2.5]*' Emissions by Year in Baltimore City, MD'), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(p4, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

# Close&save the PNG file
dev.off()