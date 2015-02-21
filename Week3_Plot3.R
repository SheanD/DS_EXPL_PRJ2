##############################################################################
## CREATES COUSERA JOHN HOPKINS EXPLORATORY DATA ANALYSIS PROJECT 2 PLOT 3  ##
##############################################################################
## QUESTION:  Which sources (point, nopoint, onroad, nonroad)               ##
##            have emission decreases from 1999 - 2008                      ##
##            for Baltimore City? Which have increases?                     ##
##                                                                          ##
## METHOD:    Using the ggplot2 plotting system, make one plot              ##
##############################################################################
## readRDS: Serialization Interface for Single Objects                      ##
##############################################################################
##                                                                          ##
##  REFERENCES:                                                             ##
##    http://docs.ggplot2.org/current/scale_discrete.html                   ##
##    http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/                      ##
##                                                                          ##
##############################################################################
## PREP WORK AREA
  ## rm(list=ls(all=TRUE))
  setwd("C:/WORKINGDIRECTORY")

## LOAD PACKAGES
  if (!require("data.table")) {
    install.packages("data.table")
  }
  
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }

  if (!require("RColorBrewer")) {
    install.packages("RColorBrewer")
  }

  require(data.table)
  require(dplyr)
  require(ggplot2)  
  
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(RColorBrewer)

## DOWNLOAD, UNZIP, AND LOAD FILE
  if (exists("NEI")== FALSE) {
    temp <- tempfile()
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    download.file(url, temp)
    NEI <- data.table(readRDS(unzip(temp, files = "summarySCC_PM25.rds", list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = ".", unzip = "internal",
        setTimes = FALSE)))
    NEI <- dplyr::tbl_df(NEI)
    unlink(temp)
    rm(temp)
    rm(url)
  }
  
  if (exists("SCC")== FALSE) {
    temp <- tempfile()
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    download.file(url, temp)
    SCC <- data.table(readRDS(unzip(temp, files = "Source_Classification_Code.rds", list = FALSE, overwrite = TRUE,
                         junkpaths = FALSE, exdir = ".", unzip = "internal",
                         setTimes = FALSE)))
    SCC <- dplyr::tbl_df(SCC)
    unlink(temp)
    rm(temp)
    rm(url)
  }
  
## sapply(NEI, typeof)
## glimpse(NEI)

## CREATE PLOT DATA
  KN <- c(as.integer(1999),as.integer(2002),as.integer(2005),as.integer(2008))
  county <- "24510"

  prjdata <- NEI %>%
    filter(year %in% KN & fips == county) %>%
        group_by(type, year) %>%
          arrange(type, year) %>%
            summarise(Emissions=sum(as.numeric(Emissions)))

## PLOT 3
png(filename="C:/WORKINGDIRECTORY/plot3.png")
  par(mfrow=c(1,1))
  g <- ggplot(data=prjdata, aes(x=as.character(year),y=Emissions, fill=Emissions))
  p <- g + geom_bar(stat='identity', Fill = rev(brewer.pal(6, "Purples")))  
  p <- p + facet_grid(.~type)
  p <- p + guides(fill=FALSE)  
  p <- p + theme(panel.grid.minor = element_blank())
  p <- p + theme(panel.grid.major = element_blank())
  p <- p + scale_x_discrete("Year", labels=c( "1999"="99", "2002"="02", "2005"="05", "2008"="08"))
  p <- p + labs(y="Emissions (tons)", title = "Source Emissions in Balitimore City, MD: 1999 to 2008")
  print(p)
dev.off()


