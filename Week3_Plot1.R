##############################################################################
## CREATES COUSERA JOHN HOPKINS EXPLORATORY DATA ANALYSIS PROJECT 2 PLOT 1  ##
##############################################################################
## QUESTION:  Have total emissions from PM2.5 descreased in the US from     ##
##            1999 to 2008?                                                 ##
##                                                                          ##
## METHOD:    Using the BASE plotting system, make a plot showing the       ##
##            total PM2.5 emission from all sources for each of the years   ##
##            1999, 2002, 2005, 2008.                                       ##
##############################################################################
## readRDS: Serialization Interface for Single Objects                      ##

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
  
  if (!require("lubridate")) {
    install.packages("lubridate")
  }
  
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  
  require(data.table)
  require(dplyr)
  require(lubridate)
  require(ggplot2)
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(ggplot2)

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

  prjdata <- NEI %>%
    filter(year %in% KN) %>%
        group_by(year) %>%
          summarise(Emissions=sum(as.numeric(Emissions)))
  
  rownames(prjdata)<-prjdata$year

## PLOT 1
png(filename="C:/WORKINGDIRECTORY/plot1.png")
  par(mfrow=c(1,1))
  plot1 <-prjdata$Emissions
  ymarks <- c(0, 2000000, 4000000, 6000000)
  f <-11
  bplot1<-barplot(plot1, main="US Total Emissions from PM2.5: 1999 to 2008", font.main = f, cex.main=0.9
        , xlab=""
        , ylab="(tons)"
        , col="red"
        , names.arg=rownames(prjdata), font=f
        , yaxt="n")
        text(bplot1, 0, round(plot1/10000,0), cex=1, pos=3, col="white", font=f)
        axis(2, at=ymarks, labels=format(ymarks,scientific=FALSE) , font= f)

dev.off()


