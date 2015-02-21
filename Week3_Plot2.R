##############################################################################
## CREATES COUSERA JOHN HOPKINS EXPLORATORY DATA ANALYSIS PROJECT 2 PLOT 2  ##
##############################################################################
## QUESTION:  Have total emissions from PM2.5 descreased in Baltimore       ##
##            City, Maryland from 1999 to 2008?                             ##
##                                                                          ##
## METHOD:    Using the BASE plotting system                                ##
##############################################################################
## readRDS: Serialization Interface for Single Objects                      ##
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
  
  require(data.table)
  require(dplyr)
  
  library(data.table)
  library(dplyr)

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
        group_by(year) %>%
          summarise(Emissions=sum(as.numeric(Emissions)))
  
  rownames(prjdata)<-prjdata$year

## PLOT 2
png(filename="C:/WORKINGDIRECTORY/plot2.png")
  par(mfrow=c(1,1))
  plot1 <-prjdata$Emissions
  ymarks <- c(0, 1000, 2000, 3000)
  f <-11
  bplot1<-barplot(plot1, main=paste("Total PM2.5 Emissions","\n Baltimore City, MD"),
        , font.main = f, cex.main=0.9
        , xlab=""
        , ylab="(tons)"
        , col="red"
        , names.arg=rownames(prjdata)
        , font=f
        , yaxt="n")
        text(bplot1, 0, round(plot1,0), cex=1, pos=3, col="white", font=f)
        axis(2, at=ymarks, labels=format(ymarks,scientific=FALSE) , font= f)
dev.off()


