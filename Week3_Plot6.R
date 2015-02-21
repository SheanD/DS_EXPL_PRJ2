##############################################################################
## CREATES COUSERA JOHN HOPKINS EXPLORATORY DATA ANALYSIS PROJECT 2 PLOT 6  ##
##############################################################################
## QUESTION:  Compare motor vehicle emissions from Baltimore City, MD       ##
##            to Los Angeles County, CA from 1999 - 2008, which has         ##
##            greater changes                                               ##
##                                                                          ##
## METHOD:    Show normalized relationship                                  ##
##############################################################################
## readRDS: Serialization Interface for Single Objects                      ##
##############################################################################
##  REFERENCES:                                                             ##
## http://stackoverflow.com/questions/22850026/filtering-row-which-contains-a-certain-string-using-dplyr
## http://stackoverflow.com/questions/26611717/can-dplyr-join-on-multiple-columns-or-composite-key
## http://stackoverflow.com/questions/19971763/r-programming-normalizing-a-column-of-data-by-another-entry-in-2-other-columns                   

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

  if (!require("noncensus")) {
    install.packages("noncensus")
  }

  require(noncensus)
  require(data.table)
  require(dplyr)
  require(ggplot2)  

  library(noncensus)
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
  
  if (exists("tbl_SCC")== FALSE) {
    temp <- tempfile()
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    download.file(url, temp)
    tbl_SCC <- data.table(readRDS(unzip(temp, files = "Source_Classification_Code.rds", list = FALSE, overwrite = TRUE,
                         junkpaths = FALSE, exdir = ".", unzip = "internal",
                         setTimes = FALSE)))
    SCC <- dplyr::tbl_df(tbl_SCC)
    unlink(temp)
    rm(temp)
    rm(url)
  }
  
## sapply(NEI, typeof)
## glimpse(NEI)
## glimpse(SCC)
## View(SCC)

## CREATE PLOT DATA
  KN <- c(as.integer(1999),as.integer(2002),as.integer(2005),as.integer(2008))
  county_BC <- "24510"
  county_LA <- "06037"

  SCC_HWY <- tbl_SCC %>%
                filter(grepl("Highway Vehicles", SCC.Level.Two)) %>% 
                  select(SCC)

  prjdata_BC <- NEI %>%
      filter(fips == county_BC )
  prjdata_BC <- semi_join (prjdata_BC, SCC_HWY, by = c("SCC"="SCC"))
  prjdata_BC <- prjdata_BC %>%
      group_by(year, County = "BC")  %>%
      summarise(Emissions=sum(as.numeric(Emissions)))

  prjdata_LA <- NEI %>%
    filter(fips == county_LA )
  prjdata_LA <- semi_join (prjdata_LA, SCC_HWY, by = c("SCC"="SCC"))
  prjdata_LA <- prjdata_LA %>%
                group_by(year, County = "LA")  %>%
                summarise(Emissions=sum(as.numeric(Emissions)))

  prjdata_all <- rbind(prjdata_BC, prjdata_LA)

#NORMALIZE ON 1999
  prjdata_all <- data.table(prjdata_all)
  prjdata_all[, Group1999 := .SD[year=='1999', Emissions], by = County]
  prjdata_all[, valueNorm := Emissions / Group1999]

## PLOT 6
  png(filename="C:/WORKINGDIRECTORY/plot6.png")
    g <- ggplot(data=prjdata_all, aes(x=as.character(year),y=valueNorm, fill=valueNorm))
    p <- g + geom_bar(stat='identity', Fill = rev(brewer.pal(6, "Purples")))  
    p <- p + facet_grid(.~County)
    p <- p + guides(fill=FALSE)  
    p <- p + theme(panel.grid.minor = element_blank())
    p <- p + theme(panel.grid.major = element_blank())
    p <- p + scale_x_discrete("Year", labels=c( "1999"="99", "2002"="02", "2005"="05", "2008"="08"))
    p <- p + labs(y="Normalized Emissions (to 1999)", title = "Normalized Motor vehicle emissions from BC AND LA: 1999 to 2008")
    p <- p + geom_hline(aes(yintercept = 1))
  print(p) 
  dev.off()


