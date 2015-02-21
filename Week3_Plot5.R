##############################################################################
## CREATES COUSERA JOHN HOPKINS EXPLORATORY DATA ANALYSIS PROJECT 2 PLOT 5  ##
##############################################################################
## QUESTION:  How have motor vehicle emissions changed                      ##
##            from 1999 - 2008 in Baltimore City, MD                        ##
##                                                                          ##
##                                                                          ##
## METHOD:                                                                  ##
##############################################################################
## readRDS: Serialization Interface for Single Objects                      ##
##############################################################################
##                                                                          ##
##  REFERENCES:                                                             ##
##    http://docs.ggplot2.org/current/scale_discrete.html                   ##
##    http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/   
##    http://www.census.gov/geo/maps-data/data/tiger.html
##                                                                          ##
##############################################################################
## http://stackoverflow.com/questions/22850026/filtering-row-which-contains-a-certain-string-using-dplyr
## http://stackoverflow.com/questions/26611717/can-dplyr-join-on-multiple-columns-or-composite-key
## 

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
  county <- "24510"
  
  SCC_HWY <- tbl_SCC %>%
                filter(grepl("Highway Vehicles", SCC.Level.Two)) %>% 
                  select(SCC)

  SCC_brake <- tbl_SCC %>%
                filter(grepl("Brake", Short.Name)) %>% 
                select(SCC)

  SCC_LD <- tbl_SCC %>%
                  filter(grepl("Light Duty", Short.Name)) %>% 
                  select(SCC)

  SCC_HD <- tbl_SCC %>%
                filter(grepl("Heavy Duty", Short.Name)) %>% 
                select(SCC)

  SCC_EVAP <- tbl_SCC %>%
    filter(grepl("EVAP", SCC.Level.Two)) %>% 
    select(SCC)

  prjdata <- NEI %>%
                 filter(fips == county)

  prjdata_HWY <- semi_join (prjdata, SCC_HWY, by = c("SCC"="SCC"))
  prjdata_HWY <- prjdata_HWY %>%
                group_by(year, type= "HWY")  %>%
                summarise(Emissions=sum(as.numeric(Emissions)))

  prjdata_brake <- semi_join (prjdata, SCC_brake, by = c("SCC"="SCC")) 
  prjdata_brake <- prjdata_brake %>%
    group_by(year, type= "brake")  %>%
    summarise(Emissions=sum(as.numeric(Emissions)))
  
  prjdata_LD <- semi_join (prjdata, SCC_LD, by = c("SCC"="SCC"))
  prjdata_LD <- prjdata_LD %>%
    group_by(year, type= "LD")  %>%
    summarise(Emissions=sum(as.numeric(Emissions)))
  
  prjdata_HD <- semi_join (prjdata, SCC_HD, by = c("SCC"="SCC"))  
  prjdata_HD <- prjdata_HD %>%
    group_by(year, type= "HD")  %>%
    summarise(Emissions=sum(as.numeric(Emissions)))

#   EVAP, TIRE, EXHAUST, ETC too few data points available
#   prjdata_EVAP <- semi_join (prjdata, SCC_EVAP, by = c("SCC"="SCC"))  
#   prjdata_EVAP <- prjdata_EVAP %>%
#     group_by(year, type= "EVAP")  %>%
#     summarise(Emissions=sum(as.numeric(Emissions)))

  prjdata_all <- rbind(prjdata_HWY, prjdata_brake)
  prjdata_all <- rbind(prjdata_all, prjdata_brake)
  prjdata_all <- rbind(prjdata_all, prjdata_LD)
  prjdata_all <- rbind(prjdata_all, prjdata_HD)

  prjdata_all <- arrange(prjdata_all, desc(Emissions), type, year )

## PLOT 5
  png(filename="C:/WORKINGDIRECTORY/plot5.png")
    g <- ggplot(data=prjdata_all, aes(x=as.character(year),y=Emissions, fill=Emissions))
    p <- g + geom_bar(stat='identity', Fill = rev(brewer.pal(6, "Purples")))  
    p <- p + facet_grid(.~type)
    p <- p + guides(fill=FALSE)  
    p <- p + theme(panel.grid.minor = element_blank())
    p <- p + theme(panel.grid.major = element_blank())
    p <- p + scale_x_discrete("Year", labels=c( "1999"="99", "2002"="02", "2005"="05", "2008"="08"))
    p <- p + labs(y="Emissions (tons)", title = "Motor vehicle emissions reductions in Balitimore City, MD: 1999 to 2008")
    print(p)
  dev.off()



