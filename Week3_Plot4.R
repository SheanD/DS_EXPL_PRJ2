##############################################################################
## CREATES COUSERA JOHN HOPKINS EXPLORATORY DATA ANALYSIS PROJECT 2 PLOT 4  ##
##############################################################################
## QUESTION:  US coal combustion-related source emissions                   ##
##            from 1999 - 2008                                              ##
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
##http://www.census.gov/geo/maps-data/data/tiger.html
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
#  county <- "24510"
  
  SCC_COAL <- tbl_SCC %>%
                filter(grepl("coal", Short.Name) | grepl("coal", EI.Sector) | grepl("coal", SCC.Level.One) | grepl("coal", SCC.Level.Two) | grepl("coal", SCC.Level.Three) | grepl("coal", SCC.Level.Four)) %>% 
                  select(SCC)

  prjdata <- semi_join (NEI, SCC_COAL, by = c("SCC"="SCC"))

  data(counties)

  county_state <- counties %>%
                    mutate(fips=gsub(" ","",paste(state_fips,county_fips)))

  prjdata <- inner_join (prjdata, county_state, by = c("fips"="fips"))

  prjdata2 <- prjdata %>%
              group_by(state, year)  %>%
                summarise(Emissions=sum(as.numeric(Emissions)))

## PLOT 4
  png(filename="C:/WORKINGDIRECTORY/plot4.png")
    par(mfrow=c(2,1))
    g <- ggplot(data=prjdata2, aes(x=as.character(year),y=Emissions, fill=Emissions))
    p <- g + geom_bar(stat='identity', Fill = rev(brewer.pal(6, "Purples")))  
    p <- p + facet_grid(.~state)
    p <- p + guides(fill=FALSE)  
    p <- p + theme(panel.grid.minor = element_blank())
    p <- p + theme(panel.grid.major = element_blank())
    p <- p + theme(axis.text.x = element_blank())
    p <- p + scale_x_discrete("Year", labels=c( "1999"="99", "2002"="02", "2005"="05", "2008"="08"))
    p <- p + labs(y="Emissions (tons)", title = "Emissions across 21 States with data available: 1999 to 2008")
    print(p)
  dev.off()



