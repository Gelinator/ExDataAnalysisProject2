plot3 <- function(wd=getwd()){
        setwd(wd)
        library(ggplot2)
        name <- "NEI_data.zip"
        grapher <- function(){
                Baltimore <- subset(NEI,NEI$fips == "24510")
                EmissionsByYearType <- aggregate(Baltimore$Emissions, by = list(Baltimore$year,Baltimore$type), FUN=sum)
                names(EmissionsByYearType) <- c("Year", "Type", "Emissions")
                png(filename = "Plot3.png",
                    width = 1000, height = 1000,bg="transparent")
                print(qplot(Year, Emissions, data=EmissionsByYearType, color=Type, 
                      geom = c("line","point"), ylab="Emissions (tons)", 
                      xlab = "Year", main = "Total emissions per type, by year"))
                dev.off()
        }
        if (exists("NEI") == TRUE & exists("SCC")== TRUE){
                grapher()
        }
        else if (file.exists("summarySCC_PM25.rds")==TRUE & 
                         file.exists("Source_Classification_Code.rds")==TRUE){
                NEI <<- readRDS("summarySCC_PM25.rds")
                SCC <<- readRDS("Source_Classification_Code.rds")
                grapher()
        }
        else if (file.exists(name)==TRUE){
                unz(name, c("summarySCC_PM25.rds","Source_Classification_Code.rds"))
                NEI <<- readRDS("summarySCC_PM25.rds")
                SCC <<- readRDS("Source_Classification_Code.rds")
                grapher()
        }
        else {
                download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                              destfile=name, method="curl")
                unzip(name, c("summarySCC_PM25.rds","Source_Classification_Code.rds"))
                NEI <<- readRDS("summarySCC_PM25.rds")
                SCC <<- readRDS("Source_Classification_Code.rds")
                grapher()
        }
        grapher()
}