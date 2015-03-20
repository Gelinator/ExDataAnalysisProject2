#Question 1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from all 
#sources for each of the years 1999, 2002, 2005, and 2008.
plot1 <- function(wd=getwd()){
        setwd(wd)
        name <- "NEI_data.zip"
        grapher <- function(){
                years <- list(NEI$year)
                EmissionsByYear <- aggregate(NEI$Emissions, by = years, FUN=sum)
                
                png(filename = "Plot1.png",
                    width = 1000, height = 1000,bg="transparent")
                
                plot(EmissionsByYear, type = "b", ylab = "Emissions (tons)", xlab= "Year", 
                     main = "Total emissions per year",xaxt = "n", col = "blue")
                axis(1,at= c(1999,2002,2005,2008),labels = c(1999,2002,2005,2008))
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
}