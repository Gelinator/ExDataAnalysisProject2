plot2 <- function(wd=getwd()){
        setwd(wd)
        name <- "NEI_data.zip"
        grapher <- function(){
                Baltimore <- subset(NEI,NEI$fips == "24510")
                years <- list(Baltimore$year)
                
                EmissionsByYear <- aggregate(Baltimore$Emissions, by = years, FUN=sum)
                
                png(filename = "Plot2.png",
                    width = 1000, height = 1000,bg="transparent")
                
                plot(EmissionsByYear, type = "b", ylab = "Emissions (tons)", xlab= "Year", 
                     main = "Total emissions in Baltimore City per year",xaxt = "n", col = "blue")
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