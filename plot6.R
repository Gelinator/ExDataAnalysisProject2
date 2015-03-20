plot6 <- function(wd=getwd()){
        setwd(wd)
        name <- "NEI_data.zip"
        library(ggplot2)
        grapher <- function(){
                all_lines <- list()
                cities <- subset(NEI,NEI$fips == "24510" | NEI$fips == "06037")
                
                for (i in c("vehicle","car","truck","buses","moto","mowers")){
                        ShortName <- grep(i,SCC$Short.Name, ignore.case=TRUE)
                        EISector <- grep(i,SCC$EI.Sector, ignore.case=TRUE)
                        LevelThree <- grep(i,SCC$SCC.Level.Three, ignore.case=TRUE)
                        LevelFour <- grep(i,SCC$SCC.Level.Four, ignore.case=TRUE)
                        all_lines <- sort(unlist(unique(c(all_lines, ShortName, EISector, LevelFour, LevelThree))))
                }
                
                rollers <- SCC[all_lines,]
                
                SCCcodes <- rollers$SCC
                
                vehicles <- subset(cities, (cities$SCC %in% SCCcodes))
                years <- list(vehicles$year)
                fipss <- list(vehicles$fips)
                EmissionsByYearCity <- aggregate(vehicles$Emissions, by = c(years,fipss), FUN=sum)
                names(EmissionsByYearCity) <- c("Year", "City", "Emissions")
                
                EmissionsByYearCity[EmissionsByYearCity == "06037"] <- "Los Angeles County"
                EmissionsByYearCity[EmissionsByYearCity == "24510"] <- "Baltimore City"
                png(filename = "Plot6.png",
                    width = 1000, height = 1000,bg="transparent")
                
                print(qplot(Year, Emissions, data=EmissionsByYearCity, color=City, 
                            geom = c("line","point"), ylab="Emissions (tons)", 
                            xlab = "Year", main = "Total emissions per city, by year",log="y"))
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