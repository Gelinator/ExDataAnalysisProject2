plot4 <- function(wd=getwd()){
        setwd(wd)
        name <- "NEI_data.zip"
        grapher <- function(){
                ShortName <- grep("coal",SCC$Short.Name, ignore.case=TRUE)
                EISector <- grep("coal",SCC$EI.Sector, ignore.case=TRUE)
                LevelThree <- grep("coal",SCC$SCC.Level.Three, ignore.case=TRUE)
                LevelFour <- grep("coal",SCC$SCC.Level.Four, ignore.case=TRUE)
                totalCoal <- sort(unique(c(ShortName, EISector, LevelFour, LevelThree)))
                
                Coaled <- SCC[totalCoal,]
                
                SCCcodes <- Coaled$SCC
                
                CoalNEI <- subset(NEI, (NEI$SCC %in% SCCcodes))
                years <- list(CoalNEI$year)
                EmissionsByYear <- aggregate(CoalNEI$Emissions, by = years, FUN=sum)
                
                png(filename = "Plot4.png",
                    width = 1000, height = 1000, bg="transparent")
                
                plot(EmissionsByYear, type = "b", ylab = "Emissions (tons)", xlab= "Year", 
                     main = "Total Emissions from Coal Combustion per Year",xaxt = "n", col = "blue")
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
        grapher()
}