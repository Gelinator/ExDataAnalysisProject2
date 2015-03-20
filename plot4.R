# Function that builds the plot4.
#The default working directory will be the current working directory. Any other can be specified

plot4 <- function(wd=getwd()){
        #setting the working directory
        setwd(wd)
        
        #specifying the zip file name
        name <- "NEI_data.zip"
        
        ##The function building the graphic
        grapher <- function(){
                
                #looking through all the observations to find coal related SCC codes description
                ShortName <- grep("coal",SCC$Short.Name, ignore.case=TRUE)
                EISector <- grep("coal",SCC$EI.Sector, ignore.case=TRUE)
                LevelThree <- grep("coal",SCC$SCC.Level.Three, ignore.case=TRUE)
                LevelFour <- grep("coal",SCC$SCC.Level.Four, ignore.case=TRUE)
                
                #removing all duplicates
                totalCoal <- sort(unique(c(ShortName, EISector, LevelFour, LevelThree)))
                
                #subsetting the SCC description file to keep solely the coal related lines
                Coaled <- SCC[totalCoal,]
                
                #listing the relevant SCC codes
                SCCcodes <- Coaled$SCC
                
                #subsetting the NEI dataset with the relevant SCC codes to keep coal related ones
                CoalNEI <- subset(NEI, (NEI$SCC %in% SCCcodes))
                
                #creates a list of all the coal related observations' year
                years <- list(CoalNEI$year)
                
                #sums the total emmissions by year for the coal related observations
                EmissionsByYear <- aggregate(CoalNEI$Emissions, by = years, FUN=sum)
                
                #preparing the png file
                png(filename = "Plot4.png",
                    width = 1000, height = 1000, bg="transparent")
                #plotting it!
                plot(EmissionsByYear, type = "b", ylab = "Emissions (tons)", xlab= "Year", 
                     main = "Total Emissions from Coal Combustion per Year",xaxt = "n", col = "blue")
                #marking the observation years as X labels
                axis(1,at= c(1999,2002,2005,2008),labels = c(1999,2002,2005,2008))
                
                #finishing off
                dev.off()
        }
        
        #Verifying if the data is in the Global Environment. If true, puts it through 
        #the grapher function. Those two objects will be cached in the Global Environment if
        #any other code from this serie has previously been used.
        if (exists("NEI") == TRUE & exists("SCC")== TRUE){
                grapher()
        }
        
        
        #If not found in Global Environment, verifies if unzipped files exist in current working directory.
        #If true, they will be loaded in the Global Environment.
        else if (file.exists("summarySCC_PM25.rds")==TRUE & 
                         file.exists("Source_Classification_Code.rds")==TRUE){
                NEI <<- readRDS("summarySCC_PM25.rds")
                SCC <<- readRDS("Source_Classification_Code.rds")
                grapher()
        }
        #If unzipped files not found in current working directory, looking if zip file present in current working directory.
        #If true, unzips it and extracts the two data files then loads them in Global Environment.
        else if (file.exists(name)==TRUE){
                unz(name, c("summarySCC_PM25.rds","Source_Classification_Code.rds"))
                NEI <<- readRDS("summarySCC_PM25.rds")
                SCC <<- readRDS("Source_Classification_Code.rds")
                grapher()
        }
        
        #If none of the above is true, downloads the zip file, unzips it and loads it in the Global Environment.
        #Then runs it through the grapher function to produce the graph.
        else {
                download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                              destfile=name, method="curl")
                unzip(name, c("summarySCC_PM25.rds","Source_Classification_Code.rds"))
                NEI <<- readRDS("summarySCC_PM25.rds")
                SCC <<- readRDS("Source_Classification_Code.rds")
                grapher()
        }
}