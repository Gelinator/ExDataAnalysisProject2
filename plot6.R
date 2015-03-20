# Function that builds the plot6.
#The default working directory will be the current working directory. Any other can be specified

plot6 <- function(wd=getwd()){
        #setting the working directory
        setwd(wd)
        
        #loading ggplot2 package
        library(ggplot2)
        
        #specifying the zip file name
        name <- "NEI_data.zip"
        
        #The function building the graphic
        grapher <- function(){
                #creates a list in the Global Environment to later be used in the for loop
                all_lines <- list()
                
                #Creates a subset of the NEI dataset to keep solely the Baltimore City and LA County areas observations
                cities <- subset(NEI,NEI$fips == "24510" | NEI$fips == "06037")
                
                #for loop built to iterate through a list of words refering to motorized vehicles.
                #On each loop cycle, it goes through all the observations to find 
                #vehicle related SCC codes description and stores them in all_lines
                #A complete list is then built and duplicates removed
                for (i in c("vehicle","car","truck","buses","moto","mowers")){
                        ShortName <- grep(i,SCC$Short.Name, ignore.case=TRUE)
                        EISector <- grep(i,SCC$EI.Sector, ignore.case=TRUE)
                        LevelThree <- grep(i,SCC$SCC.Level.Three, ignore.case=TRUE)
                        LevelFour <- grep(i,SCC$SCC.Level.Four, ignore.case=TRUE)
                        all_lines <- sort(unlist(unique(c(all_lines, ShortName, EISector, LevelFour, LevelThree))))
                }
                
                #subsetting the SCC description file to keep solely the vehicle related lines
                rollers <- SCC[all_lines,]
                
                #listing the relevant SCC codes
                SCCcodes <- rollers$SCC
                
                #subsetting the NEI dataset with the relevant SCC codes to keep vehicle related ones
                vehicles <- subset(cities, (cities$SCC %in% SCCcodes))
                
                #creates a list of all the vehicles related observations' year
                years <- list(vehicles$year)
                
                #creates a list of all the vehicles related observations' fips
                fipss <- list(vehicles$fips)
                
                #sums the total emmissions by year for the vehicle related observations in the Baltimore City and LA County area
                EmissionsByYearCity <- aggregate(vehicles$Emissions, by = c(years,fipss), FUN=sum)
                
                #renaming the column names for good measure
                names(EmissionsByYearCity) <- c("Year", "City", "Emissions")
                
                #Replacing the fips numbers with their corresponding area label
                EmissionsByYearCity[EmissionsByYearCity == "06037"] <- "Los Angeles County"
                EmissionsByYearCity[EmissionsByYearCity == "24510"] <- "Baltimore City"
                
                #preparing the png file
                png(filename = "Plot6.png",
                    width = 1000, height = 1000,bg="transparent")
                
                #plotting it!
                print(qplot(Year, Emissions, data=EmissionsByYearCity, color=City, 
                            geom = c("line","point"), ylab="log10 scale of Emissions (tons)", 
                            xlab = "Year", main = "Total emissions per city, by year",log="y"))
                
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