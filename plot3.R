# Function that builds the plot3.
#The default working directory will be the current working directory. Any other can be specified

plot3 <- function(wd=getwd()){
        #setting the working directory
        setwd(wd)
        
        #loading ggplot2 package
        library(ggplot2)
        
        #specifying the zip file name
        name <- "NEI_data.zip"
        
        #The function building the graphic
        grapher <- function(){
                
                #Creates a subset of the data for the Baltimore City area
                Baltimore <- subset(NEI,NEI$fips == "24510")
                
                #sums the total emmissions by year, then by type for the Baltimore City area observations
                EmissionsByYearType <- aggregate(Baltimore$Emissions, by = list(Baltimore$year,Baltimore$type), FUN=sum)
                
                #renaming the column names for good measure
                names(EmissionsByYearType) <- c("Year", "Type", "Emissions")
                
                #preparing the png file
                png(filename = "Plot3.png",
                    width = 1000, height = 1000,bg="transparent")
                
                #plotting it!
                print(qplot(Year, Emissions, data=EmissionsByYearType, color=Type, 
                      geom = c("line","point"), ylab="Emissions (tons)", 
                      xlab = "Year", main = "Total emissions per type, by year"))
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