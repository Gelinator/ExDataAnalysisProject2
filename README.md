# ExploratoryAssignment2

## Dependencies
    
    ggplot2

## Getting it to work

I’ve built this code in order to be as user friendly as possible. Once the codes are loaded from your computer into R,
all that needs to be done is calling the function and the all the output files will be saved to the current working directory.

The working directory is the only customization that can be done without altering the code.

The codes will verify if the data availability in the following order:
* The Global environment (if any other code as previously been used, it will have been cached in it)
* The working directory (will load the RDS files if found)
* The zipped file (will unzip and load the RDS files if found)
* If nothing is found, it will download it and load it by itself


## Interpreting the Data

### Plot 1 through 5
Connected dots plots have been used with no scaling. They can therefore be interpreted as is.

### Plot 6
A logarithmic scale has been used for the Y-axis in order to show the amplitude of variations rather than absolute variations.

## Technical details
Built on
        R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet"
Platform
        x86_64-apple-darwin13.4.0 (64-bit)