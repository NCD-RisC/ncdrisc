
library(devtools)

# Load necessary data and save as part of package
# This script needs to be run whenever the input files are updated
# Delete the old data and run the script from the root folder of the package

# Standard country names
countrylist <- read.csv("data-raw/country-list-2023-new.csv")
use_data(countrylist)

# standard variable name list - the list should be updated separately
std_names_list <- read.csv("data-raw/standard_variable_names.csv")
use_data(std_names_list)

# list of ISOs of high-altitude countries to be accounted for in haemoglobin data
high_altitude_countries <- read.csv("data-raw/high_altitude_countries.csv")
use_data(high_altitude_countries)
