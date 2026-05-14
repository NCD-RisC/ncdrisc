
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
<<<<<<< Updated upstream
=======

# example dataset for testing
example_data <- read.csv("data-raw/USA NHANES 2017-2018.csv")
use_data(example_data)

# outputs to check against for make_age_group function (do not rerun)
agemin <- ifelse(example_data$sex == 1, example_data$age_min_anthro_M, example_data$age_min_anthro_F)
agemax <- ifelse(example_data$sex == 1, example_data$age_max_anthro_M, example_data$age_max_anthro_F)
age <- clean_age_range(example_data$age, example_data$id_study, agemin, agemax)
age_group_output1 <- make_age_group(age, agemin, agemax)
age_group_output2 <- make_age_group_anthro(age, agemin, agemax)
use_data(age_group_output1)
use_data(age_group_output2)

# ncdrisc colour scheme
## Regional palette ##
region_col <- c("#84C680", "#ADD9AA", "#FCC2C1", "#E31A1C", "#4B93C3", "#5BB356", "#FB9A99", "#FF7F00", "#1F78B4", "#B15928", "#C9E1EE", "#8763AE", "#E84749", "#B7D7E8", "#6A3D9A", "#FFA332", "#FBAEAD", "#33A02C", "#A6CEE3", "#FDD6D6")
names(region_col) <- c("Andean Latin America","The Caribbean","Central and southern Africa","Central Asia","Central Europe","Central Latin America","East Africa","East Asia and the Pacific","Eastern Europe","South Asia", "High-income English-speaking countries","Melanesia","Middle East and north Africa","Northwestern Europe","Polynesia and Micronesia","Southeast Asia","West Africa","Southern Latin America","Southwestern Europe","Other sub-Saharan Africa")
use_data(region_col)

## Super-regional palette ##
sregion_col <- c("#1F78B4", "#E31A1C", "#FF7F00", "#A6CEE3", "#33A02C", "#6A3D9A", "#B15928", "#FB9A99")
names(sregion_col) <- c("Central and eastern Europe","Central Asia, Middle East and north Africa","East and southeast Asia and the Pacific","High-income western","Latin America and the Caribbean","Pacific island nations","South Asia","Sub-Saharan Africa")
use_data(sregion_col)

region_order <- c("High-income English-speaking countries",
                      "Northwestern Europe",
                      "Southwestern Europe",
                      "Central Europe",
                      "Eastern Europe",
                      "Southern Latin America",
                      "Central Latin America",
                      "Andean Latin America",
                      "The Caribbean",
                      "East Asia and the Pacific",
                      "Southeast Asia",
                      "South Asia",
                      "Central Asia",
                      "Middle East and north Africa",
                      "Polynesia and Micronesia",
                      "Melanesia",
                      "East Africa",
                      "West Africa",
                      "Central and southern Africa",
                      "Other sub-Saharan Africa")
use_data(region_order)

sregion_order <- c("High-income western",
                   "Central and eastern Europe",
                   "Latin America and the Caribbean",
                   "East and southeast Asia and the Pacific",
                   "South Asia",
                   "Pacific island nations",
                   "Central Asia, Middle East and north Africa",
                   "Sub-Saharan Africa")
use_data(sregion_order)
>>>>>>> Stashed changes
