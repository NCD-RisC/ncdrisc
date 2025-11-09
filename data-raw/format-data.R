
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
