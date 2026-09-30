# ncdrisc package
This package contains standard functions used by the NCD Risk Factor Collaboration

### 0. Data objects

* `countrylist`
NCD-RisC country, region and super-region names, latest (2025) version
* `countrylist2023new`
NCD-RisC country, region and super-region names, 2023-new version
* `region_col`
NCD-RisC standard regional colours
* `sregion_col`
NCD-RisC standard super-regional colours
* `region_order`
Standard order of regions
* `sregion_order`
Standard order of super-regions
* `std_names_list`
Standard variable names for extraction
* `high_altitude_countries`
List of ISOs for countries that are considered high-altitude countries
* `world_map`
Standard shape file for world map, created based on [Natural Earth](https://www.naturalearthdata.com/)

### 1. Data extraction

* [check_extraction.R](./R/check_extraction.R)
Standard checks on extraction script
* [save_extraction.R](./R/save_extraction.R)
Save extractions
* [check_data_changes.R](./R/check_data_changes.R)
Check changes in extracted data frame against previous extraction

### 2. Data cleaning


### 3. Excluding follow-up studies

Used for individual-level analyses (where data points are individuals). Removes follow-up studies, keeping only the first measurement for each individual. The rationale is that we cannot reuse the same individual for individual-level analyses or we would double-count.

* [remove_followups.R](./R/remove_followups.R)
Removes follow-up studies from a dataset using the "Follow-ups tracker" workbook, read from the S: drive via the helper `read_followups_tracker()`. Follow-up status comes from the tracker's `FU` column (`FU`, `FU_w_refreshers`, `nonFU`); the earliest study of each cohort group is kept. A timestamped copy of the exact tracker used is saved to a folder you pass, for traceability. Studies not yet categorised are flagged (CAUTION) but all kept — both those in the tracker with a blank `FU` and those not in the tracker at all.

### 4. Data analysis


### 5. Plotting
