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
Removes from a dataset everyone already counted in an earlier wave of their own cohort, using the "Follow-ups tracker" workbook, read from the S: drive via the helper `read_followups_tracker()`. Follow-up status comes from the tracker's `FU` column (`FU`, `FU_w_refreshers`, `nonFU`), and studies of one cohort share a `Cohort_group_id`. The earliest study of each cohort group present in the data is always kept in full, because nobody in it is counted twice.

A pure follow-up (`FU`) re-examines the earlier wave's participants and nobody else, so the whole study is removed. A follow-up with refreshers (`FU_w_refreshers`) also recruits people who have never taken part, so only its returning participants are removed, and the tracker's `Refresher linking` column says how to find them:

| `Refresher linking` | how the returning participants are found |
| --- | --- |
| `id` | they keep the id they held in an earlier wave of the same cohort group, so they are the rows whose `id` appears there. Needs an `id` column. |
| `age` | the ids do not carry across waves, but the returners occupy an age range the refreshers do not, given by `Returning age min` and `Returning age max` (a blank maximum means no upper bound). Applied to the whole wave. Needs an `age` column. |
| `unlinkable` | they cannot be identified either way, so the whole study is removed. |

`Refresher linking` is only read for a follow-up with refreshers that is not the first study of its cohort group, and may be left blank everywhere else.

A timestamped copy of the exact tracker used is saved to a folder you pass, for traceability. Studies not yet categorised are flagged (CAUTION) but all kept — both those in the tracker with a blank `FU` and those not in the tracker at all — as are refresher waves that count someone twice but have no `Refresher linking`. What was removed, and what needs attention, comes back in the `"followups"` attribute of the result.

### 4. Data analysis


### 5. Plotting
