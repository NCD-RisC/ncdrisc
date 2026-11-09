# ncdrisc 0.0.5.93

* `remove_followups()` now also removes the participants returning to a follow-up wave with
  refreshers, so a caller no longer has to deal with those waves individually. The tracker says how
  to find them, in a new column `Refresher linking`: `id` (the returners keep the id they held in an
  earlier wave of their cohort group), `age` (the ids do not carry across, so the returners are the
  rows inside `Returning age min` to `Returning age max`) or `unlinkable` (they cannot be found, so
  the whole wave goes). These three columns are now required in the tracker.
* `remove_followups()` records what it removed, and what still needs a decision, in the `"followups"`
  attribute of its result, so a caller can log the exclusions without repeating the work.
* A refresher wave is only ever acted on while an earlier wave of its cohort group is also present:
  nobody in the first wave of a cohort is counted twice, so nothing is removed from it.
* `read_followups_tracker()` now reads the workbook as text and coerces it, so a column left blank
  for more than 1000 rows is no longer read as all `NA`.

# ncdrisc 0.0.5.91 & 0.0.5.92

* Add `clean_data()` functions and `make_age_group()` function

# ncdrisc 0.0.4.99

* Added `remove_followups()` function to drop follow-up studies from a dataset
* Added `read_followups_tracker()` to read the "Follow-ups tracker" workbook from the S: drive

# ncdrisc 0.0.4.98

* Updated region names to country-list-2025 version in `countrylist`; old version retained in `countrylist2023new`
* Added standard regional colour palettes and orders: `region_col`, `sregion_col`, `region_order`, `sregion_order`
* Added world map shape file `world_map`

# ncdrisc 0.0.4.97

* Automatically add slash to end of file path in `save_extraction()`

# ncdrisc 0.0.4.96

* Bug fix

# ncdrisc 0.0.4.95

* Releasing `check_data_chagnes` branch into master branch

# ncdrisc 0.0.4.94

* Updated logic of reading files in `check_data_chagnes()` in test branch

# ncdrisc 0.0.4.93

* Created test brach with `check_data_changes()`

# ncdrisc 0.0.4.92

* Removed `check_data_changes()` function and moved to test branch

# ncdrisc 0.0.4.91

* Added `check_data_changes()` function

# ncdrisc 0.0.4

* Edited `save_extraction()` function to require explicit specification of `extracted survey` folder

# ncdrisc 0.0.3

* Added checking package version on load
* Modified loading of data to make sure it works when calling function without loading the package

# ncdrisc 0.0.2

* Added anaemia related checks to `check_extraction()` function
* Renamed the internal function `print()` to `print_it()`

# ncdrisc 0.0.1

* Created the package
* Added the `check_extraction()` function
* Added the `save_extraction()` function

