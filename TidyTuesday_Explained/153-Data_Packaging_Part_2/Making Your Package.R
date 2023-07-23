# TidyX Episode 153 - Data Packaging - Intro To Making Your F1 Data Package

# https://r-pkgs.org/ - online book with great information from Hadley Wickham
# devtools and usethis are package from Posit that are super helpful when doing package creation!

# Step 1: Starting up a new package ----

# usethis::create_package("mypackagename")

usethis::create_package("TidyTuesday_Explained/153-Data_Packaging_Part_2/f1championships")

# Step 2 Inspect the new project ----

# - R Folder
# - DESCRIPTION File
# - NAMESPACE File

# Step 3 Update Description File ----

# - Add Title
# - Add AUthors 
# - Add description
# - Add license (usethis::use_mit_license(), usethis::use_apl2_license(), etc)

# Step 4 Add in the scraping code in the data-raw folder ----

usethis::use_data_raw("f1_championship_data")

## Copy and paste from TidyX Episode 152 the relevant code for general scraping

# Step 5 Build the data and export to the data folder ----

## add usethis::use_data(object) to the end of the script from step 4






