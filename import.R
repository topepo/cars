library(tidyverse)
library(fs)
library(sessioninfo)

# Download source data, unzip, and rename file with date stamp------------------

# Make this a local path if you want to save the files, 
#  otherwise use path_temp()

working_file_path <- path_temp()
destination_path <- getwd()

date_stamp <- format(Sys.Date(), "_%Y_%m_%d")
date_path <- substring(date_stamp, 2)

data_url <- "https://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip"

target_zip_file <- path(working_file_path, "raw.zip")

download.file(url = data_url, destfile = target_zip_file)

dir_info(working_file_path) %>% 
  filter(type == "file") %>% 
  select(path, type, size, modification_time)

# get file name from inside zip
zip_info <- unzip(target_zip_file, list = TRUE)

unzip(target_zip_file, exdir = working_file_path)

dir_info(working_file_path) %>% 
  filter(type == "file") %>% 
  select(path, type, size, modification_time)

new_file_name <- 
  gsub("\\.csv$", paste0("_raw", date_stamp, "\\.csv"), zip_info$Name)

file_move(
  path(working_file_path, zip_info$Name),
  path(working_file_path, new_file_name)
)

dir_info(working_file_path) %>% 
  filter(type == "file") %>% 
  select(path, type, size, modification_time)

# Read in data, remove unwanted variables, and make some new columns -----------

# helpful resources
# https://www.fueleconomy.gov/feg/download.shtml
# https://www.fueleconomy.gov/feg/ws/index.shtml

car_data <- 
  read_csv(path(working_file_path, new_file_name), guess_max = 30000) %>%
  dplyr::select(
    -barrels08,  -barrelsA08,  -charge120,  -charge240,  -city08,  -cityA08,  
    -cityCD,  -cityE,  -cityUF,  -co2,  -co2A,  -co2TailpipeAGpm,  
    -co2TailpipeGpm,  -comb08,  -combA08,  -combE,  -combinedCD,  
    -combinedUF, -engId, -feScore,  -fuelCost08,  -fuelCostA08, -trans_dscr,  
    -ghgScore,  -ghgScoreA,  -highway08,  -highway08U,  
    -highwayA08,  -highwayA08U,  -highwayCD,  -highwayE,  -highwayUF,
    -id, -mpgData, -range,  -rangeCity,  -rangeCityA,  -rangeHwy,  -rangeHwyA,
    -UCity,  -UCityA,  -UHighway,  -UHighwayA, -youSaveSpend,  -guzzler,
    -atvType,  -rangeA,  -evMotor,  -mfrCode,  -c240Dscr,  
    -charge240b,  -c240bDscr,  -createdOn,  -modifiedOn,  -phevCity,  -phevHwy,  
    -phevComb, -fuelType1, -fuelType2, -phevBlended
  ) %>%
  dplyr::filter(year >= 2015) %>%
  # A lot of formatting factor levels to get rid of special chars and punct
  mutate(
    drive = gsub("([[:punct:]])", "", drive),
    drive = gsub(" ", "_", drive),
    drive = gsub("4", "four", drive),
    drive = gsub("-", "_", drive),
    drive = gsub("_(A1)", "", drive, fixed = TRUE),
    
    fuelType = gsub(" ", "_", fuelType),
    
    # Engine descriptor info ; there can be multiple values so make indicators
    # for the most prevelant values.  
    eng_dscr = gsub("Stop/Start", "Stop-Start", eng_dscr, fixed = TRUE),
    spark_ignited_direct_injection  = ifelse(grepl("SIDI", eng_dscr), 1, 0),
    flexible_fuel = ifelse(grepl("FFV", eng_dscr), 1, 0),
    plug_in_hybrid = ifelse(grepl("PHEV", eng_dscr), 1, 0),
    hybrid = ifelse(grepl("Hybrid", eng_dscr), 1, 0), # comb all hybrid types
    port_fuel_injection = ifelse(grepl("PFI", eng_dscr), 1, 0),
    hellcat = ifelse(grepl("Hellcat", eng_dscr), 1, 0),
    startStop = ifelse(
      startStop == "Y" | grepl("stop-start", tolower(eng_dscr)), 
      1, 0),
    ieloop = ifelse(grepl("i-ELOOP", eng_dscr), 1, 0),
    partial_zero_emissions = ifelse(grepl("PZEV", eng_dscr), 1, 0),
    eco = ifelse(grepl("Eco", eng_dscr), 1, 0),
    z06 = ifelse(grepl("Z06", eng_dscr), 1, 0),
    zr1 = ifelse(grepl("ZR1", eng_dscr), 1, 0),
    
    # More cleanup
    VClass = gsub(" - ", "_", VClass),
    VClass = gsub(",", "", VClass),
    VClass = gsub(" ", "_", VClass),
    
    trany = gsub("[[:punct:]]", "", trany),
    trany = gsub(" ", "_", trany),
    
    make = gsub("[[:punct:]]", "_", make),
    make = gsub(" ", "_", make),
    
    turbo_charged = ifelse(!is.na(tCharger), 1, 0),
    super_charged = ifelse(!is.na(sCharger), 1, 0),
    
    # For non-combustion engines
    cylinders = ifelse(is.na(cylinders), 0, cylinders),
    displ = ifelse(is.na(displ), 0, displ)
    
  ) 

# In case you want to look at engine descriptions ------------------------------

# eng_descr <- strsplit(car_data$eng_dscr, ";")
# eng_descr <- unlist(eng_descr)
# eng_descr <- eng_descr[!is.na(eng_descr)]
# eng_descr <- gsub("^\\s+|\\s+$", "", eng_descr)
# sort(table(eng_descr))

# Remove replicate data for a car config (defined in `group_by` below) ---------

# The unadjusted data can vary minutely over years (e.g. 145.0834 and 145.0835)
# so we will look for uniqueness in the combined mpg data up to a threshold of
# 1/10 mpg. 

mpg_precision <- 1

duplicates_removed <- 
  car_data %>%
  mutate(
    total_mpg = round(comb08U + combA08U + comb08U + combA08U, mpg_precision)
  )%>%
  # If the car has not changed from year-to-year, it is listed in multiple rows.
  # We keep the first instance of that car.   
  group_by(
    make, model, displ, eng_dscr, trany, fuelType, turbo_charged, super_charged,
    total_mpg
  ) %>% 
  arrange(year) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(-total_mpg)

# Check to see of there are cars that are jointly marketed and have the 
# same info for different manufacturers
check_multiple <- 
  duplicates_removed %>%
  # There are cars that are jointly marketed and have the same info for 
  # different manufacturers
  group_by(
    year, model, displ, eng_dscr, trany, fuelType, turbo_charged, super_charged
  ) %>% 
  count() %>%
  arrange(desc(n)) %>%
  dplyr::filter(n > 1)

check_multiple %>% nrow()

# Take the per-row maximum mpg ------------------------------------------------

max_mpg_per_row <- 
  duplicates_removed %>%
  ungroup() %>%
  mutate(
    city = ifelse(city08U > cityA08U, city08U, cityA08U),
    comb = ifelse(comb08U > combA08U, comb08U, combA08U),
  ) %>%
  select(-city08U, -cityA08U, -comb08U, -combA08U)

# check to make sure there is one row per car config (without fuel type)
check_one_config_per_row <- 
  max_mpg_per_row %>%
  group_by(
    year, make, model, displ, eng_dscr, trany, turbo_charged, super_charged,
    city, comb
  ) %>% 
  count() %>%
  arrange(desc(n)) %>%
  dplyr::filter(n > 1)

check_one_config_per_row %>% nrow()

# Rename and select some variables; make factors -------------------------------

final_data <-
  max_mpg_per_row %>%
  dplyr::rename(
    transmission = trany,
    car_class = VClass,
    fuel_type = fuelType,
    hatch_lug_vol = hlv,
    hatch_pas_vol = hpv,
    two_door_pass_vol = pv2,
    four_door_pass_vol = pv4,
    two_door_lug_vol = lv2,
    four_door_lug_vol = lv4,
    start_stop = startStop,
    eng_displ = displ
  ) %>%
  mutate(
    drive = as.factor(drive),
    make = as.factor(make),
    model = as.factor(model),
    fuel_type = as.factor(fuel_type),
    car_class = as.factor(car_class),
    transmission = as.factor(transmission)
  ) %>%
  # remove old columns
  dplyr::select(-eng_dscr, -tCharger, -sCharger) 

# Get the number of replicates per config with and w/o gas data ----------------

# rnow() should be 0

final_num_config_gas <- 
  final_data %>%
  group_by(
    year, make, model, eng_displ, transmission, turbo_charged, super_charged,
    flexible_fuel, spark_ignited_direct_injection, hybrid, port_fuel_injection,
    hellcat, start_stop, ieloop, partial_zero_emissions, eco, z06, zr1, drive, 
    city, comb
  ) %>% 
  count() %>%
  arrange(desc(n)) %>%
  dplyr::filter(n > 1)

final_num_config_gas %>% nrow()

final_num_config <- 
  final_data %>%
  group_by(
    year, make, model, eng_displ, transmission, turbo_charged, super_charged,
    flexible_fuel, spark_ignited_direct_injection, hybrid, port_fuel_injection,
    hellcat, start_stop, ieloop, partial_zero_emissions, eco, z06, zr1, drive
  ) %>% 
  count() %>%
  arrange(desc(n)) %>%
  dplyr::filter(n > 1)

final_num_config_gas %>% nrow()

# Check for completeness 
sum(!complete.cases(final_data))

# Training/Test Splits ---------------------------------------------------------

# Create test set based on the most recent year's data. Combine years if there
# are not at least min_test_n cars in the latest year. 

min_test_n <- 250

trn_stop_year <- 
  car_data %>%
  group_by(year) %>% 
  count() %>%
  ungroup() %>%
  arrange(desc(year)) %>%
  mutate(running_n = cumsum(n)) %>%
  dplyr::filter(running_n >= min_test_n) %>%
  slice(2) %>%
  pull(year)

# Save the data with city as the outcome ---------------------------------------

car_data <- 
  final_data %>%
  dplyr::rename(mpg = city) %>%
  select(-comb)

car_data %>% 
  group_by(year) %>% 
  count()

# now remove duplciates again based on their exact values
car_data <- 
  car_data %>%
  group_by(
    make, model, eng_displ, cylinders, drive, fuel_type, transmission,
    car_class, start_stop, flexible_fuel, plug_in_hybrid, hybrid,
    turbo_charged, super_charged, mpg
  ) %>% 
  arrange(year) %>%
  slice(1) %>%
  ungroup()

car_data %>% 
  group_by(year) %>% 
  count()

car_train <- car_data %>%
  filter(year <= trn_stop_year)

car_test <- car_data %>%
  filter(year > trn_stop_year)

new_data_path <- path(destination_path, paste0(date_path, "_city"))
recent_data_path <- path(destination_path, "latest_city")

dir_create(new_data_path)

save(car_data, file = path(new_data_path, "car_data.RData"))
save(car_train, car_test, file = path(new_data_path, "car_data_splits.RData"))

save(car_data, file = path(recent_data_path, "car_data.RData"))
save(car_train, car_test, file = path(recent_data_path, "car_data_splits.RData"))

# Save the data with combined as the outcome -----------------------------------

car_data <- 
  final_data %>%
  dplyr::rename(mpg = comb) %>%
  select(-city)

car_data %>% 
  group_by(year) %>% 
  count()

# now remove duplciates again based on their exact values
car_data <- 
  car_data %>%
  group_by(
    make, model, eng_displ, cylinders, drive, fuel_type, transmission,
    car_class, start_stop, flexible_fuel, plug_in_hybrid, hybrid,
    turbo_charged, super_charged, mpg
  ) %>% 
  arrange(year) %>%
  slice(1) %>%
  ungroup()

car_data %>% 
  group_by(year) %>% 
  count()

car_train <- car_data %>%
  filter(year <= trn_stop_year)

car_test <- car_data %>%
  filter(year > trn_stop_year)

new_data_path <- path(destination_path, paste0(date_path, "_combined"))
recent_data_path <- path(destination_path, "latest_combined")

dir_create(new_data_path)

save(car_data, file = path(new_data_path, "car_data.RData"))
save(car_train, car_test, file = path(new_data_path, "car_data_splits.RData"))

save(car_data, file = path(recent_data_path, "car_data.RData"))
save(car_train, car_test, file = path(recent_data_path, "car_data_splits.RData"))


# Save raw data file------------------------------------------------------------

file_move(
  path(working_file_path, new_file_name),
  path(destination_path, "raw", new_file_name)
)

# Package versions -------------------------------------------------------------

session_info()

if (!interactive())
  q("no")

