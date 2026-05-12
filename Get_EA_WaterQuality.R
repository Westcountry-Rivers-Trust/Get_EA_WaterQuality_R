# Information -------------------------------------------------------------

## A custom function to get Environment Agency water quality monitoring data via the Water Quality API
## It's maybe a little clunky, but it should work.

## API info:
# https://gist.github.com/canwaf/2afa25fc6160efb25ac72b7acd60278d
# https://environment.data.gov.uk/water-quality-beta/api-docs 

## Includes a worked example with multiple determinands, and how to join the data frames
# 0076; Temp Water; Temperature of Water
# 0077; Cond @ 25C; Conductivity at 25 C
# 0180; Orthophospht; Orthophosphate, reactive as P
# 6396; TurbidityNTU; Turbidity

## n.b. The primary example here is for downloading data at regional scales, but additional examples will be added in time, e.g. 
# Sub-areas
# Environment Agency sampling locations
# Radius around coordinates

## Last updated: 2026-01-06
## Author: Francis Rowney



# Setup -------------------------------------------------------------------

## Set your working directory
setwd("C:/Users/...")

## pacman::p_load() loads packages, and also installs them if you don't have them
if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  readr,
  httr, 
  jsonlite, 
  purrr, 
  tidylog,
  dplyr,
  stringr,
  tidyr,
  lubridate,
  beepr
)



# Set up the function -----------------------------------------------------

## The new EA water quality API has some limitations compared to the previous version.
# The maximum limit per call is 2500 (or less, depending on which "Accept" option is used).
# A maximum of one year of data can be called at one time.
# Multiple determinands cannot be requested simultaneously.

## This function loops through API calls month-by-month, to get around the limits.
# Also converts phenomenonTime to POSIXct, and adds a separate Date column.

getdata_eawq <- function(area, determinand, start_date, end_date){
  
  # Generate sequence of month starts
  month_starts <- seq(start_date, end_date, by = "month")
  month_ends <- c(month_starts[-1] - 1, end_date)
  
  # Initialize empty list to store results
  data_list <- list()
  
  # Loop through each month
  for (i in seq_along(month_starts)) {
    
    # Message to let the user know what's happening
    cat("Getting data for:", format(month_starts[i], "%Y-%m"), "\n")
    
    # API call
    response <- POST(
      "https://environment.data.gov.uk/water-quality/data/observation?",
      query = list(
        determinand = determinand, 
        dateFrom = format(month_starts[i], "%Y-%m-%d"),
        dateTo = format(month_ends[i], "%Y-%m-%d"),
        limit = 2500,
        precannedArea = area
      ), 
      add_headers(Accept = "text/csv")
    )
    
    # Extract content
    data_list[[i]] <- content(response, as = "text", encoding = "UTF-8")
    
    # Add a half-second delay to help keep the API running smoothly
    Sys.sleep(0.5)
  }
  
  # Combine all months into single dataframe
  # Set everything to character to avoid binding issues related to non-numeric results (e.g. "<1"). This can be dealt with later once you've got the data. 
  bind_rows(
    lapply(data_list, function(x) {
      tryCatch(
        read.csv(text = x, colClasses = "character"), 
        error=function(e) NULL
      )
    })
  ) %>% 
    drop_na(result) %>% # Get rid of any dodgy rows
    mutate(phenomenonTime = as_datetime(phenomenonTime)) %>% # Convert to POSIXct
    mutate(Date = as_date(phenomenonTime)) # Add a Date column
}



# Worked example ----------------------------------------------------------

#### Parameters

### Area
## Two example options are given here, but there are probably more options for precannedArea. However, they are not fully documented/published.
## It is also possible to use a GeoJSON polygon for your area of interest instead of precannedArea.

# For an Environment Agency/Natural England area, use "environment_agency,[CODE]", e.g. "environment_agency,DCS"
# https://naturalengland-defra.opendata.arcgis.com/maps/administrative-boundaries-environment-agency-and-natural-england-public-face-areas/explore
area <- "environment_agency,DCS"

# For local authority areas, use "local_authority,[CODE]", e.g. "local_authority,E06000002"
# https://www.ons.gov.uk/aboutus/transparencyandgovernance/freedomofinformationfoi/lookuptableforukauthoritycodes2024
area <- "local_authority,E06000002"


### Date range
## Make sure you follow this format: "YYYY-MM-DD", or use Sys.Date() for today's date.
## Sys.Date() can also be modified for time relative to the present, e.g. Sys.Date()-7 is a week ago.
start_date <- as.Date("2025-01-01")
end_date <- as.Date("2025-12-31")



#### Get the data for each determinand 

# Temp Water; Temperature of Water
temp <- getdata_eawq(
  area=area, 
  determinand="0076",
  start_date=start_date, 
  end_date=end_date
) %>% 
  rename(`Temp Water` = result)


# Cond @ 25C;Conductivity at 25 C
cond <- getdata_eawq(
  area=area, 
  determinand="0077",
  start_date=start_date, 
  end_date=end_date
) %>% 
  rename(`Cond @ 25C` = result)


# Orthophospht;	Orthophosphate, reactive as P
phos <- getdata_eawq(
  area=area, 
  determinand="0180",
  start_date=start_date, 
  end_date=end_date
) %>% 
  rename(`Orthophospht` = result)


# TurbidityNTU;	Turbidity
turb <- getdata_eawq(
  area=area, 
  determinand="6396",
  start_date=start_date, 
  end_date=end_date
) %>% 
  rename(`TurbidityNTU` = result)



#### Join the data frames together and export as CSV 

### Create a list of the data frames
df_list <- list(temp, cond, phos, turb)

### Remove columns that are specific to determinands and join the data frames
wq_data <- lapply(df_list, function(x) {
  x %>% 
    select(-c(id, determinand.notation, determinand.prefLabel, unit))
}) %>% 
  reduce(full_join) %>% 
  arrange(samplingPoint.region, samplingPoint.area, samplingPoint.subArea, samplingPoint.prefLabel, Date) %>% 
  relocate(Date)

### Export the dataset as a CSV
write_csv(wq_data, "FILENAME.csv")




# Getting data for a specific EA location ---------------------------------


## If you want data from just one Environment Agency sampling point:
getdata_eawq_loc <- function(location, determinand, start_date, end_date){
  
  # Generate sequence of month starts
  month_starts <- seq(start_date, end_date, by = "month")
  month_ends <- c(month_starts[-1] - 1, end_date)
  
  # Initialize empty list to store results
  data_list <- list()
  
  # Loop through each month
  for (i in seq_along(month_starts)) {
    
    # Message to let the user know what's happening
    cat("Getting data for:", format(month_starts[i], "%Y-%m"), "\n")
    
    # API call
    response <- POST(
      "https://environment.data.gov.uk/water-quality/data/observation?",
      query = list(
        pointNotation = location,  
        #precannedArea = area, 
        determinand = determinand, 
        dateFrom = format(month_starts[i], "%Y-%m-%d"),
        dateTo = format(month_ends[i], "%Y-%m-%d"),
        limit = 2500
      ), 
      add_headers(Accept = "text/csv")
    )
    
    # Extract content
    data_list[[i]] <- content(response, as = "text", encoding = "UTF-8")
    
    # Add small delay to be respectful to the API
    Sys.sleep(0.5)
  }
  
  # Combine all months into single dataframe
  # Set everything to character to avoid binding issues related to non-numeric results (e.g. "<1"). This can be dealt with later once you've got the data. 
  bind_rows(
    lapply(data_list, function(x) {
      tryCatch(
        read.csv(text = x, colClasses = "character"), 
        error=function(e) NULL
      )
    })
  ) %>% 
    drop_na(result) %>% # Get rid of any dodgy rows
    mutate(phenomenonTime = as_datetime(phenomenonTime)) %>% # Convert to POSIXct
    mutate(Date = as_date(phenomenonTime)) # Add a Date column
}


#### Worked example

### EA sampling point
location <- "NE-45100054"

### Date range
## Make sure you follow this format: "YYYY-MM-DD", or use Sys.Date() for today's date.
## Sys.Date() can also be modified for time relative to the present, e.g. Sys.Date()-7 is a week ago.
start_date <- as.Date("2025-01-01")
end_date <- as.Date("2025-12-31")


# Temp Water; Temperature of Water
temp <- getdata_eawq_loc(
  location=location, 
  determinand="0076",
  start_date=start_date, 
  end_date=end_date
) %>% 
  rename(`Temp Water` = result)
