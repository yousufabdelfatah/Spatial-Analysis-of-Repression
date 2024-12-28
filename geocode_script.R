
# load packages
library(ggmap)
library(tidyverse)

### Protests

#########- THIS COSTS MONEY. DO NOT RUN. API KEY REMOVED FROM SCRIPT


# read in the data
df_2011_2018 <- readxl::read_excel("event_catalogue_2_2011-2018 - academic_version.xlsx", skip = 1)
df_2019_2020 <- readxl::read_excel("event_catalogue_2019-2020 - academic_version.xlsx")


#google API Key
# register_google(key = "")


# Extract relevant columns
locations_2011_2018 <- df_2011_2018 %>% 
  select(governorate = "محافظة_الفعالية",
         circuit = "دائرة_الفعالية",
         details =  "مسار_الفعالية_زمنيًا_ومكانيًا")


locations_2019_2020 <- df_2019_2020 %>% 
  select(governorate = "محافظة الفعالية",
         circuit = "الدائرة",
         details = "مسار الفعالية زمنياً ومكانياً")


# Combine columns to form a complete address
locations_2011_2018 <- locations_2011_2018 %>%
  mutate(full_address = paste(details, circuit, governorate, sep = ", "))

locations_2019_2020 <- locations_2019_2020 %>%
  mutate(full_address = paste(details, circuit, governorate, sep = ", "))


# Function to geocode addresses
geocode_addresses <- function(addresses) {
  geocoded_data <- geocode(addresses, output = "latlona", source = "google")
  return(geocoded_data)
}

# Geocode addresses
geocoded_2011_2018 <- geocode_addresses(locations_2011_2018$full_address)
geocoded_2019_2020 <- geocode_addresses(locations_2019_2020$full_address)

# Combine geocoded data with original data
locations_2011_2018 <- cbind(locations_2011_2018, geocoded_2011_2018)
locations_2019_2020 <- cbind(locations_2019_2020, geocoded_2019_2020)


# put a lat/lon column so I can sample and then spot check
locations_2011_2018 <- locations_2011_2018 %>%
  mutate(lat_lon = paste(lat, lon, sep = ", "))

locations_2019_2020 <- locations_2019_2020 %>%
  mutate(lat_lon = paste(lat, lon, sep = ", "))


# sample
sample_n(locations_2011_2018, 10)

### Arrests

# now do the same thing for the arrests
arrests <- read_csv('merged_df.csv')

# let's coalesce the redundant columns we think we're going to need
arrests <- 
  arrests %>% 
  mutate(residence = coalesce(residence, residence_governorate, residence_location)) %>% 
  dplyr::select(-residence_governorate, -residence_location) %>% 
  mutate(governorate = coalesce(governorate, governorate_disappearance, govenorate)) %>% 
  dplyr::select(-governorate_disappearance, -govenorate) %>% 
  mutate(arrest_date = coalesce(arrest_date, as.character(event_date))) %>% 
  dplyr::select(-event_date)


# let's fix the dates
# this next bunch of code is ripped straight off of arrest_analysis.rmd from my 591

standardize_dates <- function(date_vector) {
  standardized_dates <- lapply(date_vector, function(date) {
    # Initialize the result with the original date value
    result <- date
    
    # Try parsing with common date formats using lubridate
    parsed_date <- parse_date_time(date, orders = c("ymd", "mdy", "dmy", "ydm"))
    
    # If a date is successfully parsed, update the result
    if (!is.na(parsed_date)) {
      result <- as.Date(parsed_date)
    }
    # Check if the date might be a numeric Excel date
    else if (!is.na(as.numeric(date)) && as.numeric(date) > 0) {
      # Attempt to convert Excel numeric date to Date
      excel_date <- as.Date(as.numeric(date), origin = "1899-12-30")
      # Update the result if conversion is successful
      if (!is.na(excel_date)) {
        result <- excel_date
      }
    }
    
    # Return the result, which might be the original date or the converted date
    return(result)
  })
  
  return(standardized_dates)
}


# put the output into its own list
new_dates <- standardize_dates(arrests$arrest_date)

sum(is.na(arrests$arrest_date))



# Create two new columns from that list
arrests$arrest_date_dates <- sapply(new_dates, function(j) if(class(j) == "Date") j else NA)
arrests$arrest_date_chars <- sapply(new_dates, function(j) if(class(j) == "character") j else NA)


# turn the numeric value back into actual dates
arrests$arrest_date_dates <- as.Date(arrests$arrest_date_dates, origin = "1970-01-01")


# keep rows where the date is reasonable
arrests <- arrests %>%
  filter(is.na(arrest_date_dates) | 
         (year(arrest_date_dates) >= 2010 & year(arrest_date_dates) <= 2022))


# deal with character dates
# drop if it says غيز
arrests <- arrests %>%
  filter(!grepl("غير", arrest_date_chars))

# drop for هارب
arrests <- arrests %>%
  filter(!grepl("هارب", arrest_date_chars))



# function to convert Arabic months to english months
convert_arabic_month <- function(date_string) {
  # Mapping of Arabic month names to English or numeric equivalents
  months_map <- c(
    "يناير" = "01", "فبراير" = "02", "مارس" = "03", 
    "أبريل" = "04", "مايو" = "05", "يونيو" = "06",
    "يوليو" = "07", "اغسطس" = "08", "سبتمبر" = "09",
    "أكتوبر" = "10", "نوفمبر" = "11", "ديسمبر" = "12",
    "اكتوبر" = "10", "يوينو" = "06", "اغسطس" = "08"
  )
  
  # Replace Arabic month names with their numeric equivalents
  for (month in names(months_map)) {
    date_string <- gsub(month, months_map[month], date_string)
  }
  
  return(date_string)
}



# let's convert using the above and add to its own column
# apply conversion
arrests <- arrests %>%
  mutate(
    # Convert Arabic months in the date strings
    arrest_date_chars_converted = sapply(arrest_date_chars, convert_arabic_month)
  )



# function to convert arabic numbers to English numbers
convert_arabic_numerals <- function(date_string) {
  # Mapping of Arabic numerals to Western Arabic numerals
  arabic_to_western <- setNames(as.character(0:9), c("٠", "١", "٢", "٣", "٤", "٥", "٦", "٧", "٨", "٩"))
  
  # Split the date string into individual characters
  chars <- strsplit(date_string, "")[[1]]
  
  # Convert each Arabic numeral to its Western counterpart
  converted_chars <- sapply(chars, function(char) {
    if (char %in% names(arabic_to_western)) {
      return(arabic_to_western[char])
    } else {
      return(char)
    }
  })
  
  # Collapse the characters back into a single string
  return(paste0(converted_chars, collapse = ""))
}



# apply numeric conversion
arrests <- arrests %>%
  mutate(
    # Convert Arabic numerals in the date strings
    arrest_date_chars_converted = sapply(arrest_date_chars_converted, convert_arabic_numerals)
  )


# preprocess to standardize date formats with slashes
arrests <- arrests %>%
  mutate(
    arrest_date_chars_converted = trimws( # Trim leading/trailing spaces after all replacements
      gsub("_", "/", # Replace underscores with slashes
           gsub("\\|", "/", # Replace pipe characters with slashes
                gsub(" +", " ", # Replace multiple spaces with a single space
                     gsub("/ +", "/", # Remove spaces after slashes
                          gsub(" +/", "/", # Remove spaces before slashes
                               gsub("\\.", "/", # Replace periods with slashes
                                    gsub("-+", "/", # Replace dashes with slashes
                                         gsub("\\\\", "/", # Replace backslashes with slashes
                                              gsub("[\u00A0]+", " ", # Replace non-breaking spaces with a single space
                                                   arrest_date_chars_converted
                                              )
                                         )
                                    )
                               )
                          )
                     )
                )
           )
      )
    )
  )



# add a slash to spaces between numbers
arrests <- arrests %>%
  mutate(
    arrest_date_chars_converted = gsub("(\\d)\\s+(\\d)", "\\1/\\2", arrest_date_chars_converted)
  )


# New code chunk specifically for conditional removal of initial number and slash in strings with 3 slashes
arrests <- arrests %>%
  mutate(
    arrest_date_chars_converted = ifelse(
      # Condition: Check if the string has exactly 3 slashes
      grepl("^[^/]+/[^/]+/[^/]+/[^/]+$", arrest_date_chars_converted),
      # If true: Remove the initial number and slash
      gsub("^\\d+/", "", arrest_date_chars_converted),
      # If false: Leave the string as is
      arrest_date_chars_converted
    )
  )


# Correcting year typos for any year in the 2010s missing the "1"
arrests <- arrests %>%
  mutate(
    arrest_date_chars_converted = ifelse(grepl("/20\\d/", arrest_date_chars_converted),
                                         gsub("/20(\\d)/", "/201\\1/", arrest_date_chars_converted),
                                         arrest_date_chars_converted)
  )

# get rid of text before and after dates
arrests <- arrests %>%
  mutate(
    arrest_date_chars_converted = gsub(".*?(\\d{1,2}/\\d{1,2}/\\d{4}).*", "\\1", arrest_date_chars_converted)
  )



# get rid of text before and after dates that have the format MM/YYYY
arrests <- arrests %>%
  mutate(
    arrest_date_chars_converted = gsub(".*?(\\d{1,2}/\\d{4}).*", "\\1", arrest_date_chars_converted)
  )

# add a day if it's just a month year combo
arrests <- arrests %>%
  mutate(
    arrest_date_chars_converted = gsub("^((0?[1-9]|1[012])/\\d{4})$", "01/\\1", arrest_date_chars_converted)
  )

# when it's عام
arrests <- arrests %>%
  mutate(
    arrest_date_chars_converted = gsub(".*عام\\s*(\\d{4}).*", "1/1/\\1", arrest_date_chars_converted)
  )

arrests <- arrests %>%
  mutate(
    # Trim white spaces
    arrest_date_chars_converted = trimws(arrest_date_chars_converted),
    # Attempt to parse dates with different formats
    arrest_dates_char_updated = case_when(
      !is.na(ymd(arrest_date_chars_converted)) ~ ymd(arrest_date_chars_converted),
      !is.na(dmy(arrest_date_chars_converted)) ~ dmy(arrest_date_chars_converted),
      !is.na(mdy(arrest_date_chars_converted)) ~ mdy(arrest_date_chars_converted),
      # Fallback if none of the above formats match
      TRUE ~ as.Date(NA)
    )
  )

arrests <- arrests %>%
  # Conditionally set arrest_date_chars to NA where arrest_dates_char_updated has a value
  mutate(arrest_date_chars = if_else(!is.na(arrest_dates_char_updated), NA_character_, arrest_date_chars)) %>%
  # Drop the arrest_date_chars_converted column
  dplyr::select(-arrest_date_chars_converted)

# now let's combine our dates columns
arrests <- 
  arrests %>% 
  mutate(arrest_date = coalesce(arrest_date_dates, arrest_dates_char_updated)) %>% 
  dplyr::select(-arrest_date_dates, -arrest_dates_char_updated, arrest_date_chars)

arrests <- arrests %>% 
  drop_na(arrest_date)

rm(new_dates)

arrests <- arrests %>%
  mutate(
    full_address = paste(
      coalesce(arrest_location, ""),
      coalesce(town, ""),
      coalesce(police_circuit, ""),
      coalesce(city, ""),
      coalesce(governorate, ""),
      sep = ", "
    ))

# if there are 4 commas make it one
arrests$full_address <- str_replace_all(arrests$full_address, ", , , ," , ",")
# if there are 3 commas make it one
arrests$full_address <- str_replace_all(arrests$full_address, ", , ," , ",")
# if there are 2 commas make it one
arrests$full_address <- str_replace_all(arrests$full_address, ", ," , ",")

# remove leading and trailing commas
arrests$full_address = str_replace_all(arrests$full_address, "^,|,$", "") 

# remove leading and trailing spaces
arrests$full_address = str_trim(arrests$full_address, side = "both")


# turn empty strings into na
arrests$full_address = na_if(arrests$full_address, "")


#drop if full address is na
arrests <- arrests %>% 
  drop_na(full_address)


# Geocode addresses
geocoded_arrests <- geocode_addresses(arrests$full_address)

# Combine geocoded data with original data
arrests <- cbind(arrests, geocoded_arrests)

# put a lat/lon column so I can sample and then spot check
arrests <- arrests %>%
  mutate(lat_lon = paste(lat, lon, sep = ", "))




# sample
sample_n(arrests, 10)


# write to csv


# using residence see distance between where people were arrested and where they live





