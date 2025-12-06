# Cleaning Data
# Miami Data First

# Libraries
library(dplyr)
library(stringr)
library(lubridate)
library(janitor)
library(tidyr)

# Clean column names into a new dataset
MiamiData_clean <- MiamiData %>% 
  clean_names()

# Keep only needed columns
MiamiData_clean <- MiamiData_clean %>%
  select(
    ticket_id, issue_type, issue_description, ticket_status,
    street_address, city, state, zip_code,
    latitude, longitude,
    ticket_created_date_time, ticket_closed_date_time,
    goal_days, actual_completed_days, over_due_flag
  )

# Clean up date and numeric columns
MiamiData_clean <- MiamiData_clean %>%
  mutate(
    ticket_created_date_time = ymd_hms(ticket_created_date_time),
    ticket_closed_date_time  = ymd_hms(ticket_closed_date_time),
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude),
    goal_days = as.numeric(goal_days),
    actual_completed_days = as.numeric(actual_completed_days)
  )

# Filter valid Miami rows
MiamiData_clean <- MiamiData_clean %>%
  filter(
    tolower(city) == "miami",
    state %in% c("FL", "Florida"),
    !is.na(latitude),
    !is.na(longitude),
    ticket_status %in% c("OPEN", "CLOSED")
  )

# Remove duplicates
MiamiData_clean <- MiamiData_clean %>% 
  distinct(ticket_id, .keep_all = TRUE)


# Gainsville Data
# Clean column names into a new dataset
GainesvilleData_clean <- GainesvilleData %>%
  clean_names()

# Keep only needed columns
GainesvilleData_clean <- GainesvilleData_clean %>%
  select(
    id, status, request_type, description,
    last_updated, closed,
    minutes_to_acknowledge, minutes_to_close, days_to_close,
    assigned_to, reporter_display,
    address, latitude, longitude,
    location, police_sector, commission_district
  )

# Clean up date and numeric columns
GainesvilleData_clean <- GainesvilleData_clean %>%
  mutate(
    last_updated = mdy_hms(last_updated, quiet = TRUE),
    closed = mdy_hms(closed, quiet = TRUE),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    minutes_to_acknowledge = as.numeric(minutes_to_acknowledge),
    minutes_to_close = as.numeric(minutes_to_close),
    days_to_close = as.numeric(days_to_close)
  )

# Filter valid Gainesville rows
GainesvilleData_clean <- GainesvilleData_clean %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(description),
    !is.na(status),
    status %in% c("Open", "Closed", "Archived")
  )

# Remove duplicate IDs
GainesvilleData_clean <- GainesvilleData_clean %>%
  distinct(id, .keep_all = TRUE)

# Drop rows with NA in important columns
GainesvilleData_clean <- GainesvilleData_clean %>%
  drop_na(
    description, status, latitude, longitude,
    minutes_to_acknowledge, minutes_to_close, days_to_close,
    assigned_to, reporter_display, address,
    police_sector, commission_district
  )

# Remove ID and status ONLY at the end
GainesvilleData_clean <- GainesvilleData_clean %>%
  select(-id, -status)

head(MiamiData_clean, 3)

colnames(GainesvilleData_clean)
colnames(MiamiData_clean)

summary(nchar(GainesvilleData_clean$description))
summary(nchar(MiamiData_clean$issue_description))

write.csv(MiamiData_clean, "MiamiData_clean.csv", row.names = FALSE)
write.csv(GainesvilleData_clean, "GainesvilleData_clean.csv", row.names = FALSE)

