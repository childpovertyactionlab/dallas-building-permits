rm(list=ls(all=TRUE))
library(tidyverse)
library(rio)
library(lubridate)
library(sf)
library(arcgisbinding) #needed in order to write or delete to gdb, can still read from gdb with sf package only

#### Geodatabase containing all geocoded records
st_layers("Final Table/DallasPermits.gdb")

#### Import most recent geocoded permits data from gdb
#### The Permits_Newest and Permits_Older dataframes will likely have to be edited on a case by case basis based on geocoding conducted
Permits_Newest <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_June2020thruJune2021") %>%
  select(-Addr_type) %>%
  st_transform(crs = 6584) %>%
  select(-(issued_month:issued_week), -Match_addr)

Permits_Older <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2020_thruMay") %>%
  rename(permit_type = USER_Permit_Type,
         permit_number = USER_Permit_No,
         permit_groups = USER_Permit_Groups,
         issued_date = USER_Issued_Date,
         address = USER_Address,
         zip_code = USER_Zip_Code,
         contractor = USER_Contractor,
         value = USER_Value,
         area = USER_Area,
         work_description = USER_Work_Description,
         land_use = USER_Land_Use,
         city = USER_City,
         state = USER_State) %>%
  select(-X, -Y, -permit_groups)

Permits_Join <- rbind(Permits_Older, Permits_Newest)

#Filter out and save records for the most recent fully completed year if file does not already exist within geodatabase
permits_2020 <- Permits_Join %>%
  filter(issued_date >= as.Date("2020-01-01") & issued_date < as.Date("2021-01-01"))

arc.write("Final Table/DallasPermits.gdb/Permits_2020", permits_2020)

#Filter out and save records for the most recent partially completed year
Permits_Current <- Permits_Join %>%
  filter(issued_date >= as.Date("2021-01-01") & issued_date < as.Date("2022-01-01"))
arc.write("Final Table/DallasPermits.gdb/Permits_2021_thruJune", permits_2021)

#Delete any duplicate record layerse now that full year data is exported
st_layers("Final Table/DallasPermits.gdb")
arc.delete("Final Table/DallasPermits.gdb/Permits_June2020thruJune2021")
st_layers("Final Table/DallasPermits.gdb")

#selection of permits of interest into distinct groups
#add new permit selections based on the above PermitType dataframe
PermitDemolition <- Permits_Current %>%
  filter(permit_type %in% c("Demolition Permit Commercial")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

PermitCommercial <- Permits_Current %>%
  filter(permit_type %in% c("Building (BU) Commercial  Reconstruction", "Building (BU) Commercial  Renovation")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

PermitMultiFamilyImp <- Permits_Current %>%
  filter(permit_type %in% c("Building (BU) Multi Family  Renovation", "Building (BU) Multi Family  Alteration")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

mfnewconstruction <- Permits_Current %>%
  filter(permit_type %in% c("Building (BU) Multi Family  New Construction")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

PermitNewCommercial <- Permits_Current %>%
  filter(permit_type %in% "Building (BU) Commercial  New Construction") %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

##Demolition Single Family##
sfnewconstruction <- Permits_Current %>%
  filter(permit_type %in% c("Building (BU) Single Family  New Construction")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

sfdemolition <- Permits_Current %>%
  filter(permit_type %in% c("Demolition Permit SFD/Duplex")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

sfimprovement <- Permits_Current %>%
  filter(permit_type %in% c("Single Family  Alteration or Reconstruction or Renovation")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

#Export created sf dataframes into geodatabase
arc.write("Final Table/Permit Clean/PermitsbyType.gdb/Permits_2021_Demolition", PermitDemolition)
arc.write("Final Table/Permit Clean/PermitsbyType.gdb/Permits_2021_Commercial", PermitCommercial)
arc.write("Final Table/Permit Clean/PermitsbyType.gdb/Permits_2021_NewCommercial", PermitNewCommercial)

arc.write("Final Table/Permit Clean/PermitsbyType.gdb/Permits_2021_SingleFamilyImprovement", sfimprovement)
arc.write("Final Table/Permit Clean/PermitsbyType.gdb/Permits_2021_SingleFamilyDemolition", sfdemolition)
arc.write("Final Table/Permit Clean/PermitsbyType.gdb/Permits_2021_SingleFamilyNew", sfnewconstruction)
arc.write("Final Table/Permit Clean/PermitsbyType.gdb/Permits_2021_MultiFamilyNew", mfnewconstruction)
