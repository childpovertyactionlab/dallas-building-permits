rm(list=ls(all=TRUE))
library(tidyverse)
library(rio)
library(lubridate)
library(sf)

st_layers("Final Table/DallasPermits.gdb")
Permits_2015 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2015")
Permits_2016 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2016")
Permits_2017 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2017")
Permits_2018 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2018")
Permits_2019 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2019")
Permits_2020 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2020") %>%
  select(-Match_type)
Permits_2021 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2021_thruJune") %>%
  select(-Match_type)

Permits_Join <- rbind(Permits_2015, Permits_2016) %>%
  rbind(., Permits_2017) %>%
  rbind(., Permits_2018) %>%
  rbind(., Permits_2019) %>%
  rename(permit_type = Permit_Type,
         permit_number = PermitNum,
         #permit_groups = Permit_Groups,
         #issued_date = PermitDate,
         address = Address,
         zip_code = Zip_Code,
         contractor = Contractor,
         value = Value,
         area = Area,
         work_description = Work_Description,
         land_use = Land_Use) %>%
  mutate(city = "Dallas",
         state = "TX",
         issued_date = date(PermitDate)) %>%
  select(-X, -Y, -Mapsco, -Count, -PermitDate, -PermitYear) %>%
  rbind(., Permits_2020) %>%
  rbind(., Permits_2021)

names(Permits_Join)

colSums(is.na(Permits_Join))
test <- Permits_Join %>%
  filter(is.na(value)) %>%
  st_drop_geometry(.) %>%
  group_by(permit_type) %>%
  summarize(count = n())

st_write(Permits_Join, "C:/Users/micha/Documents/GitHub/dallas-building-permits/Data/CityofDallas_Permits.geojson", delete_dsn = TRUE)

Permits_Combined <- rbind(Permits_2020, Permits_2021)

Permits_Summary <- Permits_Combined %>%
  st_drop_geometry(.) %>%
  group_by(permit_type) %>%
  summarise(count = n())

names(Permits_Combined)

Permits_LU <- Permits_Combined %>%
  st_drop_geometry(.) %>%
  group_by(land_use) %>%
  summarise(count = n())

st_write(Permits_Summary, "test_records.geojson")

Permits_CommDemo <- Permits_Combined %>%
  st_drop_geometry(.) %>%
  filter(permit_type == "Demolition Permit Commercial") %>%
  group_by(land_use) %>%
  summarize(count = n())
