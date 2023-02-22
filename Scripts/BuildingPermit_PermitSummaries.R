library(tidyverse)
library(rio)
library(lubridate)
library(sf)

libDB <- "C:/Users/micha/CPAL Dropbox/"

st_layers(paste0(libDB, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Dallas Permits.gdb"))

#Permits_2015 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2015")
#Permits_2016 <- st_read("Final Table/DallasPermits.gdb", layer = "Permits_2016")
Permits_2017 <- st_read(paste0(libDB, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Dallas Permits.gdb"), layer = "Permits_2017")
Permits_2018 <- st_read(paste0(libDB, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Dallas Permits.gdb"), layer = "Permits_2018")
Permits_2019 <- st_read(paste0(libDB, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Dallas Permits.gdb"), layer = "Permits_2019")
Permits_2020 <- st_read(paste0(libDB, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Dallas Permits.gdb"), layer = "Permits_2020") %>%
  select(-Match_type)
Permits_2021 <- st_read(paste0(libDB, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Dallas Permits.gdb"), layer = "Permits_2021") %>%
  select(-Match_type)
Permits_2022 <- st_read(paste0(libDB, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Dallas Permits.gdb"), layer = "Permits_2022") %>%
  mutate(area = as.character(area)) %>%
  select(-issued_date) %>%
  distinct(.)

Permits_date <- import(paste0(libDB, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Data/Raw Data/Permits_2022.xlsx")) %>%
  janitor::clean_names(.) %>%
  mutate(issued_date = as.Date(issued_date, format = "%m/%d/%Y")) %>%
  select(permit_number, issued_date)

test <- left_join(Permits_2022, Permits_date)

howmany <- test %>%
  filter(is.na(issued_date))

Permits_2022 <- test %>%
  filter(!is.na(issued_date))

Permits_Join <- rbind(Permits_2017, Permits_2018) %>%
  rbind(., Permits_2019) %>%
  rename(permit_type = Permit_Type,
         permit_number = PermitNum,
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
  rbind(., Permits_2021) %>%
  select(-Status) %>%
  rbind(., Permits_2022)
  
str(Permits_Join)
str(Permits_2022)

permitfilter <- c("Building (BU) Commercial  Addition",
                  "Building (BU) Commercial  Alteration",
                  "Building (BU) Commercial  Finish Out",
                  "Building (BU) Commercial  New Construction",
                  "Building (BU) Commercial  Reconstruction",
                  "Building (BU) Commercial  Renovation",
                  "Building (BU) Multi Family  Addition",
                  "Building (BU) Multi Family  Alteration",
                  "Building (BU) Multi Family  Finish Out",
                  "Building (BU) Multi Family  New Construction",
                  "Building (BU) Multi Family  Reconstruction",
                  "Building (BU) Multi Family  Renovation",
                  "Building (BU) Single Family  Addition",
                  "Building (BU) Single Family  Alteration",
                  "Building (BU) Single Family  Finish Out",
                  "Building (BU) Single Family  New Construction",
                  "Building (BU) Single Family  Reconstruction",
                  "Building (BU) Single Family  Renovation",
                  "Demolition Permit Commercial",
                  "Demolition Permit SFD/Duplex"
)  


permitsfiltered <- Permits_Join %>%
  filter(permit_type %in% permitfilter)

names(Permits_Join)

colSums(is.na(Permits_Join))
test <- Permits_Join %>%
  filter(is.na(value)) %>%
  st_drop_geometry(.) %>%
  group_by(permit_type) %>%
  summarize(count = n())

st_write(permitsfiltered, "C:/Users/micha/Documents/GitHub/dallas-building-permits/Data/CityofDallas_Permits.geojson", delete_dsn = TRUE)

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
