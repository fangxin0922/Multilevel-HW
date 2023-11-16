## read in pm2.5 data ##
pm <- read.csv("C:\\Users\\jenni\\Downloads\\airnowby_site_10.14.2020.csv", header=TRUE, sep=",", na.strings=c("","NA"))
unique(pm$SiteName)

#install.packages("data.table")
library(data.table)
library(dplyr)
#install.packages("bit64")
library(bit64)

## read in mobility data ##
#Get a list of CSV files in the folder
setwd("C:\\Users\\jenni\\OneDrive\\Desktop\\mobility_to_use")
csv_files <- list.files(pattern = ".csv", path = ".")

#Read all CSV files and combine them into one dataset
mobility <- lapply(csv_files, fread) %>%
  bind_rows()

#keep only one row by site for ease of identifying site names that dont match between the PM and mobility datasets
pm_unique <- pm %>% 
  distinct(SiteName, .keep_all = TRUE)

mobility_unique <- mobility %>% 
  distinct(country_region, sub_region_1, sub_region_2, metro_area, .keep_all = TRUE)

## need to do some manual mapping because the names vary between the files ## 
##I exported the files above so I could better look at them and use them in excel to check that the names from each dataset matched, and to fix them if not
#write.csv(pm_unique, file = "C:\\Users\\jenni\\Downloads\\pm_unique.csv", row.names = FALSE)
#write.csv(mobility_unique, file = "C:\\Users\\jenni\\Downloads\\mobility_unique.csv", row.names = FALSE)

## Here i am uploading a file where I manually mapped the name from the PM dataset to the corresponding name in the mobility dataset
#upload file with the corresponding names for mobility and pm data
names <- read.csv("C:\\Users\\jenni\\Downloads\\names.csv", header=TRUE, sep=",", na.strings=c("","NA"))

##add back to the original PM datafile, so we have a place where the PM data is linked to the corresponding mobility site
pm_merged <- merge(names, pm, by.x = c("PM_SiteName"), by.y = "SiteName")

##filter out the mobility data to just keep sites we have PM data for
unique_pm <- as_tibble(unique(pm_merged$Mobility_SiteName))
unique_pm <- na.omit(unique_pm)
mobility <- as_tibble(mobility)

mobility_filtered <- mobility %>%
  filter(rowSums(across(c("country_region_code" ,                              
                                            "country_region"       ,                             
                                                     "sub_region_1"          ,                            
                                                   "sub_region_2"             ,                         
                                                     "metro_area" ), ~. %in% unique_pm$value)) > 0)

columns_to_replace_na <- c("country_region_code", "country_region", "sub_region_1", "sub_region_2", "metro_area")

mobility_filtered <- mobility_filtered %>%
  mutate_at(columns_to_replace_na, ~na_if(., ""))

##some cases where we have multiple overlapping names with the PM Dataset, like when we have "buenos aires" for the PM data, but multiple neighborhoods for the mobility data, in that case we take the "buenos aires" averaged data, to best correspond with the PM data
selected_rows <- mobility_filtered %>%
  group_by(country_region_code, country_region, sub_region_1, sub_region_2, metro_area) %>%
  slice(1) %>%
  ungroup()

### fixing of the mobility data based on what was identified in the step above ###
##select general buenos aires in general (it is broken down by neighborhood, but we do not have that level of granularity for the PM data)
mobility_filtered <- mobility_filtered %>%
  filter(!(sub_region_1 == "Buenos Aires" & !is.na(sub_region_2)))

##we are taking guatemala city in general, even though it does have neighborhood granularity
mobility_filtered <- mobility_filtered %>%
  filter(!(country_region == "Guatemala" & (sub_region_1 != "Guatemala Department" | !is.na(sub_region_2))))

##there are a lot of mobility sites in "India" we are going to take specifically New Delhi, Hyderabad, Chennai, and Mumbai, because we have PM data for each of those cities
mobility_filtered <- mobility_filtered %>%
  filter(!(country_region == "India" & !(sub_region_2 %in% c("New Delhi", "Hyderabad", "Chennai", "Mumbai"))))

## the PM data calls Yangon, Myanmar, Rangoon. Here we are taking just Yangon from the mobility data, although there are a couple other cities and country averaged mobility data for myanmar
mobility_filtered <- mobility_filtered %>%
  filter(!(country_region == "Myanmar (Burma)" & (metro_area != "Yangon Metropolitan Area" )))

##although lima, peru has neighborhood granularity, we are just going to take the city level
mobility_filtered <- mobility_filtered %>%
  filter(!(sub_region_1 == "Lima Region" & !is.na(sub_region_2)))

##fix up the mobility site name column because right now it spans across 3 columns. we just want one column "Mobility_SiteName"
mobility_filtered <- mobility_filtered %>%
  mutate(Mobility_SiteName = case_when(
    is.na(sub_region_1) & is.na(sub_region_2) & is.na(metro_area) ~ as.character(country_region),
    !is.na(sub_region_1) & is.na(sub_region_2) & is.na(metro_area) ~ as.character(sub_region_1),
    is.na(sub_region_2) & !is.na(sub_region_2) & is.na(metro_area) ~ as.character(sub_region_2),
    !is.na(metro_area) ~ as.character(metro_area),
    !is.na(sub_region_2) & is.na(metro_area) ~ as.character(sub_region_2),
    TRUE ~ NA_character_
  )) 

### merge the clean PM and mobility datasets finally! ##

# first remove data from PM dataset for which we have no mobility data
pm_merged <- filter(pm_merged, !is.na(Mobility_SiteName))

#make sure date var is a date
mobility_filtered$date <- as.Date(mobility_filtered$date, format = "%m/%d/%Y")
colnames(pm_merged)[colnames(pm_merged) == "UTC"] <- "date"
pm_merged$date <- as.Date(pm_merged$date, format = "%m/%d/%Y")

check <- filter(final_ds, Mobility_SiteName == "Bangladesh")

#merge the datasets by date and site id. keep all observations for PM even if there is no matching date from the mobility (incase we want to do anything regarding detrending/seasonality etc)
library(tidyr)
final_ds <- merge(mobility_filtered, pm_merged, by = c("Mobility_SiteName", "date"), all.x = TRUE, all.y = TRUE)
final_ds <- final_ds %>% select(c("Mobility_SiteName", "date", "country_region", "retail_and_recreation_percent_change_from_baseline" ,
                                  "grocery_and_pharmacy_percent_change_from_baseline" ,
                                   "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline",   
                                   "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline",       
                                   "PM_SiteName", "a_mean"  , "sd"))

##some further cleaning up
final_ds <- final_ds %>%
  group_by(Mobility_SiteName) %>%
  fill(country_region, .direction = "downup")
#The associated country for Kolkata and Guatemala city did not read in? added it in manually here
final_ds$country_region[final_ds$Mobility_SiteName == "Kolkata"] <- "India"
final_ds$country_region[final_ds$Mobility_SiteName == "Guatemala"] <- "Guatemala"

## read in the HDI data
hdi <- read.csv("C:\\Users\\jenni\\Downloads\\hdi.csv", header=TRUE, sep=",", na.strings=c("","NA"))

#keep only the 2020 data
hdi <- hdi %>% select(c("country", "hdicode", "hdi_2020"))

##fix the names in the HDI dataset that wont merge properly to our final_ds cause theyre spelled dif or whatever reason
hdi$country[hdi$country == "Viet Nam"] <- "Vietnam"
hdi$country[hdi$country == "Lao People's Democratic Republic"] <- "Laos"
hdi$country[hdi$country == "Myanmar"] <- "Myanmar (Burma)"

#merge to the main dataset by country
final_ds <- merge(hdi, final_ds, by.x = "country", by.y = "country_region", all.y = TRUE)

write.csv(final_ds, file = "C:\\Users\\jenni\\Downloads\\final_ds.csv", row.names = FALSE)
