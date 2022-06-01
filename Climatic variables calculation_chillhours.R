library(tidyverse, warn.conflicts = FALSE)
library(future, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(furrr, warn.conflicts = FALSE)
library(agclimtools, warn.conflicts = FALSE)
library(chillR)

analog <- function(file_path){

  print(file_path)

  path_split <- str_split(file_path, pattern = "/", simplify = TRUE)

  name_split <- str_split(path_split[,3], pattern = "_", simplify = TRUE)

  lat = as.numeric(name_split[,2])
  lon = as.numeric(name_split[,3])

  df <-
    read_gridmet(paste0("/data/project/agaid/AnalogData_Sid_gr/", file_path), begin = 1990, end = 2020) %>%
    mutate(Year = year(date),
           JDay = yday(date),
           month = month(date),
           Tmin = tmin,
           Tmax = tmax)  %>%
    make_hourly_temps(latitude = lat, keep_sunrise_sunset = FALSE) %>%
    stack_hourly_temps(latitude = lat, keep_sunrise_sunset = FALSE) %>%
    chilling(Start_JDay=274, End_JDay=90)%>%
    mutate(lat = lat,
           lon = lon,
           model = path_split[, 1],
           climate_proj = path_split[, 2])
}

plan(multicore)

## 19 GCM model
models <- list.dirs("/data/", full.names = FALSE, recursive = FALSE)

## text file created by overlaying 1/16 climate grid over CDL
file_name <- read_lines("/data/cdl_points.txt")

## two RCP scenarios
climate_proj <- c("rcp45","rcp85")
climate_proj <- climate_proj[1]

file_path <- expand_grid(models, climate_proj, file_name) %>%
  mutate(file_path = paste(models, climate_proj, file_name, sep = "/")) %>%
  pull(file_path)

## creating list of grid points for which climatic data exists in the database
existing_file_paths <- file_path[file.exists(file.path("/data/", file_path))]

## creating SLURM job array on high performance cluster
args = commandArgs(trailingOnly=TRUE)
## submitting each location as a single job
a_future_loc <- existing_file_paths[as.numeric(args[1])]

print(args)
print(a_future_loc)
## each file name was in following format data_lat_lon
path_split1 <- str_split(a_future_loc, pattern = "/", simplify = TRUE)
name_split1 <- str_split(path_split1[,3], pattern = "_", simplify = TRUE)
## Function for finding frost free season length

## calling the climatic_data function for 1 grid point
df <- climatic_data(a_future_loc)
df$location <- paste0(df$lat, "_", df$lon)
County <- read.csv("/data/point.csv") ## this file contains name of of state and county, location(lat_lon) for each grid points
df <- merge(df,County,by = "location")

out_dir<- "/home/"
write.csv(df, paste0(out_dir,paste(path_split1[,3]),".csv"))
