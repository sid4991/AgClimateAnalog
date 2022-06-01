library(tidyverse, warn.conflicts = FALSE)
library(future, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(furrr, warn.conflicts = FALSE)
library(agclimtools, warn.conflicts = FALSE)

##Function for calculating agro climatic variables
climatic_data <- function(file_path){

  print(file_path)

  path_split <- str_split(file_path, pattern = "/", simplify = TRUE)

  name_split <- str_split(path_split[,3], pattern = "_", simplify = TRUE)

  lat = as.numeric(name_split[,2])
  lon = as.numeric(name_split[,3])

  df <-
    read_gridmet(paste0("/data/project/agaid/AnalogData_Sid_gr/", file_path), begin = 1990, end = 2020) %>%
    mutate(year = year(date),
           doy = yday(date),
           month = month(date)) %>%
    add_sunrise_sunset(lat = lat)

  df %>%
    expand_grid(hour = seq(ceiling(min(df$sunrise)), floor(max(df$sunset)))) %>%
    mutate(temp = temp_after_sunrise(hour, tmin, tmax, sunrise, sunset)) %>%
    group_by(year,month,date,tmax, tmin, precip) %>%
    summarise(hours_above_th = sum(temp > 32)) %>%
    mutate(gdd_gen = (tmax + tmin)/2) %>%
    group_by(year, month) %>%
    summarise(gdd_gen = sum(if_else(gdd_gen < 0 & gdd_gen> 29.44, 0, gdd_gen)),## growing degree day for base threshold 0c and upper threshold 29.44c
              heat_dh_32 = sum(hours_above_th),## heat degree hours above 32
              Precip = sum(precip),
              growing_spell(tmin,0))  %>%  ## frost free season length
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

df <- recast(df, location + year + lat + lon + model + climate_proj + State_County  ~ variable + month, id.var = c("location","year", "lat", "lon", "model", "climate_proj","State_County", "month"))
df <- df %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

df[, "Pr_max"] <- apply(df[, xx:yy], 1, max) ## xx:yy column number with monthly Pr_max
df[, "Pr_min"] <- apply(df[, xx:yy], 1, min) ## xx:yy column number with monthly Pr_min
df$PrUniformity <- df$Pr_max - df$Pr_min ##creating precipitation uniformity variable by subtracting wettest month and driest month

df[, "Heat_Degree_Hours_Summer_32"] <- apply(df[, xx:yy], 1, sum) ## summing the heat degree hours for summer xx:yy
df$GDD_Calendar = rowSums(df[,c(xx:yy)]) ## summing the monthly GDD in xx:yy columns to annual
df$Season_Length = rowSums(df[,c(xx:yy)]) ## annual season length

df <- df[-c(xx:yy)] ## delete the unwanted data in columns xx:yy

out_dir<- "/home//" ## define output directory it will save data for all the grid points
write.csv(df, paste0(out_dir,paste0(path_split1[,3],"_",path_split1[,2],"_",path_split1[,1]),".csv"))
