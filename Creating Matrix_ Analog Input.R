library(furrr, warn.conflicts = FALSE)
library(tidyverse)
library(reshape2, warn.conflicts = FALSE)
library(data.table)

## We are creating 3 matrix which will go as an input for analog calulcation
## Matrix A: historical climate normals: 30-year mean across 1990-2020
## Matrix B: projected climate normal: 30-year mean across 2040-2070
## Matrix C: annual historical observations across 1990-2020 to calculate the interannual climatic variability

##Matrix B
data_dir <- path.expand("/data/RCP45/") ## read all files from the directory
filePaths <- list.files(data_dir, "\\.csv$", full.names = TRUE)
rcp_output<- lapply(filePaths,fread)
rcp_output <- rbindlist(rcp_output, use.names = TRUE)

##filter for mid century or time period of interest
rcp_output <- dplyr::filter(rcp_output,year>= 2040 & year<=2070)


Bmatrix <- rcp_output %>% group_by(State_County,model) %>%
  summarise_all(.funs = c("median"))
Bmatrix <- cbind(year = 2055, Bmatrix) ## keeping the year column with 2055 (middle of 2040-70) its not related to any calculation
print("Matrix B created")

write.csv(Bmatrix,"/data/Bmatrix.csv",row.names = F)

data_dir <- path.expand("/data/historical/") ## read historical data
filePaths <- list.files(data_dir, "\\.csv$", full.names = TRUE)
historical_output<- lapply(filePaths,fread)
historical_output <- rbindlist(historical_output, use.names = TRUE)

##filter for time period of interest
historical_output <- dplyr::filter(historical_output,year>= 1990 & year<=2020)


## Matrix A and Matrix C
Amatrix <- historical_output %>% group_by(State_County,model) %>%
  summarise_all(.funs = c("median"))
Amatrix <- cbind(year = 2000, Amatrix)
print("Matrix A created")

Cmatrix <- historical_output %>% group_by(year,State_County,model) %>%
  summarise_all(.funs = c("median"))
Cmatrix <- cbind(year = 2000, Cmatrix)
print("Matrix C created")

write.csv(Amatrix1,"/data/Amatrix.csv",row.names = F)
write.csv(Amatrix1,"/data/Cmatrix.csv",row.names = F)

