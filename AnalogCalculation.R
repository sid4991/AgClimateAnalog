
## Create output folders

## main_out <- "/define main parent directory/"
## out_dir <- paste0(main_out, "sub directory for different scenarios/")

## Read the historical data and projected data
Historical <- read.csv("/data/Amatrix_gridmet.csv")
HistoricalICV <- read.csv("/data/Cmatrix_gridmet.csv")

## Projected matrix different for RCP 8.5 and RCP 4.5
Projected <- read.csv("/data/Bmatrix.csv")

## Defining varibales
time <- "2040_70"
emission <- "RCP85"
models <- unique(Projected$model)
a_model <- models[1]
Projected <- Projected %>% filter(model == a_model)
Projected <- Projected[,-HistoricalICV(3)]

#Select the variables which will be used to calculate the Analogs
Historical <- Historical[,HistoricalICV(1:2,5,13:16)]
Projected <- Projected[,HistoricalICV(1:2,5,13:16)]
HistoricalICV <- HistoricalICV[,HistoricalICV(1:2,5,13:16)]


print (paste0("out_dir is : ",  out_dir))
if (dir.exists(out_dir) == F) { dir.create(path = out_dir, recursive = T) }



local_locations <- Projected$State_County
trunc.SDs <- 0.1 # Principal component truncation rule
non_numeric_cols <- colnames(Historical)[HistoricalICV(1:2)]
numeric_cols <- colnames(Historical)[3:7]

n_neighbors <- ## number of neighbours
NN_dist_tb <- data.table()
NNs_loc_year_tb <- data.table()
NN_sigma_tb <- data.table()
t_executed <- data.table()
executed <- data.table()

args = commandArgs(trailingOnly=TRUE)
a_future_loc <- local_locations[as.numeric(args[1])]
print(args)
print(a_future_loc)


Bj <- Projected %>% filter(State_County==a_future_loc)
Cj <- HistoricalICV %>% filter(State_County==a_future_loc)
print (dim(Cj))

## Step 1: express climate data as standardized anomalies of reference period (1990-2020)
Cj.sd <- apply(Cj[, numeric_cols, drop=F], MARGIN=2, FUN = sd, na.rm = T)
Cj.sd[Cj.sd<(10^-10)] = 1

A_prime <- Historical
A_prime[, numeric_cols] <- sweep(A_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`)

Bj_prime <- Bj
Bj_prime[, numeric_cols] <-sweep(Bj_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`)
Cj_prime <- Cj
Cj_prime[, numeric_cols] <-sweep(Cj_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`)

## Step 2: Extract the principal components (PCs) of the reference period ICV and project all data onto these PCs
PCA <- prcomp(Cj_prime[, numeric_cols][!is.na(apply(Cj_prime[, numeric_cols], 1, mean)) ,])
PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))

X <- as.data.frame(predict(PCA, A_prime))
X <- cbind(A_prime[, non_numeric_cols], X)

Yj <- as.data.frame(predict(PCA, Bj_prime[, numeric_cols]))
Yj <- cbind(Bj_prime[, non_numeric_cols], Yj)

Zj <- as.data.frame(predict(PCA, Cj_prime[, numeric_cols]))
Zj <- cbind(Cj_prime[, non_numeric_cols], Zj)

## Step 3a: express PC scores as standardized anomalies of reference interannual variability
Zj_sd <- apply(Zj[, (1+length(non_numeric_cols)):(PCs + length(non_numeric_cols)), drop=F], MARGIN = 2, FUN = sd, na.rm=T)
X_prime <- sweep(X[, (1+length(non_numeric_cols)):(PCs + length(non_numeric_cols))], MARGIN=2, Zj_sd, FUN = `/`)
X_prime <- cbind(X[, non_numeric_cols], X_prime)
Yj_prime <- sweep(Yj[, (1+length(non_numeric_cols)):(PCs + length(non_numeric_cols))], MARGIN=2, Zj_sd, FUN = `/`)
Yj_prime <- cbind(Yj[, non_numeric_cols], Yj_prime)


X_prime <- na.omit(X_prime)
Yj_prime <- na.omit(Yj_prime)
X_prime[sapply(X_prime, is.infinite)] <- NA
X_prime <- X_prime[complete.cases(X_prime), ]


NN_list <- get.knnx(data = X_prime[, (1+length(non_numeric_cols)):(PCs+length(non_numeric_cols))],
                    query= Yj_prime[, (1+length(non_numeric_cols)):(PCs+length(non_numeric_cols))],
                    k=n_neighbors, algorithm="brute") # Euclidean nearest neighbour distance in the z-standardized PCs of interannual climatic variability, i.e. the Mahalanobian nearest neighbour.

NN_chi <- EnvStats::pchi(as.vector(NN_list$nn.dist), PCs) # percentile of the nearest neighbour distance on the chi distribution with degrees of freedom equaling the dimensionality of the distance measurement (PCs)

NN_sigma <- EnvStats::qchi(NN_chi, 1) # values of the chi percentiles on a standard half-normal distribution (chi distribution with one degree of freedom)

NN_idx <- NN_list$nn.index
NN_dist <- NN_list$nn.dist %>% data.table()

NNs_loc_year <- as.vector(X_prime[as.vector(NN_idx), HistoricalICV('State_County')])
NNs_loc_year <- Reduce(cbind,
                       split(NNs_loc_year,
                             rep(1:n_neighbors, each=1))) %>% data.table()

names(NNs_loc_year)[seq(1,ncol(NNs_loc_year),2)] <- paste0("Loc_NN_", HistoricalICV(1:n_neighbors))

NN_sigma <- Reduce(cbind,
                   split(NN_sigma,
                         rep(1:n_neighbors, each=(length(NN_sigma)/n_neighbors)))) %>% data.table()
names(NN_sigma)[1] <- "V1"

names(NN_dist) <- paste0("NN_", HistoricalICV(1:n_neighbors))
names(NN_sigma) <- paste0("sigma_NN_", HistoricalICV(1:n_neighbors))

NN_dist <- cbind(Yj[, HistoricalICV("State_County")], NN_dist)
NN_sigma <- cbind(Yj[, HistoricalICV("State_County")], NN_sigma)
NNs_loc_year <- cbind(Yj[, HistoricalICV("State_County")], NNs_loc_year)



NN_dist_tb <- rbind(NN_dist_tb, NN_dist)
NN_sigma_tb <- rbind(NN_sigma_tb, NN_sigma)
NNs_loc_year_tb <- rbind(NNs_loc_year_tb, NNs_loc_year)

NNs_loc_year_tb <- cbind(NNs_loc_year_tb,NN_sigma_tb[,HistoricalICV(2:6)])
NNs_loc_year_tb <-  NNs_loc_year_tb[, HistoricalICV(1,2,7,3,8,4,9,5,10,6,11)]
names(NNs_loc_year_tb) <- HistoricalICV("State","N1","V1","N2","V2","N3","V3","N4","V4","N5","V5")
NNs_loc_year_tb<- reshape2:: melt(NNs_loc_year_tb,id.vars = "State")
rm(NN_dist,NN_sigma)

distance_col_names <- HistoricalICV("State_County",paste0("NN_", HistoricalICV(1:n_neighbors)))
write.csv(NNs_loc_year_tb, paste0(out_dir, paste("/NN_loc_year_tb",
                                                 a_future_loc, gsub("-", "_", a_model), time, emission, sep="_"), ".csv"))
