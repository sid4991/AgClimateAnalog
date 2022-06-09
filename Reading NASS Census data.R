## Reading the NASS census data
## data can be accessed from https://www.nass.usdata.gov/Quick_Stats/CDQT/chapter/1/table/1/year/2017

data <- readata.dataelim2("/2017_cdataqt_dataata.txt")

## reading county level data
data <- data[data$AGG_LEVEL_DESC == "COUNTY",]

## select the sector
data <- data[data$SECTOR_DESC == "CROPS",]

## Table 29 selects data for vegetables
veg <- data[data$CENSUS_TABLE == 29,]

## Table 31 selects data for tree fruit and nuts (citrus and non citrus also part of this)
tfn <- data[data$CENSUS_TABLE == 31,]

## Table 32 selects data for berries
berries <- data[data$CENSUS_TABLE == 32,]

## unique(berries$SHORT_DESC) will give list of all variables under the column SHORT_DESC
## We are interested in Acres
berries_i <- berries[berries$SHORT_DESC == "BERRY TOTALS, IRRIGATED - ACRES GROWN",]
nut <- tfn[tfn$SHORT_DESC == "TREE NUT TOTALS - ACRES BEARING & NON-BEARING",]
citrus <- tfn[tfn$SHORT_DESC == "CITRUS TOTALS - ACRES BEARING & NON-BEARING",]
noncitrus <- tfn[tfn$SHORT_DESC == "NON-CITRUS TOTALS, (EXCL BERRIES) - ACRES BEARING & NON-BEARING",]
veg <- veg[veg$SHORT_DESC == "VEGETABLE TOTALS, IN THE OPEN - ACRES HARVESTED",]

berries_i$Berries <- as.numeric(gsub(",","",berries_i$VALUE))
berries_State <- berries_i %>% group_by(STATE_ALPHA) %>% summarise(berriesSum = sum(Berries, na.rm=T))
berries_i <- berries_i[c(10,11,13,16)]

nut$Nut <- as.numeric(gsub(",","",nut$VALUE))
nut_State <- nut %>% group_by(STATE_ALPHA) %>% summarise(nutSum = sum(Nut, na.rm=T))
nut <- nut[c(10,11,13,16)]

citrus$Citrus <- as.numeric(gsub(",","",citrus$VALUE))
citrusSum_State <- citrus %>% group_by(STATE_ALPHA) %>% summarise(citrusSum = sum(Citrus, na.rm=T))
citrus <- citrus[c(10,11,13,16)]

noncitrus$NonCitrus <- as.numeric(gsub(",","",noncitrus$VALUE))
noncitrusSum_State <- noncitrus %>% group_by(STATE_ALPHA) %>% summarise(noncitrusSum = sum(NonCitrus, na.rm=T))
noncitrus <- noncitrus[c(10,11,13,16)]

veg$Vegetable <- as.numeric(gsub(",","",veg$VALUE))
VegSum_State <- veg %>% group_by(STATE_ALPHA) %>% summarise(VegSum = sum(Vegetable, na.rm=T))
veg <- veg[c(10,11,13,16)]

## We get county level data for acres under tree and fruit nuts, berries, vegetables
