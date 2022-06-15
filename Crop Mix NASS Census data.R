##CropShareCommonality

CropMix <- data.frame()

## saving the list of unique vegetables, fruit tree nuts and other crops
## https://quickstats.nass.usda.gov/

veglist <- read.csv("/9060BDD9-965E-359A-9553-AC8C162D0B1F.csv")
veglist=c(unique(veglist$Commodity))
otherlist <- read.csv("/5EB2F751-BCCF-36AA-B8BD-218DDFD19F50.csv")
otherlist=c(unique(otherlist$Commodity))
otherlist <- c(otherlist,c("HAY","CORN FOR GRAIN","CORN FOR SILAGE","SORGHUM FOR GRAIN","PEAS"))

fruittreenutlist <- read.csv("/367DD72A-56AD-3765-94AE-C344FCEAC1BB.csv")
fruittreenutlist=c(unique(fruittreenutlist$Commodity))
fruittreenutlist <- c(fruittreenutlist,c("BERRIES","BLUEBERRIES","RASPBERRIES" ))

## list of counties
County <- unique(NASS_stat$value)

for (location in County){
  Temp <- data.frame()
  print(location)
  data <- dplyr::filter(NASS_stat,value == location)
  data <- data %>% dplyr::rename(State_county = value)
  data <- data[-c(14,15,30,31,57,58,179,180)]

  data$CHERRIES = rowSums(data[,c(11,12)])
  data$GRAPES = rowSums(data[,c(17,18)])
  data$LEMONS = rowSums(data[,c(23,24)])
  data$ORANGES = (data[,c(29)])
  data$PEACHES = (data[,c(34)])
  data$PEARS = (data[,c(37)])
  data$PECANS = (data[,c(40)])
  data$PLUMS = (data[,c(47)])
  data$BERRIES = rowSums(data[,c(54,55)])
  data$BLUEBERRIES = (data[,c(57)])
  data$RASPBERRIES = (data[,c(65)])
  data$BEANS = rowSums(data[,c(74,76)])
  data$CABBAGE = rowSums(data[,c(84,86,88)])
  data$GREENS = rowSums(data[,c(109,111,113,115)])
  data$LETTUCE = (data[,c(120)])
  data$MELONS = rowSums(data[,c(124,126,127)])
  data$ONIONS = rowSums(data[,c(131,133)])
  data$PEAS = rowSums(data[,c(137,139,141)])
  data$PEPPERS = rowSums(data[,c(143,145)])
  data$SQUASH = (data[,c(157)])
  data$SUGARCANE = (data[,c(187)])
  data$CORN = (data[,c(163)])

  data <- data[-c(11,12,17,18,23,24,29:31,34:42,46:48,54:59,65:68,74:77,84:88,109:116,120:128,137:146,157:163,186:187)]

  data1 <- melt(data,id.vars= "State_county")


  Temp <- data1[c(4:nrow(data1)),]
  Temp$value <- as.numeric(Temp$value)
  Temp <- Temp[order(- Temp$value),]
  Temp <- filter(Temp, !grepl('FRESH MARKET', variable))
  Temp <- filter(Temp, !grepl('PROCESSING', variable))
  Temp$variable <- gsub("HAY & HAYLAGE ", "HAY", Temp$variable)
  Temp$variable <- gsub("TOMATOES, IN THE OPEN ", "TOMATOES", Temp$variable)
  Temp$variable <- gsub("VEGETABLE TOTALS, IN THE OPEN", "VEGETABLE", Temp$variable)
  Temp$variable <- gsub("BLACKBERRIES, INCL DEWBERRIES & MARIONBERRIES ", "BLACKBERRIES", Temp$variable)
  Temp$variable <- gsub("SORGHUM, GRAIN", "SORGHUM FOR GRAIN", Temp$variable)
  Temp$variable <- gsub("CORN, GRAIN ", "CORN FOR GRAIN", Temp$variable)
  Temp$variable <- gsub("CORN, SILAGE  ", "CORN FOR SILAGE", Temp$variable)
  Temp$variable <- gsub("[[:space:]]", "", Temp$variable)
  Temp$variable <- str_to_title(Temp$variable)

  fruittreenutlist <- str_to_title(fruittreenutlist)
  veglist <- str_to_title(veglist)
  otherlist <- str_to_title(otherlist)

  Temp <- Temp %>% filter(!variable %in% otherlist)

  Temp <- Temp %>% drop_na()
  CropMix<- rbind(CropMix,Temp)

}
## file with analogs for each county

analog <- read.csv("/RCP45_2040_70.csv")
analog <- filter(analog, Category == "Speciality Crop")
analog <- analog[-c(3:8)]

CommonCrop <- data.frame()
CommonCrop_Score <- data.frame(Target=c(1),State=c(1),Score=c(1),Total=c(1),Value =c(1))

for (i in unique(analog$State)){
  data <- filter(analog,State == i)
  data <- filter(data,value != i)
  list_analog <- data$value
  Crop <- filter(CropMix,State_county == i)
  for (j in unique(list_analog)){
    AnalogCrop <- filter(CropMix,State_county == j)
    if(nrow(AnalogCrop)==0) next
    cat(j)
    AllCrop <- rbind(Crop,AnalogCrop)
    AllCrop$Score <- sum(duplicated(AllCrop$variable))
    CommonCrop_Score$State <- unique(AnalogCrop$State)
    CommonCrop_Score$Score <- unique(AllCrop$Score)
    CommonCrop_Score$Total <- length(unique(AllCrop$variable))
    CommonCrop_Score$Target <- i
    CommonCrop_Score$Value <- round(CommonCrop_Score$Score/CommonCrop_Score$Total,2)
    CommonCrop <- rbind(CommonCrop,CommonCrop_Score)
    CommonCrop <- CommonCrop %>%
      group_by(Target) %>%
      slice(which.max(Value))
  }
}
write.csv(CommonCrop,"Common_crop_FNV_RCP45.csv",row.names = FALSE)
