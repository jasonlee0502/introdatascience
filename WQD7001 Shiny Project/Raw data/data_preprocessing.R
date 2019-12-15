Data <- read.csv("listings_cleaned.csv", header = TRUE, stringsAsFactors = FALSE)
Data <- Data[,-1]

Data$host_id <- as.factor(Data$host_id)
Data$last_review <- as.Date(Data$last_review, format = "%d/%m/%Y")
colN <- c("listing", "host_id", "host_name", "neighbourhood_group", "neighbourhood", "lat", "long", "room_type", "price", "min_night"
          , "no_of_review", "last_review_date", "Rpmon", "host_list_cnt", "availability")
colnames(Data) <- colN

Mapdata <- Data[, c("host_name", "listing", "neighbourhood_group", "neighbourhood", "lat", "long", "room_type", "price", "no_of_review")]
saveRDS(Mapdata, file = "mapsg.rds")
  
FullData <- Data[, c("host_id", "host_name", "listing", "neighbourhood_group", "neighbourhood", "room_type", "price", "no_of_review", "Rpmon", "availability")]
colnames(FullData) <- c("Host ID", "Host Name", "Propery Name", "Region", "Neighbourhood", "Room Type", "Price", "Number of Reviews", "Reviews per Month", "Availability")
saveRDS(FullData, file = "fullairbnb.rds")
