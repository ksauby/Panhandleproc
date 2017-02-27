#' Find closest weather stations to each sampling location
#' 
#' @description For each location, compile weather data from the closest weather stations.
#' @param sites List of sampling locations with x, y coordinates.
#' @param climate_data Climate dataset
#' @param Distance Radius (km) within which to look for climate stations for a particular location. Defaults to 85 kilometers.
#'
#' @export
#' @importFrom sp coordinates spTransform CRS
#' @importFrom reshape2 melt

findClosestWeatherStations <- function(sites, climate_data, Distance=85) {
	# merge sampling locations and weather station locations to calculate distance matrix (all pairwise distances among points)
	A <- sites %>%
		select(Location.name, Latitude, Longitude) %>%
		rbind.fill(select(wstations, Name, Latitude, Longitude))
	# first convert sampling locations and weather station coordinates to UTM
	coordinates(A) <- c("Longitude", "Latitude")
	proj4string(A) <- CRS("+proj=longlat +datum=WGS84")  ## for example
	# then calculate dist matrix (which will now be in m)
	A %<>% spTransform(CRS("+proj=utm +zone=16 ellps=WGS84"))
	# convert coordinates back to numeric
	A <- as.data.frame(cbind(A$Name, A@coords))
	names(A) <- c("Name", "Easting", "Northing")
	A[,c("Easting", "Northing")] %<>% apply(., 2, as.numeric)
	# convert coordinate units to km
	A[,c("Easting", "Northing")] %<>% apply(., 2, function(x) {x/1000})
	# calculate distance matrix
	distance_matrix <- as.data.frame(
		as.matrix(
			dist(
				cbind(A$Easting, A$Northing), 
				method="euclidian"
			)
		)
	)
	# modify distance matrix
	# 		keep columns 1:6 (correspond to the 6 sampling locations)
	# 		keep rows 7:170 (correspond to the weather stations)
	distance_matrix <- distance_matrix[7:206, 1:6]
	names(distance_matrix) <- Location_list
	# merge distance matrix with weather station info
	B <- select(
		wstations, 
		Name, 
		Station.ID, 
		Latitude, 
		Longitude, 
		Precipitation, 
		Temperature, 
		Used_in_Analysis, 
		Sampling_Site, 
		Near_Sampling_Site, 
		Date_Range
	)
	distance_matrix %<>% cbind(B)
	# change distance matrix columns to one column
	weather_station_info <- melt(
		distance_matrix, 
		id.vars=c(
			"Name", 
			"Station.ID", 
			"Latitude", 
			"Longitude", 
			"Precipitation", 
			"Temperature", 
			"Used_in_Analysis", 
			"Sampling_Site", 
			"Near_Sampling_Site", 
			"Date_Range"
		)
	)
	names(weather_station_info)[(dim(weather_station_info)[2]-1) : 
		dim(weather_station_info)[2]] <- c("Sampling_Location", "Distance")
	# standardize Station IDs - remove "GHCND:" if the the ID has it
	weather_station_info$Station.ID <- gsub(
		"^.*\\:", "", 
		weather_station_info$Station.ID
	)
	climate_data$STATION <- gsub("^.*\\:", "", climate_data$STATION)
	# SELECT CLOSEST WEATHER STATIONS FOR EACH SAMPLING LOCATION
	# merge distance data with climate_data
	climate_data_temp <- weather_station_info %>% 
		select(Station.ID, Sampling_Location, Distance) %>%
		merge(climate_data, by.x="Station.ID", by.y="STATION")
	BLSP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="BLSP" & Distance <= Distance)
	HBSP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="HBSP" & Distance <= Distance) %>% 
		arrange(Date)
	MB_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="MB" & Distance <= Distance)
	NP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="NP" & Distance <= Distance)
	SASP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="SASP" & Distance <= Distance)
	TSP_stations <- climate_data_temp %>% 
		filter(Sampling_Location=="TSP" & Distance <= Distance)
	# create list of climate stations per sampling location
	Dat = list(
		`climate_data`	= climate_data_temp,
		`BLSP_stations` = BLSP_stations,
		`HBSP_stations` = HBSP_stations,
		`MB_stations` 	= MB_stations,
		`NP_stations` 	= NP_stations,
		`SASP_stations` = SASP_stations,
		`TSP_stations` 	= TSP_stations
	)
	return(Dat)
}