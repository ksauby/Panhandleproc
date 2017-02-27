#' Rename Locations
#' 
#' @param dat Dataframe
#' @description Rename Mexico Beach, Nokuse, and Sweetwater in the dataset.
#'
#' @export

renameLocations <- function(dat) {
	if ("Mexico Beach" %in% dat$Location) {
		dat[which(dat$Location=="Mexico Beach"), ]$Location 	<- "MB"
		dat[which(dat$Location=="Nokuse"), ]$Location 			<- "N"	
		dat[which(dat$Location=="Sweetwater"), ]$Location 		<- "TSP"
	}
	return(dat)
}