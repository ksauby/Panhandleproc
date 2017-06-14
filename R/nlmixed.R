#' @title Replace Parameter Names for NLMIXED Output
#' 
#' @param y Parameter estimates output
#' 
#' @export
replaceNLMIXEDnames <- function(y) {
	y[y$Parameter == "A0" | y$Parameter == "B0", ]$Parameter <- "Intercept"	
	y[y$Parameter == "A1" | y$Parameter == "B1", ]$Parameter <- "C_t"
	y[y$Parameter == "A2" | y$Parameter == "B2", ]$Parameter <- "Native Bug"
	y[y$Parameter == "A3" | y$Parameter == "B4", ]$Parameter <- 
		"Mean Max. Temp (Spring/Summer)"
	y[y$Parameter == "A4", ]$Parameter <- "P1 (Spring/Summer)"
	y[y$Parameter == "B3", ]$Parameter <- "T1 (Fall/Winter)"
	y[y$Parameter == "B5", ]$Parameter 	<- "Mean Degree Day (Spring/Summer)"
	y[y$Parameter == "B6", ]$Parameter 	<- "P1 (Fall/Winter)"
	return(y)
}