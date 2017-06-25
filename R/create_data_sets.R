#' Create Dataset with All Surveys
#' 
#' @description Create dataset from all surveys and calculate:
#' \itemize{
#'  \item lagged fruit values
#'  \item lagged dates
#'  \item lagged insect presence
#'  \item relative growth rate
#' }
#' @param timeseries Dataset
#' 
#' @export
#' @importFrom GTMNERRproc createNewInsectVariables

createAllSurveysDataset <- function(timeseries) {
	timeseries_all_surveys <- timeseries
	timeseries_all_surveys %<>% 
		# lag variables
		calculateSizeLags(
			arrange.variable="Date", 
			grouping.variable="PlantID"
		) %>%
		calculateDateLags %>%
		calculateInsectLags(
			arrange.variable="Date", 
			grouping.variable="PlantID"
		) %>%
		# RGR
		calculateRGR
		# convert to factor
		timeseries_all_surveys[,c(
			"Location",
			"Species",
			"Coastal",
			"Season",
			"PlantID",
			"PlantID2")] %<>%
			apply(., 2, as.factor
		)
		# convert to numeric
		timeseries_all_surveys[,c(
			"Cylinder_Tall_t",
			"Cone_t",
			"Height_t",            
			"Width_t",               
			"Height_t_1",
			"Cone_t_1",               
			"Cylinder_Tall_t_1",
			"RGR_Height",
			"RGR_Height365",
			"RGR_Size",
			"RGR_Size365",
			"RGR_Cone",
			"RGR_Cone365",
			"RGR_Cylinder_Tall",
			"RGR_CylinderTall365")] %<>%
			apply(., 2, as.numeric
		)
		# convert to integer
		timeseries_all_surveys[,c(
			"DA_t",
			"CH_t",
			"ME_t",
			"CA_t",
			"Size_t",
			"Fruit_t",  
			"FruitPres_t",
			"CACAPresent",
			"MEPRPresent",
			"DACTPresent",          
			"CHVIPresent",
			"NumInsectSpecies_t",
			"Size_t_1",
			"CA_t_1",
			"ME_t_1",
			"CH_t_1",                 
			"DA_t_1",
			"Dead",
			"DaysSincePrevSurvey")] %<>%
			apply(., 2, as.integer
		)
   		# convert to Date
		timeseries_all_surveys$Date %<>% as.Date(format = "%Y-%m-%d")	
		timeseries_all_surveys$Previous_Survey_Date %<>% 
			as.Date(format = "%Y-%m-%d")	
	# Save
	setwd("/Users/KSauby/Documents/Projects/marsico-time-series/")
	cache("timeseries_all_surveys")
	return(timeseries_all_surveys)
}	

#' Create Dataset with Yearly Fruit Surveys
#' 
#' @param timeseries Dataset
#' @description Create a dataset with yearly observations of fruit, size, and insect observations. Each starts on the first day of spring of that calendar year, then ends on the last day of winter in the next calendar year (e.g., Spring 2009 - Winter 2010). Variables that are calculated included the maximum number of fruit observed, insect presence/absence during the year, and the maximum and minimum plant size that year. All years, except for 2009, have two observations per year.
#' The dataset is created according to the following steps:
#' \itemize{
#'  \item create a year variable (called "FecundityYear")
#'  \item determine if insect was ever observed during the FecundityYear
#'  \item calculate maximum and minimum plant size and volume
#'  \item determine if surveys were complete for the entire year (no missing information)
#' }
#' 
#' @export

createFruitYearDataset <- function(timeseries) {
	timeseries_fruityear <- timeseries
	timeseries_fruityear %<>% 
		as.data.frame %>%
		createFecundityYear %>%
		filter(!(is.na(FecundityYear))) %>%
		arrange(FecundityYear) %>%
		group_by(PlantID) %>%
		mutate(PrevFecundityYear = FecundityYear - 1) %>%
		ungroup %>%
		group_by(PlantID, FecundityYear) %>%
		summarise(
			PrevFecundityYear				= PrevFecundityYear[1],
			Location 				= Location[1],
			Species					= Species[1],
			ID						= ID[1],
			Fruit_t 				= max(Fruit_t),
			FruitPres_t 			= max(FruitPres_t),
			ME_t 					= max(ME_t),
			CA_t 					= max(CA_t),
			CH_t 					= max(CH_t),
			DA_t 					= max(DA_t),
			Size_max_t 				= max(Size_t),
			Cone_max_t 				= max(Cone_t), 
			Cylinder_Tall_max_t 	= max(Cylinder_Tall_t),
			Size_min_t 				= min(Size_t),
			Cone_min_t 				= min(Cone_t), 
			Cylinder_Tall_min_t 	= min(Cylinder_Tall_t),
			complete_insect_surveys = min(complete_insect_surveys), 
			complete_surveys		= min(complete_surveys)
		) %>%
		# lag variables
		calculateLagGroupedDF(
			arrange.variable="FecundityYear", 
			grouping.variable="PlantID",
			vars="Size_max_t"
		) %>%
		calculateFruitLags(
			arrange.variable="FecundityYear", 
			grouping.variable="PlantID"
		) %>%
		calculateInsectLags(
			arrange.variable="FecundityYear", 
			grouping.variable="PlantID"
		)
}