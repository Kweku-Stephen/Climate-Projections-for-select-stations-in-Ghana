###################################################################################################################
# Creating Ouput folders ####
for(dir in c("Data_Outputs", "Plots_Outputs")) {
	if(dir %in% dir()) {
		message(dir, ":dir exists")
	} else {
		dir.create(dir)
	}
}


# Required Packages ####
Sys.setenv("_R_USE_PIPEBIND_" = "true") # ivoking pipebind operator
#loading required library
sapply(
	c("maggrittr", "ggplot2", "dplyr"),
	character.only = TRUE
)


# Unzipping file
dir(pattern = "sefwi bekua.zip") |> 
	archive::archive_extract()


# importing datasets into R ####
dir(
	path = "sefwi bekua/TMAX/RCP26",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9999", "9988")
	) |>
	setNames(
		dir(
			path = "sefwi bekua/TMAX/RCP26", 
			pattern = ".csv"
		)
	) -> sefwi_bekua_TMAX_RCP26_Corrected_uncorrected


# Data Processing ####
lapply(
	sefwi_bekua_TMAX_RCP26_Corrected_uncorrected,
	#Ananymous function to create a date class and remove the variable "UNCORRECTED"
	\(data = "") {
		data.frame(
			Date = as.Date(
				paste(data[ ,"YEAR"], data[ ,"MONTH"], data[ ,"DAY"], sep = "-"), 
				format = "%Y-%m-%e"
			),
			Tmax = data[ ,"CORRECTED.VALUE"]
		)
	}
) -> sefwi_bekua_TMAX_RCP26_Corrected


# Testing for duplicated Timestamps in "sefwi bekua_RR_RCP26_Corrected" ####
lapply(
	sefwi_bekua_TMAX_RCP26_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	sefwi_bekua_TMAX_RCP26_Corrected,
	\(data = "", var = "", Date = "") {
		
		# Function to test for duplicates using Date object
		chk_dups <- function(Data = "", date = "") any(duplicated(Data[ ,date]))
		
		# Testing for duplicates in the Date object of the datatable
		if(chk_dups(Data = data, date = Date) == TRUE) {
			data[!is.na(data[ ,var]), ]
		} else {
			data
		}
	},
	var = "Tmax",
	Date = "Date"
) -> sefwi_bekua_TMAX_RCP26_Corrected_unduplicated


# Validating removed duplicated dates
sefwi_bekua_TMAX_RCP26_Corrected_unduplicated |> 
	lapply(\(data = "") any(duplicated(data[ ,"Date"]))) # if duplicates still present, we run the code below

# checking duplicated dates
sefwi_bekua_TMAX_RCP26_Corrected_unduplicated |> 
	lapply(\(data = "") data[duplicated(data[ ,"Date"]), ])

# # Removing duplicates in dates (this removes )
# sefwi bekua_TMAX_RCP26_Corrected_unduplicated %<>%
# 	lapply(
# 		\(data = "") data[!duplicated(data[ ,"Date"]), ]
# 	)

# setting all duplicate involved date'S `TMAX` to NA (Include this if Tmax values are non zeros)
sefwi_bekua_TMAX_RCP26_Corrected_unduplicated %<>% 
	lapply(
		#sefwi bekua_TMAX_RCP26_Corrected_unduplicated,
		\(data = "") {
			dups <- data[duplicated((data[ ,"Date"])), "Date"]
			dt <- data[!duplicated(data[ ,"Date"]), ]
			dt[dt[ ,"Date"] %in% dups, 2] <- NA
			dt
		}
	)



# poulating missing years in each dataframe of the list "sefwi bekua_RR_Corrected_unduplicated"
sefwi_bekua_TMAX_RCP26_Corrected_unduplicated %<>%
	lapply(
		#sefwi bekua_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
sefwi_bekua_TMAX_RCP26 <- lapply(
	sefwi_bekua_TMAX_RCP26_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(sefwi_bekua_TMAX_RCP26_Corrected_unduplicated[[1]], data)){
			data[ ,c("Date", "Tmax")]
		} else{
			data[ ,"Tmax"]
		}
	}
) |> . =>
	data.frame(do.call(cbind, .)) |>
	setNames(c("Date", "CM5A", "MIROC5", "NOAA")) |>  . =>
	within(
		.,
		{Ensemble = apply(.[, -1], 1, mean, na.rm = TRUE)}
	)
# sefwi bekua_RR <- data.frame(
# 	Date = sefwi bekua_RR_Corrected_unduplicated[[1]][ ,"Date"],
# 	CM5A = sefwi bekua_RR_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = sefwi bekua_RR_Corrected_unduplicated[[2]][ ,"Rain"],
# 	NOAA = sefwi bekua_RR_Corrected_unduplicated[[3]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = T)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
split(
	sefwi_bekua_TMAX_RCP26[ ,-1],
	format(sefwi_bekua_TMAX_RCP26[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> sefwi_bekua_MeanTMAX_RCP26

# Alternatively
# aggregate(
# 	cbind(CM5A, MIROC5, NOAA, Ensemble) ~ format(sefwi bekua_RR$Date, "%Y"),
# 	data = sefwi bekua_RR,
# 	FUN = sum,
# 	na.rm = TRUE
# )







# RCP 4.5 ####
# importing datasets into R ####
dir(
	path = "sefwi bekua/TMAX/RCP45",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9999", "9988")
	) |>
	setNames(
		dir(
			path = "sefwi bekua/TMAX/RCP45", 
			pattern = ".csv"
		)
	) -> sefwi_bekua_TMAX_RCP45_Corrected_uncorrected


# Data Processing ####
lapply(
	sefwi_bekua_TMAX_RCP45_Corrected_uncorrected,
	#Ananymous function to create a date class and remove the variable "UNCORRECTED"
	\(data = "") {
		data.frame(
			Date = as.Date(
				paste(data[ ,"YEAR"], data[ ,"MONTH"], data[ ,"DAY"], sep = "-"), 
				format = "%Y-%m-%e"
			),
			Tmax = data[ ,"CORRECTED.VALUE"]
		)
	}
) -> sefwi_bekua_TMAX_RCP45_Corrected


# Testing for duplicated Timestamps in "sefwi bekua_RR_Corrected" ####
lapply(
	sefwi_bekua_TMAX_RCP45_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	sefwi_bekua_TMAX_RCP45_Corrected,
	\(data = "", var = "", Date = "") {
		
		# Function to test for duplicates using Date object
		chk_dups <- function(Data = "", date = "") any(duplicated(Data[ ,date]))
		
		# Testing for duplicates in the Date object of the datatable
		if(chk_dups(Data = data, date = Date) == TRUE) {
			data[!is.na(data[ ,var]), ]
		} else {
			data
		}
	},
	var = "Tmax",
	Date = "Date"
) -> sefwi_bekua_TMAX_RCP45_Corrected_unduplicated


# Validating removed duplicated dates
sefwi_bekua_TMAX_RCP45_Corrected_unduplicated |> 
	lapply(\(data = "") any(data[duplicated(data[ ,"Date"]), ]))

# # checking duplicated dates
# sefwi bekua_TMAX_RCP26_Corrected_unduplicated |> 
# 	lapply(\(data = "") data[duplicated(data[ ,"Date"]), ])

# since there are no duplicates, we comment the code  chunk below
# # Removing duplicates in dates
# sefwi bekua_TMAX_RCP45_Corrected_unduplicated %<>%
# 	lapply(
# 		\(data = "") data[!duplicated(data[ ,"Date"]), ]
# 	)

# # setting all duplicate involved date'S `TMAX` to NA (Include this if Tmax values are non zeros)
# sefwi bekua_TMAX_RCP26_Corrected_unduplicated %<>% 
# 	lapply(
# 		#sefwi bekua_TMAX_RCP26_Corrected_unduplicated,
# 		\(data = "") {
# 			dups <- data[duplicated((data[ ,"Date"])), "Date"]
# 			dt <- data[!duplicated(data[ ,"Date"]), ]
# 			dt[dt[ ,"Date"] %in% dups, 2] <- NA
# 			dt
# 		}
# 	)




# poulating missing years in each dataframe of the list "sefwi bekua_RR_Corrected_unduplicated"
sefwi_bekua_TMAX_RCP45_Corrected_unduplicated %<>%
	lapply(
		#sefwi bekua_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
sefwi_bekua_TMAX_RCP45 <- lapply(
	sefwi_bekua_TMAX_RCP45_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(sefwi_bekua_TMAX_RCP45_Corrected_unduplicated[[1]], data)){
			data[ ,c("Date", "Tmax")]
		} else{
			data[ ,"Tmax"]
		}
	}
) |> . =>
	data.frame(do.call(cbind, .)) |>
	setNames(c("Date", "MIROC5", "NOAA", "MPI")) |>  . =>
	within(
		.,
		{Ensemble = apply(.[, -1], 1, mean, na.rm = TRUE)}
	)

# sefwi bekua_RR_RCP45 <- data.frame(
# 	Date = sefwi bekua_RR_RCP45_Corrected_unduplicated[[1]][ ,"Date"],
# 	CNRM = sefwi bekua_RR_RCP45_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = sefwi bekua_RR_RCP45_Corrected_unduplicated[[2]][ ,"Rain"],
# 	MPI = sefwi bekua_RR_RCP45_Corrected_unduplicated[[3]][ ,"Rain"],
# 	NOAA = sefwi bekua_RR_RCP45_Corrected_unduplicated[[4]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = TRUE)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
split(
	sefwi_bekua_TMAX_RCP45[ ,-1],
	format(sefwi_bekua_TMAX_RCP45[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> sefwi_bekua_MeanTMAX_RCP45

# Alternatively
# aggregate(
# 	cbind(CM5A, MIROC5, NOAA, Ensemble) ~ format(sefwi bekua_RR$Date, "%Y"),
# 	data = sefwi bekua_RR,
# 	FUN = sum,
# 	na.rm = TRUE
# )








# RCP85 ####
# importing datasets into R ####
dir(
	path = "sefwi bekua/TMAX/RCP85",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9999", "9988")
	) |>
	setNames(
		dir(
			path = "sefwi bekua/TMAX/RCP85", 
			pattern = ".csv"
		)
	) -> sefwi_bekua_TMAX_RCP85_Corrected_uncorrected


# Data Processing ####
lapply(
	sefwi_bekua_TMAX_RCP85_Corrected_uncorrected,
	#Ananymous function to create a date class and remove the variable "UNCORRECTED"
	\(data = "") {
		data.frame(
			Date = as.Date(
				paste(data[ ,"YEAR"], data[ ,"MONTH"], data[ ,"DAY"], sep = "-"), 
				format = "%Y-%m-%e"
			),
			Tmax = data[ ,"CORRECTED.VALUE"]
		)
	}
) -> sefwi_bekua_TMAX_RCP85_Corrected


# Testing for duplicated Timestamps in "sefwi bekua_RR_Corrected" ####
lapply(
	sefwi_bekua_TMAX_RCP85_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	sefwi_bekua_TMAX_RCP85_Corrected,
	\(data = "", var = "", Date = "") {
		
		# Function to test for duplicates using Date object
		chk_dups <- function(Data = "", date = "") any(duplicated(Data[ ,date]))
		
		# Testing for duplicates in the Date object of the datatable
		if(chk_dups(Data = data, date = Date) == TRUE) {
			data[!is.na(data[ ,var]), ]
		} else {
			data
		}
	},
	var = "Tmax",
	Date = "Date"
) -> sefwi_bekua_TMAX_RCP85_Corrected_unduplicated


# Validating removed duplicated dates
sefwi_bekua_TMAX_RCP85_Corrected_unduplicated |> 
	lapply(\(data = "") any(duplicated(data[ ,"Date"])))

# checking duplicated dates
sefwi_bekua_TMAX_RCP85_Corrected_unduplicated |> 
	lapply(\(data = "") data[duplicated(data[ ,"Date"]), ])

# setting all duplicate involved date'S `TMAX` to NA (Include this if Tmax values are non zeros)
sefwi_bekua_TMAX_RCP85_Corrected_unduplicated %<>% 
	lapply(
		#sefwi bekua_TMAX_RCP26_Corrected_unduplicated,
		\(data = "") {
			dups <- data[duplicated((data[ ,"Date"])), "Date"]
			dt <- data[!duplicated(data[ ,"Date"]), ]
			dt[dt[ ,"Date"] %in% dups, 2] <- NA
			dt
		}
	)





# poulating missing years in each dataframe of the list "sefwi bekua_RR_Corrected_unduplicated"
sefwi_bekua_TMAX_RCP85_Corrected_unduplicated %<>%
	lapply(
		#sefwi bekua_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
sefwi_bekua_TMAX_RCP85 <- lapply(
	sefwi_bekua_TMAX_RCP85_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(sefwi_bekua_TMAX_RCP85_Corrected_unduplicated[[1]], data)){
			data[ ,c("Date", "Tmax")]
		} else{
			data[ ,"Tmax"]
		}
	}
) |> . =>
	data.frame(do.call(cbind, .)) |>
	setNames(c("Date", "CNRM", "CM5A", "MIROC5")) |>  . =>
	within(
		.,
		{Ensemble = apply(.[, -1], 1, mean, na.rm = TRUE)}
	)

# sefwi bekua_RR_RCP85 <- data.frame(
# 	Date = sefwi bekua_RR_RCP85_Corrected_unduplicated[[1]][ ,"Date"],
# 	CNRM = sefwi bekua_RR_RCP85_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = sefwi bekua_RR_RCP85_Corrected_unduplicated[[2]][ ,"Rain"],
# 	MPI = sefwi bekua_RR_RCP85_Corrected_unduplicated[[3]][ ,"Rain"],
# 	NOAA = sefwi bekua_RR_RCP85_Corrected_unduplicated[[4]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = TRUE)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
# The years 1952:1955 have no entries for observations for the models CM5A and MIROC5 except CNRM
split(
	sefwi_bekua_TMAX_RCP85[ ,-1],
	format(sefwi_bekua_TMAX_RCP85[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> sefwi_bekua_MeanTMAX_RCP85

# # Alternatively
# aggregate(
# 	cbind(CNRM, CM5A, MIROC5, Ensemble) ~ format(sefwi bekua_RR_RCP85$Date, "%Y"),
# 	data = sefwi bekua_RR_RCP85,
# 	FUN = sum,
# 	na.rm = TRUE
# ) |> head()






# Building all Ensembles into a dataframe ####
`Ensembles of scenarios TMAX sefwi bekua` <- data.frame(
	Date = as.numeric(rownames(sefwi_bekua_MeanTMAX_RCP26)),
	RCP26 = sefwi_bekua_MeanTMAX_RCP26[ ,"Ensemble"],
	RCP45 = sefwi_bekua_MeanTMAX_RCP45[ ,"Ensemble"],
	RCP85 = sefwi_bekua_MeanTMAX_RCP85[ ,"Ensemble"]
) |> 
	subset(Date >= 1980)

# Observed, historicals of scenarios plus eNsemble of historical of scenarios ####
`observed + Hist_scenarios TMAX sefwi bekua` = data.frame(
	observed = read.csv(
		"C:\\Users\\pc\\OneDrive\\Documents\\Climate_of_Select_Stations\\Mean MaxTmp\\Sefwi_BEKWAI_tmax.csv",
		col.names = c("Year", "observed")
	),
	RCP26 = subset(`Ensembles of scenarios TMAX sefwi bekua`, Date <= 2020)[ ,"RCP26"],
	RCP45 = subset(`Ensembles of scenarios TMAX sefwi bekua`, Date <= 2020)[ ,"RCP45"],
	RCP85 = subset(`Ensembles of scenarios TMAX sefwi bekua`, Date <= 2020)[ ,"RCP85"]
) |> . =>
	within(
		.,
		{
			`Ensemble of hist scenarios` = apply(
				.[ ,c("RCP26","RCP45", "RCP85")],
				1,
				mean, 
				na.rm = TRUE
			)
		}
	) |>
	setNames(
		c(
			"Year", 
			"observed", 
			"RCP26", 
			"RCP45", 
			"RCP85", 
			"Ensemble of hist scenarios"
		)
	)
