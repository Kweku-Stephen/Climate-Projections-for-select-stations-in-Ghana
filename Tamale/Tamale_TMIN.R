###################################################################################################################
# Creating Ouput folders ####
for(dir in c("Data_Outputs", "Plots_Outputs")) {
	if(dir %in% dir()) {
		message("dir exists")
	} else {
		dir.create(dir)
	}
}


# Required Packages ####
Sys.setenv("_R_USE_PIPEBIND_" = "true") # Invoking pipebind functionality
#loading required packages
sapply(
	c("magrittr", "ggplot2", "dplyr"),
	require,
	character.only = TRUE
)


# Unzipping file
dir(pattern = "Tamale.zip") |> 
	archive::archive_extract()


# importing datasets into R ####
dir(
	path = "Tamale/TMIN/RCP26",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9999", "9988")
	) |>
	setNames(
		dir(
			path = "Tamale/TMIN/RCP26", 
			pattern = ".csv"
		)
	) -> Tamale_TMIN_RCP26_Corrected_uncorrected


# Data Processing ####
lapply(
	Tamale_TMIN_RCP26_Corrected_uncorrected,
	#Ananymous function to create a date class and remove the variable "UNCORRECTED"
	\(data = "") {
		data.frame(
			Date = as.Date(
				paste(data[ ,"YEAR"], data[ ,"MONTH"], data[ ,"DAY"], sep = "-"), 
				format = "%Y-%m-%e"
			),
			Tmin = data[ ,"CORRECTED.VALUE"]
		)
	}
) -> Tamale_TMIN_RCP26_Corrected


# Testing for duplicated Timestamps in "Tamale_RR_RCP26_Corrected" ####
lapply(
	Tamale_TMIN_RCP26_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	Tamale_TMIN_RCP26_Corrected,
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
	var = "Tmin",
	Date = "Date"
) -> Tamale_TMIN_RCP26_Corrected_unduplicated


# Validating removed duplicated dates
Tamale_TMIN_RCP26_Corrected_unduplicated |> 
	lapply(\(data = "") any(duplicated(data[ ,"Date"]))) # if duplicates still present, we run the code below

# checking duplicated dates
Tamale_TMIN_RCP26_Corrected_unduplicated |> 
	lapply(\(data = "") data[duplicated(data[ ,"Date"]), ])

# # Removing duplicates in dates (this removes )
# Tamale_TMAX_RCP26_Corrected_unduplicated %<>%
# 	lapply(
# 		\(data = "") data[!duplicated(data[ ,"Date"]), ]
# 	)

# setting all duplicate involved date'S `TMAX` to NA (Include this if Tmax values are non zeros)
Tamale_TMIN_RCP26_Corrected_unduplicated %<>% 
	lapply(
		#Tamale_TMAX_RCP26_Corrected_unduplicated,
		\(data = "") {
			dups <- data[duplicated((data[ ,"Date"])), "Date"]
			dt <- data[!duplicated(data[ ,"Date"]), ]
			dt[dt[ ,"Date"] %in% dups, 2] <- NA
			dt
		}
	)



# poulating missing years in each dataframe of the list "Tamale_RR_Corrected_unduplicated"
Tamale_TMIN_RCP26_Corrected_unduplicated %<>%
	lapply(
		#Tamale_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
Tamale_TMIN_RCP26 <- lapply(
	Tamale_TMIN_RCP26_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(Tamale_TMIN_RCP26_Corrected_unduplicated[[1]], data)){
			data[ ,c("Date", "Tmin")]
		} else{
			data[ ,"Tmin"]
		}
	}
) |> . =>
	data.frame(do.call(cbind, .)) |>
	setNames(c("Date","CM5A", "MIROC5", "NOAA")) |>  . =>
	within(
		.,
		{Ensemble = apply(.[, -1], 1, mean, na.rm = TRUE)}
	)
# Tamale_RR <- data.frame(
# 	Date = Tamale_RR_Corrected_unduplicated[[1]][ ,"Date"],
# 	CM5A = Tamale_RR_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = Tamale_RR_Corrected_unduplicated[[2]][ ,"Rain"],
# 	NOAA = Tamale_RR_Corrected_unduplicated[[3]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = T)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
split(
	Tamale_TMIN_RCP26[ ,-1],
	format(Tamale_TMIN_RCP26[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> Tamale_MeanTMIN_RCP26

# Alternatively
# aggregate(
# 	cbind(CM5A, MIROC5, NOAA, Ensemble) ~ format(Tamale_RR$Date, "%Y"),
# 	data = Tamale_RR,
# 	FUN = sum,
# 	na.rm = TRUE
# )







# RCP 4.5 ####
# importing datasets into R ####
dir(
	path = "Tamale/TMIN/RCP45",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9999", "9988")
	) |>
	setNames(
		dir(
			path = "Tamale/TMIN/RCP45", 
			pattern = ".csv"
		)
	) -> Tamale_TMIN_RCP45_Corrected_uncorrected


# Data Processing ####
lapply(
	Tamale_TMIN_RCP45_Corrected_uncorrected,
	#Ananymous function to create a date class and remove the variable "UNCORRECTED"
	\(data = "") {
		data.frame(
			Date = as.Date(
				paste(data[ ,"YEAR"], data[ ,"MONTH"], data[ ,"DAY"], sep = "-"), 
				format = "%Y-%m-%e"
			),
			Tmin = data[ ,"CORRECTED.VALUE"]
		)
	}
) -> Tamale_TMIN_RCP45_Corrected


# Testing for duplicated Timestamps in "Tamale_RR_Corrected" ####
lapply(
	Tamale_TMIN_RCP45_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	Tamale_TMIN_RCP45_Corrected,
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
	var = "Tmin",
	Date = "Date"
) -> Tamale_TMIN_RCP45_Corrected_unduplicated


# Validating removed duplicated dates
Tamale_TMIN_RCP45_Corrected_unduplicated |> 
	lapply(\(data = "") any(data[duplicated(data[ ,"Date"]), ]))

# since there are no duplicates, we comment the code  chunk below
# # Removing duplicates in dates
# Tamale_TMAX_RCP45_Corrected_unduplicated %<>%
# 	lapply(
# 		\(data = "") data[!duplicated(data[ ,"Date"]), ]
# 	)

# poulating missing years in each dataframe of the list "Tamale_RR_Corrected_unduplicated"
Tamale_TMIN_RCP45_Corrected_unduplicated %<>%
	lapply(
		#Tamale_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
Tamale_TMIN_RCP45 <- lapply(
	Tamale_TMIN_RCP45_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(Tamale_TMIN_RCP45_Corrected_unduplicated[[1]], data)){
			data[ ,c("Date", "Tmin")]
		} else{
			data[ ,"Tmin"]
		}
	}
) |> . =>
	data.frame(do.call(cbind, .)) |>
	setNames(c("Date", "CNRM", "MIROC5", "MPI", "NOAA")) |>  . =>
	within(
		.,
		{Ensemble = apply(.[, -1], 1, mean, na.rm = TRUE)}
	)

# Tamale_RR_RCP45 <- data.frame(
# 	Date = Tamale_RR_RCP45_Corrected_unduplicated[[1]][ ,"Date"],
# 	CNRM = Tamale_RR_RCP45_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = Tamale_RR_RCP45_Corrected_unduplicated[[2]][ ,"Rain"],
# 	MPI = Tamale_RR_RCP45_Corrected_unduplicated[[3]][ ,"Rain"],
# 	NOAA = Tamale_RR_RCP45_Corrected_unduplicated[[4]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = TRUE)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
split(
	Tamale_TMIN_RCP45[ ,grep("[^Date]", names(Tamale_TMIN_RCP45), value = TRUE)],
	format(Tamale_TMIN_RCP45[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> Tamale_MeanTMIN_RCP45

# Alternatively
# aggregate(
# 	cbind(CM5A, MIROC5, NOAA, Ensemble) ~ format(Tamale_RR$Date, "%Y"),
# 	data = Tamale_RR,
# 	FUN = sum,
# 	na.rm = TRUE
# )








# RCP85 ####
# importing datasets into R ####
dir(
	path = "Tamale/TMIN/RCP85",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9988", "9999")
	) |>
	setNames(
		dir(
			path = "Tamale/TMIN/RCP85", 
			pattern = ".csv"
		)
	) -> Tamale_TMIN_RCP85_Corrected_uncorrected


# Data Processing ####
lapply(
	Tamale_TMIN_RCP85_Corrected_uncorrected,
	#Ananymous function to create a date class and remove the variable "UNCORRECTED"
	\(data = "") {
		data.frame(
			Date = as.Date(
				paste(data[ ,"YEAR"], data[ ,"MONTH"], data[ ,"DAY"], sep = "-"), 
				format = "%Y-%m-%e"
			),
			Tmin = data[ ,"CORRECTED.VALUE"]
		)
	}
) -> Tamale_TMIN_RCP85_Corrected


# Testing for duplicated Timestamps in "Tamale_RR_Corrected" ####
lapply(
	Tamale_TMIN_RCP85_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	Tamale_TMIN_RCP85_Corrected,
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
	var = "Tmin",
	Date = "Date"
) -> Tamale_TMIN_RCP85_Corrected_unduplicated


# Validating removed duplicated dates
Tamale_TMIN_RCP85_Corrected_unduplicated |> 
	lapply(\(data = "") any(duplicated(data[ ,"Date"])))

# checking duplicated dates
Tamale_TMIN_RCP85_Corrected_unduplicated |> 
	lapply(\(data = "") data[duplicated(data[ ,"Date"]), ])

# setting all duplicate involved date'S `TMAX` to NA (Include this if Tmax values are non zeros)
Tamale_TMIN_RCP85_Corrected_unduplicated %<>% 
	lapply(
		#Tamale_TMAX_RCP26_Corrected_unduplicated,
		\(data = "") {
			dups <- data[duplicated((data[ ,"Date"])), "Date"]
			dt <- data[!duplicated(data[ ,"Date"]), ]
			dt[dt[ ,"Date"] %in% dups, 2] <- NA
			dt
		}
	)





# poulating missing years in each dataframe of the list "Tamale_RR_Corrected_unduplicated"
Tamale_TMIN_RCP85_Corrected_unduplicated %<>%
	lapply(
		#Tamale_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
Tamale_TMIN_RCP85 <- lapply(
	Tamale_TMIN_RCP85_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(Tamale_TMIN_RCP85_Corrected_unduplicated[[1]], data)){
			data[ ,c("Date", "Tmin")]
		} else{
			data[ ,"Tmin"]
		}
	}
) |> . =>
	data.frame(do.call(cbind, .)) |>
	setNames(c("Date", "CNRM", "CM5A", "MIROC5")) |>  . =>
	within(
		.,
		{Ensemble = apply(.[, -1], 1, mean, na.rm = TRUE)}
	)

# Tamale_RR_RCP85 <- data.frame(
# 	Date = Tamale_RR_RCP85_Corrected_unduplicated[[1]][ ,"Date"],
# 	CNRM = Tamale_RR_RCP85_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = Tamale_RR_RCP85_Corrected_unduplicated[[2]][ ,"Rain"],
# 	MPI = Tamale_RR_RCP85_Corrected_unduplicated[[3]][ ,"Rain"],
# 	NOAA = Tamale_RR_RCP85_Corrected_unduplicated[[4]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = TRUE)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
# The years 1952:1955 have no entries for observations for the models CM5A and MIROC5 except CNRM
split(
	Tamale_TMIN_RCP85[ ,grep("[^Date]", names(Tamale_TMIN_RCP85), value = TRUE)],
	format(Tamale_TMIN_RCP85[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> Tamale_MeanTMIN_RCP85

# # Alternatively
# aggregate(
# 	cbind(CNRM, CM5A, MIROC5, Ensemble) ~ format(Tamale_RR_RCP85$Date, "%Y"),
# 	data = Tamale_RR_RCP85,
# 	FUN = sum,
# 	na.rm = TRUE
# ) |> head()






# Building all Ensembles into a dataframe ####
`Ensembles of scenarios TMIN Tamale` <- data.frame(
	Date = as.numeric(rownames(Tamale_MeanTMIN_RCP26)),
	RCP26 = Tamale_MeanTMIN_RCP26[ ,"Ensemble"],
	RCP45 = Tamale_MeanTMIN_RCP45[ ,"Ensemble"],
	RCP85 = Tamale_MeanTMIN_RCP85[ ,"Ensemble"]
) |> 
	subset(Date >= 1980)

# Observed, historicals of scenarios plus eNsemble of historical of scenarios ####
`observed + Hist_scenarios TMIN Tamale` = data.frame(
	observed = read.csv(
		"C:/Users/pc/OneDrive/Documents/R_PROJECTS/Tamale/Annual Minimum Temperature of Tamale (1960-2016).csv",
		col.names = c("Year", "observed"),
		skip = 20
	) |> . =>
		transform(., Year = as.Date(.[ ,"Year"], "%m/%d/%Y")),
	RCP26 = subset(`Ensembles of scenarios TMIN Tamale`, Date <= 2016)[ ,"RCP26"],
	RCP45 = subset(`Ensembles of scenarios TMIN Tamale`, Date <= 2016)[ ,"RCP45"],
	RCP85 = subset(`Ensembles of scenarios TMIN Tamale`, Date <= 2016)[ ,"RCP85"]
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
	setNames(c("Year", "observed", "RCP26", "RCP45", "RCP85", "Ensemble of hist scenarios")) |> . =>
	transform(.,Year = as.numeric(unlist(format(.["Year"], "%Y"))))




# Visualization
gridExtra::grid.arrange(
	ggplot(data = `Ensembles of scenarios Tamale`,aes(x = Date)) +
		geom_line(data = `observed + Hist_scenarios Tamale`, aes(x = Year, y = RCP26, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios Tamale`, aes(x = Year, y = RCP45, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios Tamale`, aes(x = Year, y = RCP85, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios Tamale`, aes(x = Year, y = Ensemble.of.hist.scenarios, col = "Ensemble"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios Tamale`, aes(x = Year, y = observed, col = "Observed"), lwd = 2)+
		geom_line(data = subset(`Ensembles of scenarios Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP26, col = "RCP26"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP45, col = "RCP45"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP85, col = "RCP85"), lwd = 1) +
		scale_colour_manual("", breaks = c("Observed","Ensemble","Historical", "RCP26", "RCP45", "RCP85"),
						values = c("darkblue","black", "grey85", "red", "darkgreen", "brown")) +
		scale_x_continuous(breaks = seq(1980,2100 ,by = 35), limits = c(1980, 2100)) +
		labs(subtitle = "Tamale Rainfall", y = "Rainfall(mm)", x = "Year") +
		theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
			 axis.text = element_text(size = 30),
			 axis.title = element_text(size = 28),
			 legend.text = element_text(size = 26)),
	
	ggplot(data = `Ensembles of scenarios TMAX Tamale`,aes(x = Date)) +
		geom_line(data = `observed + Hist_scenarios TMAX Tamale`, aes(x = Year, y = RCP26, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMAX Tamale`, aes(x = Year, y = RCP45, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMAX Tamale`, aes(x = Year, y = RCP85, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMAX Tamale`, aes(x = Year, y = Ensemble.of.hist.scenarios, col = "Ensemble"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMAX Tamale`, aes(x = Year, y = observed, col = "Observed"), lwd = 2)+
		geom_line(data = subset(`Ensembles of scenarios TMAX Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP26, col = "RCP26"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios TMAX Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP45, col = "RCP45"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios TMAX Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP85, col = "RCP85"), lwd = 1) +
		scale_colour_manual("", breaks = c("Observed","Ensemble","Historical", "RCP26", "RCP45", "RCP85"),
						values = c("darkblue","black", "grey85", "red", "darkgreen", "brown")) +
		scale_x_continuous(breaks = seq(1980,2100 ,by = 35), limits = c(1980, 2100)) +
		labs(subtitle = "Tamale Maximum Temperature", y = expression("Temperature("~degree*C*")"), x = "Year") +
		theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
			 axis.text = element_text(size = 30),
			 axis.title = element_text(size = 28),
			 legend.text = element_text(size = 26)),
	
	ggplot(data = `Ensembles of scenarios TMIN Tamale`,aes(x = Date)) +
		geom_line(data = `observed + Hist_scenarios TMIN Tamale`, aes(x = Year, y = RCP26, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMIN Tamale`, aes(x = Year, y = RCP45, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMIN Tamale`, aes(x = Year, y = RCP85, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMIN Tamale`, aes(x = Year, y = Ensemble.of.hist.scenarios, col = "Ensemble"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMIN Tamale`, aes(x = Year, y = observed, col = "Observed"), lwd = 2)+
		geom_line(data = subset(`Ensembles of scenarios TMIN Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP26, col = "RCP26"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios TMIN Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP45, col = "RCP45"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios TMIN Tamale`, Date >= 2017), 
				aes(x = Date, y = RCP85, col = "RCP85"), lwd = 1) +
		scale_colour_manual("", breaks = c("Observed","Ensemble","Historical", "RCP26", "RCP45", "RCP85"),
						values = c("darkblue","black", "grey85", "red", "darkgreen", "brown")) +
		scale_x_continuous(breaks = seq(1980,2100 ,by = 35), limits = c(1980, 2100)) +
		labs(subtitle = "Tamale Minimum Temperature", y = expression("Temperature("~degree*C*")"), x = "Year") +
		theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
			 axis.text = element_text(size = 30),
			 axis.title = element_text(size = 28),
			 legend.text = element_text(size = 26)),
	
	nrow = 2, ncol = 2
)

dev.copy(png, filename = "Plots_Outputs/Projected Climate of Tamale.png", width = 1500, height = 900)
dev.off()



