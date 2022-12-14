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
Sys.setenv("_R_USE_PIPEBIND_" = "true")


# Unzipping file
dir(pattern = "Navrongo.zip") |> 
	archive::archive_extract()


# importing datasets into R ####
dir(
	path = "Navrongo/TMIN/RCP26",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9999", "9988")
	) |>
	setNames(
		dir(
			path = "Navrongo/TMIN/RCP26", 
			pattern = ".csv"
		)
	) -> Navrongo_TMIN_RCP26_Corrected_uncorrected


# Data Processing ####
lapply(
	Navrongo_TMIN_RCP26_Corrected_uncorrected,
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
) -> Navrongo_TMIN_RCP26_Corrected


# Testing for duplicated Timestamps in "Navrongo_RR_RCP26_Corrected" ####
lapply(
	Navrongo_TMIN_RCP26_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	Navrongo_TMIN_RCP26_Corrected,
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
) -> Navrongo_TMIN_RCP26_Corrected_unduplicated


# Validating removed duplicated dates
Navrongo_TMIN_RCP26_Corrected_unduplicated |> 
	lapply(\(data = "") any(duplicated(data[ ,"Date"]))) # if duplicates still present, we run the code below

# checking duplicated dates
Navrongo_TMIN_RCP26_Corrected_unduplicated |> 
	lapply(\(data = "") data[duplicated(data[ ,"Date"]), ])

# # Removing duplicates in dates (this removes )
# Navrongo_TMAX_RCP26_Corrected_unduplicated %<>%
# 	lapply(
# 		\(data = "") data[!duplicated(data[ ,"Date"]), ]
# 	)

# setting all duplicate involved date'S `TMAX` to NA (Include this if Tmax values are non zeros)
Navrongo_TMIN_RCP26_Corrected_unduplicated %<>% 
	lapply(
		#Navrongo_TMAX_RCP26_Corrected_unduplicated,
		\(data = "") {
			dups <- data[duplicated((data[ ,"Date"])), "Date"]
			dt <- data[!duplicated(data[ ,"Date"]), ]
			dt[dt[ ,"Date"] %in% dups, 2] <- NA
			dt
		}
	)



# poulating missing years in each dataframe of the list "Navrongo_RR_Corrected_unduplicated"
Navrongo_TMIN_RCP26_Corrected_unduplicated %<>%
	lapply(
		#Navrongo_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
Navrongo_TMIN_RCP26 <- lapply(
	Navrongo_TMIN_RCP26_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(Navrongo_TMIN_RCP26_Corrected_unduplicated[[1]], data)){
			data[ ,c("Date", "Tmin")]
		} else{
			data[ ,"Tmin"]
		}
	}
) |> . =>
	data.frame(do.call(cbind, .)) |>
	setNames(c("Date","MIROC5","CM5A", "NOAA")) |>  . =>
	within(
		.,
		{Ensemble = apply(.[, -1], 1, mean, na.rm = TRUE)}
	)
# Navrongo_RR <- data.frame(
# 	Date = Navrongo_RR_Corrected_unduplicated[[1]][ ,"Date"],
# 	CM5A = Navrongo_RR_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = Navrongo_RR_Corrected_unduplicated[[2]][ ,"Rain"],
# 	NOAA = Navrongo_RR_Corrected_unduplicated[[3]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = T)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
split(
	Navrongo_TMIN_RCP26[ ,-1],
	format(Navrongo_TMIN_RCP26[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> Navrongo_MeanTMIN_RCP26

# Alternatively
# aggregate(
# 	cbind(CM5A, MIROC5, NOAA, Ensemble) ~ format(Navrongo_RR$Date, "%Y"),
# 	data = Navrongo_RR,
# 	FUN = sum,
# 	na.rm = TRUE
# )







# RCP 4.5 ####
# importing datasets into R ####
dir(
	path = "Navrongo/TMIN/RCP45",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9999", "9988")
	) |>
	setNames(
		dir(
			path = "Navrongo/TMIN/RCP45", 
			pattern = ".csv"
		)
	) -> Navrongo_TMIN_RCP45_Corrected_uncorrected


# Data Processing ####
lapply(
	Navrongo_TMIN_RCP45_Corrected_uncorrected,
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
) -> Navrongo_TMIN_RCP45_Corrected


# Testing for duplicated Timestamps in "Navrongo_RR_Corrected" ####
lapply(
	Navrongo_TMIN_RCP45_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	Navrongo_TMIN_RCP45_Corrected,
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
) -> Navrongo_TMIN_RCP45_Corrected_unduplicated


# Validating removed duplicated dates
Navrongo_TMIN_RCP45_Corrected_unduplicated |> 
	lapply(\(data = "") any(data[duplicated(data[ ,"Date"]), ]))

# since there are no duplicates, we comment the code  chunk below
# # Removing duplicates in dates
# Navrongo_TMAX_RCP45_Corrected_unduplicated %<>%
# 	lapply(
# 		\(data = "") data[!duplicated(data[ ,"Date"]), ]
# 	)

# poulating missing years in each dataframe of the list "Navrongo_RR_Corrected_unduplicated"
Navrongo_TMIN_RCP45_Corrected_unduplicated %<>%
	lapply(
		#Navrongo_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
Navrongo_TMIN_RCP45 <- lapply(
	Navrongo_TMIN_RCP45_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(Navrongo_TMIN_RCP45_Corrected_unduplicated[[1]], data)){
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

# Navrongo_RR_RCP45 <- data.frame(
# 	Date = Navrongo_RR_RCP45_Corrected_unduplicated[[1]][ ,"Date"],
# 	CNRM = Navrongo_RR_RCP45_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = Navrongo_RR_RCP45_Corrected_unduplicated[[2]][ ,"Rain"],
# 	MPI = Navrongo_RR_RCP45_Corrected_unduplicated[[3]][ ,"Rain"],
# 	NOAA = Navrongo_RR_RCP45_Corrected_unduplicated[[4]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = TRUE)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
split(
	Navrongo_TMIN_RCP45[ ,-1],
	format(Navrongo_TMIN_RCP45[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> Navrongo_MeanTMIN_RCP45

# Alternatively
# aggregate(
# 	cbind(CM5A, MIROC5, NOAA, Ensemble) ~ format(Navrongo_RR$Date, "%Y"),
# 	data = Navrongo_RR,
# 	FUN = sum,
# 	na.rm = TRUE
# )








# RCP85 ####
# importing datasets into R ####
dir(
	path = "Navrongo/TMIN/RCP85",
	pattern = ".csv",
	full.names = TRUE
) |> 
	lapply(
		read.csv,
		na.strings = c("-99.9", "-9999", "-9988", "-99", "9988", "9999")
	) |>
	setNames(
		dir(
			path = "Navrongo/TMIN/RCP85", 
			pattern = ".csv"
		)
	) -> Navrongo_TMIN_RCP85_Corrected_uncorrected


# Data Processing ####
lapply(
	Navrongo_TMIN_RCP85_Corrected_uncorrected,
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
) -> Navrongo_TMIN_RCP85_Corrected


# Testing for duplicated Timestamps in "Navrongo_RR_Corrected" ####
lapply(
	Navrongo_TMIN_RCP85_Corrected,
	\(data = "") any(duplicated(data[ ,"Date"]))
)

# Removing Duplicated Timestamps ####
# `duplicated()` returns the seccond to nth (if applicable) duplicate and assumes the first is the original entry
# However, the first entry in most cases is the duplicate (in this context)
lapply(
	Navrongo_TMIN_RCP85_Corrected,
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
) -> Navrongo_TMIN_RCP85_Corrected_unduplicated


# Validating removed duplicated dates
Navrongo_TMIN_RCP85_Corrected_unduplicated |> 
	lapply(\(data = "") any(duplicated(data[ ,"Date"])))

# checking duplicated dates
Navrongo_TMIN_RCP85_Corrected_unduplicated |> 
	lapply(\(data = "") data[duplicated(data[ ,"Date"]), ])

# setting all duplicate involved date'S `TMAX` to NA (Include this if Tmax values are non zeros)
Navrongo_TMIN_RCP85_Corrected_unduplicated %<>% 
	lapply(
		#Navrongo_TMAX_RCP26_Corrected_unduplicated,
		\(data = "") {
			dups <- data[duplicated((data[ ,"Date"])), "Date"]
			dt <- data[!duplicated(data[ ,"Date"]), ]
			dt[dt[ ,"Date"] %in% dups, 2] <- NA
			dt
		}
	)





# poulating missing years in each dataframe of the list "Navrongo_RR_Corrected_unduplicated"
Navrongo_TMIN_RCP85_Corrected_unduplicated %<>%
	lapply(
		#Navrongo_RR_Corrected_unduplicated,
		\(data = "") {
			dplyr::full_join(
				data,
				data.frame(Date = seq(as.Date("1951-01-01"), as.Date("2100-12-31"), by = "day"))
			) |> . =>
				.[order(.[ ,"Date"]), ]
		}
	)


# Data Reshaping ####
Navrongo_TMIN_RCP85 <- lapply(
	Navrongo_TMIN_RCP85_Corrected_unduplicated,
	
	\(data = "") {
		if(identical(Navrongo_TMIN_RCP85_Corrected_unduplicated[[1]], data)){
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

# Navrongo_RR_RCP85 <- data.frame(
# 	Date = Navrongo_RR_RCP85_Corrected_unduplicated[[1]][ ,"Date"],
# 	CNRM = Navrongo_RR_RCP85_Corrected_unduplicated[[1]][ ,"Rain"],
# 	MIROC5 = Navrongo_RR_RCP85_Corrected_unduplicated[[2]][ ,"Rain"],
# 	MPI = Navrongo_RR_RCP85_Corrected_unduplicated[[3]][ ,"Rain"],
# 	NOAA = Navrongo_RR_RCP85_Corrected_unduplicated[[4]][ , "Rain"]
# ) |> . =>
# 	within(
# 		.,
# 		{Ensemble = apply(.[ ,-1], 1, mean, na.rm = TRUE)}
# 	)


# Aggregating to annual Rainfall ####
# SPLIT-APPLY-COMBINE METHOD RETURNS ALL NAs AS ZEROs
# The years 1952:1955 have no entries for observations for the models CM5A and MIROC5 except CNRM
split(
	Navrongo_TMIN_RCP85[ ,-1],
	format(Navrongo_TMIN_RCP85[ ,"Date"], format = "%Y")
) |> 
	lapply(
		\(data = "") sapply(data, mean, na.rm = TRUE) 
	) |> . =>
	do.call(rbind, .) -> Navrongo_MeanTMIN_RCP85

# # Alternatively
# aggregate(
# 	cbind(CNRM, CM5A, MIROC5, Ensemble) ~ format(Navrongo_RR_RCP85$Date, "%Y"),
# 	data = Navrongo_RR_RCP85,
# 	FUN = sum,
# 	na.rm = TRUE
# ) |> head()






# Building all Ensembles into a dataframe ####
`Ensembles of scenarios TMIN Navrongo` <- data.frame(
	Date = as.numeric(rownames(Navrongo_MeanTMIN_RCP26)),
	RCP26 = Navrongo_MeanTMIN_RCP26[ ,"Ensemble"],
	RCP45 = Navrongo_MeanTMIN_RCP45[ ,"Ensemble"],
	RCP85 = Navrongo_MeanTMIN_RCP85[ ,"Ensemble"]
) |> 
	subset(Date >= 1980)

# Observed, historicals of scenarios plus eNsemble of historical of scenarios ####
`observed + Hist_scenarios TMIN Navrongo` = data.frame(
	observed = read.csv(
		"C:/Users/pc/OneDrive/Documents/Climate_of_Select_Stations/Navrongo Min Tmp.csv",
		col.names = c("Year", "observed")
	),
	RCP26 = subset(`Ensembles of scenarios TMIN Navrongo`, Date <= 2021)[ ,"RCP26"],
	RCP45 = subset(`Ensembles of scenarios TMIN Navrongo`, Date <= 2021)[ ,"RCP45"],
	RCP85 = subset(`Ensembles of scenarios TMIN Navrongo`, Date <= 2021)[ ,"RCP85"]
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
	setNames(c("Year", "observed", "RCP26", "RCP45", "RCP85", "Ensemble of hist scenarios"))





# Visualization
gridExtra::grid.arrange(
	ggplot(data = `Ensembles of scenarios Navrongo`,aes(x = Date)) +
		geom_line(data = `observed + Hist_scenarios Navrongo`, aes(x = Year, y = RCP26, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios Navrongo`, aes(x = Year, y = RCP45, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios Navrongo`, aes(x = Year, y = RCP85, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios Navrongo`, aes(x = Year, y = `Ensemble of hist scenarios`, col = "Ensemble"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios Navrongo`, aes(x = Year, y = observed, col = "Observed"), lwd = 2)+
		geom_line(data = subset(`Ensembles of scenarios Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP26, col = "RCP26"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP45, col = "RCP45"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP85, col = "RCP85"), lwd = 1) +
		geom_smooth(data = subset(`Ensembles of scenarios Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP26, col = "RCP26"), lwd = 1, method = "loess")  +
		geom_smooth(data = subset(`Ensembles of scenarios Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP45, col = "RCP45"), lwd = 1, method = "loess") +
		geom_smooth(data = subset(`Ensembles of scenarios Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP85, col = "RCP85"), lwd = 1, method = "loess") +
		scale_colour_manual("", breaks = c("Observed","Ensemble","Historical", "RCP26", "RCP45", "RCP85"),
						values = c("darkblue","black", "grey85", "red", "darkgreen", "brown")) +
		scale_x_continuous(breaks = seq(1980,2100 ,by = 35), limits = c(1980, 2100)) +
		labs(subtitle = "Navrongo Rainfall", y = "Rainfall(mm)", x = "Year") +
		theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
			 axis.text = element_text(size = 30),
			 axis.title = element_text(size = 28),
			 legend.text = element_text(size = 26)),
	
	ggplot(data = `Ensembles of scenarios TMAX Navrongo`,aes(x = Date)) +
		geom_line(data = `observed + Hist_scenarios TMAX Navrongo`, aes(x = Year, y = RCP26, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMAX Navrongo`, aes(x = Year, y = RCP45, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMAX Navrongo`, aes(x = Year, y = RCP85, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMAX Navrongo`, aes(x = Year, y = `Ensemble of hist scenarios`, col = "Ensemble"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMAX Navrongo`, aes(x = Year, y = observed, col = "Observed"), lwd = 2)+
		geom_line(data = subset(`Ensembles of scenarios TMAX Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP26, col = "RCP26"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios TMAX Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP45, col = "RCP45"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios TMAX Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP85, col = "RCP85"), lwd = 1) +
		scale_colour_manual("", breaks = c("Observed","Ensemble","Historical", "RCP26", "RCP45", "RCP85"),
						values = c("darkblue","black", "grey85", "red", "darkgreen", "brown")) +
		scale_x_continuous(breaks = seq(1980,2100 ,by = 35), limits = c(1980, 2100)) +
		labs(subtitle = "Navrongo Maximum Temperature", y = expression("Temperature("~degree*C*")"), x = "Year") +
		theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
			 axis.text = element_text(size = 30),
			 axis.title = element_text(size = 28),
			 legend.text = element_text(size = 26)),
	
	ggplot(data = `Ensembles of scenarios TMIN Navrongo`,aes(x = Date)) +
		geom_line(data = `observed + Hist_scenarios TMIN Navrongo`, aes(x = Year, y = RCP26, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMIN Navrongo`, aes(x = Year, y = RCP45, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMIN Navrongo`, aes(x = Year, y = RCP85, col = "Historical"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMIN Navrongo`, aes(x = Year, y = `Ensemble of hist scenarios`, col = "Ensemble"), lwd = 1) +
		geom_line(data = `observed + Hist_scenarios TMIN Navrongo`, aes(x = Year, y = observed, col = "Observed"), lwd = 2)+
		geom_line(data = subset(`Ensembles of scenarios TMIN Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP26, col = "RCP26"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios TMIN Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP45, col = "RCP45"), lwd = 1) +
		geom_line(data = subset(`Ensembles of scenarios TMIN Navrongo`, Date >= 2022), 
				aes(x = Date, y = RCP85, col = "RCP85"), lwd = 1) +
		scale_colour_manual("", breaks = c("Observed","Ensemble","Historical", "RCP26", "RCP45", "RCP85"),
						values = c("darkblue","black", "grey85", "red", "darkgreen", "brown")) +
		scale_x_continuous(breaks = seq(1980,2100 ,by = 35), limits = c(1980, 2100)) +
		labs(subtitle = "Navrongo Minimum Temperature", y = expression("Temperature("~degree*C*")"), x = "Year") +
		theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
			 axis.text = element_text(size = 30),
			 axis.title = element_text(size = 28),
			 legend.text = element_text(size = 26)),
	
	nrow = 2, ncol = 2
)

dev.copy(png, filename = "Plots_Outputs/Projected Climate of Navrongo.png", width = 1500, height = 900)
dev.off()



