library(readr)

#	this script is to collect all of the CSVs into one CSV

game = "Serious Statistics and Scripts"

setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/OCAT Data/RX Vega 64/Vulkan/Max/")

OCATcomb = data.frame(matrix(ncol = 24, nrow = 0))
OCATtemp = data.frame(matrix(ncol = 24, nrow = 0))

LOC = c(
"Hatsheput",
"Dunes",
"Thebes - Karnak"
)

READ = function(fold="", Quality = "", API="") {
	if (API != "") {
		API = paste0(API, "/")
	}
	if (length(LOC[1]) == 0)	{
		LOC = paste0(rep("Recording ", length(CSV)), 1:length(CSV))
	}
	len = min(length(LOC), length(CSV))	
	for (place in 1:len) {
		if (CSV[place] != ".csv") {
			if (grepl(GPU, getwd()))	{
				OCATtemp = read_csv(paste0(CSV[place]))[,1:20]
			}	else	{
				OCATtemp = read_csv(paste0(fold, GPU, "/" , API, Quality, "/", CSV[place]))[,1:20]
			}
		} else {next}
		OCATtemp[,21] = GPU
		OCATtemp[,22] = Quality
		OCATtemp[,23] = LOC[place]
		OCATtemp[,24] = gsub("/", "", API)
		OCATcomb = rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}


GPU = "RX Vega 64"
CSV = c(
"OCAT-Sam2017.exe-2019-07-20T104032",
"OCAT-Sam2017.exe-2019-07-20T104635",
"OCAT-Sam2017.exe-2019-07-20T105239"
)
CSV = paste0(CSV, ".csv")
OCATcomb = READ("", "Max", "Vulkan")


colnames(OCATcomb)[21:24] = c("GPU", "Quality", "Location", "API")
write_csv(OCATcomb, "@Combined - Max.csv")