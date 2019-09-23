#	this script is to collect all of the CSVs into one CSV
library(readr)

game	=	"Serious Statistics and Scripts"

setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/OCAT Data/RTX 2060/DirectX 11 - Anti-Lag/Max/")

OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
OCATtemp	=	data.frame(matrix(ncol = 24, nrow = 0))

listLOC	=	c(
"Hatsheput",
"Dunes",
"Thebes - Karnak"
)

READ	=	function(fold="", Quality = "", API="")	{
	if (API != "") {
		API	=	paste0(API, "/")
	}
	if (length(listLOC[1]) == 0)	{
		listLOC	=	paste0("Recording ", 1:length(CSV))
	}
	len	=	min(length(listLOC), length(CSV))
	for (place in 1:len)	{
		if (CSV[place] != ".csv")	{
			if (grepl(GPU, getwd()))	{
				OCATtemp	=	read_csv(paste0(CSV[place]))[,1:20]
			}	else	{
				OCATtemp	=	read_csv(paste0(fold, GPU, "/" , API, Quality, "/", CSV[place]))[,1:20]
			}
		}	else {next}
		OCATtemp[,21]	=	GPU
		OCATtemp[,22]	=	Quality
		OCATtemp[,23]	=	listLOC[place]
		OCATtemp[,24]	=	gsub("/", "", API)
		OCATcomb	=	rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}


GPU	=	"RTX 2060"
CSV	=	c(
"OCAT-Sam2017.exe-2019-08-22T111400",
"OCAT-Sam2017.exe-2019-08-22T111924",
"OCAT-Sam2017.exe-2019-08-22T112520"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	READ("", "Max", "DirectX 11 - Anti-Lag")


colnames(OCATcomb)[21:24]	=	c("GPU", "Quality", "Location", "API")
write_csv(OCATcomb, "@Combined - Max.csv")