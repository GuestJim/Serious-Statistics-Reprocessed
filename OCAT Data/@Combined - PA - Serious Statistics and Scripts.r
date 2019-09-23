#	this script is to collect all of the CSVs into one CSV
library(readr)

game	=	"Serious Statistics and Scripts"

setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/OCAT Data/")

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


GPU	=	"RX Vega 64"
CSV	=	c(
"OCAT-Sam2017.exe-2019-07-20T105950",
"OCAT-Sam2017.exe-2019-07-20T110524",
"OCAT-Sam2017.exe-2019-07-20T111116"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	READ("", "Max", "DirectX 11")

GPU	=	"RX Vega 64"
CSV	=	c(
"OCAT-Sam2017.exe-2019-07-20T102033",
"OCAT-Sam2017.exe-2019-07-20T102708",
"OCAT-Sam2017.exe-2019-07-20T103333"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	READ("", "Max", "DirectX 12")

GPU	=	"RX Vega 64"
CSV	=	c(
"OCAT-Sam2017.exe-2019-07-20T104032",
"OCAT-Sam2017.exe-2019-07-20T104635",
"OCAT-Sam2017.exe-2019-07-20T105239"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	READ("", "Max", "Vulkan")

GPU	=	"RX Vega 64"
CSV	=	c(
"OCAT-Sam2017.exe-2019-07-20T111805",
"OCAT-Sam2017.exe-2019-07-20T112356",
"OCAT-Sam2017.exe-2019-07-20T112923"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	READ("", "Max", "DirectX 11 - Anti-Lag")

GPU	=	"RTX 2060"
CSV	=	c(
"OCAT-Sam2017.exe-2019-08-22T105551",
"OCAT-Sam2017.exe-2019-08-22T110136",
"OCAT-Sam2017.exe-2019-08-22T110709"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	READ("", "Max", "DirectX 11")

GPU	=	"RTX 2060"
CSV	=	c(
"OCAT-Sam2017.exe-2019-08-22T101729",
"OCAT-Sam2017.exe-2019-08-22T102316",
"OCAT-Sam2017.exe-2019-08-22T102921"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	READ("", "Max", "DirectX 12")

GPU	=	"RTX 2060"
CSV	=	c(
"OCAT-Sam2017.exe-2019-08-22T103648",
"OCAT-Sam2017.exe-2019-08-22T104234",
"OCAT-Sam2017.exe-2019-08-22T104810"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	READ("", "Max", "Vulkan")

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