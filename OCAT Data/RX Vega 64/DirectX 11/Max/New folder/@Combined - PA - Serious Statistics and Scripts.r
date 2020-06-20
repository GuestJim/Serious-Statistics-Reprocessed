#	this script is to collect all of the CSVs into one CSV
library(readr)

game	=	"Serious Statistics and Scripts"

setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/OCAT Data/RX Vega 64/DirectX 11/Max/")

OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
COMPRESS	=	TRUE

listLOC	=	c(
"Hatsheput",
"Dunes",
"Thebes - Karnak"
)

READ	=	function(fold="",	CSV,	GPU,	Quality = "",	API="")	{
	if (API != "")	API	=	paste0(API, "/")
	if (length(listLOC[1]) == 0)	listLOC	=	paste0("Recording ", 1:length(CSV))
	
	len	=	min(length(listLOC), length(CSV))
	out	=	data.frame(matrix(ncol = 24, nrow = 0))
	
	for (place in 1:len)	{
		if (CSV[place] != ".csv")	{
			fileLOC	=	paste0(fold, GPU, "/" , Quality, "/", CSV[place])
			if	(API != "")	fileLOC	=	paste0(fold, GPU, "/" , API, Quality, "/", CSV[place])
			
			if	(grepl(GPU, getwd()) & grepl(Quality, getwd()))		fileLOC	=	paste0(CSV[place])
			if	(grepl(GPU, getwd()) & !grepl(Quality, getwd()))	fileLOC	=	paste0(fold, API, Quality, "/",CSV[place])
		}	else {next}
		OCATtemp	=	read_csv(fileLOC)[, 1:20]
		
		OCATtemp$GPU		=	GPU
		OCATtemp$Quality	=	Quality
		OCATtemp$Location	=	listLOC[place]
		OCATtemp$API		=	gsub("/", "", API)
		out	=	rbind(out, OCATtemp)
	}
	return(out)
}


GPU	=	"RX Vega 64"
CSV	=	c(
"OCAT-Sam2017.exe-2019-07-20T105950",
"OCAT-Sam2017.exe-2019-07-20T110524",
"OCAT-Sam2017.exe-2019-07-20T111116"
)
CSV	=	paste0(CSV, ".csv")
OCATcomb	=	rbind(OCATcomb, READ("", CSV, GPU, "Max", "DirectX 11"))


if	(COMPRESS)	{
	write_csv(OCATcomb, "@Combined - Max.csv.bz2")
}	else	{
	write_csv(OCATcomb, "@Combined - Max.csv")
}