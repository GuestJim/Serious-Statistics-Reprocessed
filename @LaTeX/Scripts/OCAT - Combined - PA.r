#	this script is to collect all of the CSVs into one CSV
library(readr)

game	=	"!GAME!"
COMPRESS	=	TRUE

setwd("!PATH!")

OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))

listLOC	=	c(
!LOC!
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

!LONG!

if	(COMPRESS)	{
	write_csv(OCATcomb, "@Combined - Max.csv.bz2")
}	else	{
	write_csv(OCATcomb, "@Combined - Max.csv")
}