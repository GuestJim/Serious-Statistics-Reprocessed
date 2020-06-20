#	this script is to collect all of the CSVs into one CSV
library(readr)

game	=	"Serious Statistics and Scripts"
COMPRESS	=	TRUE

COLUMN	=	NULL	;	SUBSET	=	NULL

setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/OCAT Data/RX Vega 64/Vulkan/Max/")
relPath	=	paste0(unlist(strsplit(getwd(), "OCAT Data"))[1], "OCAT Data")

if	(getwd() == relPath & (is.null(COLUMN) & is.null(SUBSET)))	{
	COLUMN	=	"Quality"
	SUBSET	=	"High"
}

txtFIND	=	function(TXT, rel.Path = relPath)	{
	locFILE	=	paste0(rel.Path, "/", TXT)
	if (file.exists(locFILE))	return(readLines(locFILE, warn = FALSE))
	return(NULL)
}

listLOC	=	txtFIND("Locations.txt")

csvFIND	=	function(DIRECT = getwd())	{
	LIST		=	list.files(DIRECT, recursive = TRUE, pattern = ".csv")
	LIST		=	LIST[grepl("OCAT", LIST) & grepl("csv", LIST)]
	LIST.full	=	paste0(DIRECT, "/", LIST)
	LIST.rel	=	t(data.frame(lapply(LIST.full, strsplit, "OCAT Data/"), row.names = NULL)[2, ])
	colnames(LIST.rel)	=	NULL
	rownames(LIST.rel)	=	NULL

	return(LIST.rel)
}

# LIST.rel	=	csvFIND()


csvCONF	=	function(CSV.list, LOCs = listLOC)	{
	CSV.config	=	t(as.data.frame(sapply(CSV.list, strsplit, "/")))
	colnames(CSV.config)	=	NULL
	rownames(CSV.config)	=	NULL

	CONFIG	=	data.frame(matrix(ncol = 5, nrow = nrow(CSV.config)))
	colnames(CONFIG)	=	c("GPU", "API", "Quality", "Location", "CSV")

	CONFIG$GPU		=	CSV.config[, 1]
	if	(ncol(CSV.config) == 4)	CONFIG$API		=	CSV.config[, 2]
	CONFIG$Quality	=	CSV.config[, ncol(CSV.config)-1]

	appLOC	=	function(DATA, LOCs)	{
		if (is.null(LOCs))	LOCs	=	paste0("Recording ", 1:nrow(DATA))
		rep(LOCs, length.out = nrow(DATA))
	}
	GROUPS	=	list(GPU = CONFIG$GPU, API = CONFIG$API, Quality = CONFIG$Quality)
	if (any(is.na(GROUPS$API)))	GROUPS$API	=	NULL
	CONFIG$Location	=	unlist(by(CONFIG, GROUPS, appLOC, LOCs))
#	appLOC and by together apply the Location names, and generate them if necessary, to match the GPU-API-Quality groups
	CONFIG$CSV		=	CSV.config[, ncol(CSV.config)]

	return(CONFIG)
}

# CSV.config	=	csvCONF(LIST.rel)
CSV.configFull	=	csvCONF(csvFIND())


csvFILT	=	function(CSV.list, COL, SUB)	{
	if (!is.null(COL)	&	!is.null(SUB))	return(CSV.list[CSV.list[, COL] == SUB, ])
	return(CSV.list)
}

CSV.config	=	csvFILT(CSV.configFull, COLUMN, SUBSET)


read_OCAT	=	function(INFO, GPU = NULL, API = NULL, QUA = NULL, LOC = NULL)	{
	if (is.data.frame(INFO))	{
		GPU		=	INFO$GPU
		API		=	INFO$API
		QUA		=	INFO$Quality
		LOC		=	INFO$Location
		FILE	=	INFO$CSV
	}	else	{FILE	=	INFO}

	filePATH		=	paste(relPath, GPU, QUA, FILE, sep = "/")
	if	(!any(is.na(API), is.null(API)))	filePATH		=	paste(relPath, GPU, API, QUA, FILE, sep = "/")

	out				=	read_csv(filePATH)[, 1:20]

	out$GPU			=	GPU
	out$Quality		=	QUA
	out$Location	=	LOC
	out$API			=	""
	if	(!any(is.na(API), is.null(API)))	out$API	=	API

	return(out)
}


csvOCAT	=	function(CSVs)	{
	OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
	for (ROW in 1:nrow(CSVs))	{
		OCATcomb	=	rbind(OCATcomb, read_OCAT(CSVs[ROW, ]))
	}

	return(OCATcomb)
}

OCATcomb	=	csvOCAT(CSV.config)

if	(COMPRESS)	{
	write_csv(OCATcomb, "@Combined - Max.csv.bz2")
}	else	{
	write_csv(OCATcomb, "@Combined - Max.csv")
}