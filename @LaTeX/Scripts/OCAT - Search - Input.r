library(readr)
library(ggplot2)
library(moments)

game	=	"!GAME!"
# cGPU	=	!GPU!

gameF	=	gsub(":", "-", game)
gameF	=	unlist(strsplit(gameF, split=" [(]"))[1]

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

COLUMN	=	NULL	;	SUBSET	=	NULL
#	these are changed later if NULL and if it is found we are in the OCAT Data folder

useSHORT	=	TRUE
# testAPI		=	FALSE
listFPS		=	NULL
#	for adding to the FPS Percentile list
diffLim		=	NULL
QUAN		=	c(0.01, 0.99)
FtimeLimit	=	1000/15
yratesEXT	=	NULL

gWIDTH	=	8
gHEIGH	=	9
app.BREAK	=	FALSE
#	switch for if line breaks should be used in the labels
#		can be changed prior to graphs being created for selective application

textOUT		=	TRUE
HTMLOUT		=	TRUE
graphs		=	TRUE
graphs_all	=	FALSE

textFRAM	=	TRUE
graphFRAM	=	TRUE

textDISP	=	FALSE
graphDISP	=	FALSE

textREND	=	FALSE
graphREND	=	FALSE

textDRIV	=	FALSE
graphDRIV	=	FALSE

textAPI		=	FALSE
textLOC		=	FALSE
#	will generate TXT and HTML, if HTML is enabled, files for each API/Location

textDiff	=	FALSE
graphDiff	=	FALSE
#	cannot be DIFF because of naming conflict in Output

if (interactive())	{
	setwd("!PATH!")
}	else	{
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#		prevents rplots.pdf from being generated

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

listGPU		=	c(
"RX 580",
"RX Vega 64",
"GTX 770",
"GTX 980",
"GTX 1070",
"GTX 1080",
"RTX 2060",
"RTX 2080"
)

listQUA		=	txtFIND("Qualities.txt")

listLOC		=	txtFIND("Locations.txt")
shortLOC	=	txtFIND("Locations Short.txt")
levsLOC		=	listLOC
if	(useSHORT	&	!is.null(shortLOC))	levsLOC	=	shortLOC

listAPI		=	txtFIND("APIs.txt")
shortAPI	=	txtFIND("APIs Short.txt")
levsAPI		=	listAPI
if	(useSHORT	&	!is.null(shortAPI))	levsAPI	=	shortAPI


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


typeFIND	=	function(DATA, TYPE)	{
	if	(length(unique(DATA[, TYPE])) == 1)	return(unique(DATA[, TYPE]))
	return(NULL)
}

cGPU	=	typeFIND(CSV.config, "GPU")
multiGPU	=	is.null(cGPU)
labsGPU		=	labs(caption = cGPU)
if (multiGPU)	labsGPU	=	labs()

testAPI	=	is.null(typeFIND(CSV.config, "API"))
testQUA	=	is.null(typeFIND(CSV.config, "Quality"))


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
	if (!any(is.na(API), is.null(API)))	out$API	=	API
	
	return(out)
}


csvOCAT	=	function(CSVs)	{
	OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
	for (ROW in 1:nrow(CSVs))	{
		OCATcomb	=	rbind(OCATcomb, read_OCAT(CSVs[ROW, ]))
	}
	
	return(OCATcomb)
}

resultsFull	=	csvOCAT(CSV.config)


resultsFull$GPU		=	ordered(resultsFull$GPU,		levels = listGPU)
resultsFull$Quality	=	ordered(resultsFull$Quality)
if	(!is.null(listQUA))	resultsFull$Quality		=	ordered(resultsFull$Quality,	levels = listQUA)

resultsFull$Location	=	ordered(resultsFull$Location)
if	(!is.null(listLOC)) resultsFull$Location	=	ordered(resultsFull$Location,	levels = listLOC)

resultsFull$API		=	ordered(resultsFull$API)
if	(!is.null(listAPI)) resultsFull$API			=	ordered(resultsFull$API,		levels = listAPI)

lockBinding("resultsFull", .GlobalEnv)
#	locks the variable in the Global Environment, preventing it from being altered

results = resultsFull
#	protects the original data, after having been formatted
#	not so necessary, as all edits to it occur within function environments

GROUPS	=	list(GPU = results$GPU, API = results$API, Quality = results$Quality, Location = results$Location)
if	(!testAPI)	GROUPS$API		=	NULL
if	(!testQUA)	GROUPS$Quality	=	NULL
#	this way I can start with a full GROUPS and then remove the unnecesary components
#		this is easier than trying to build it up

diff.CONS	=	function(DATA, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(DATA, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(DATA, lag = lag)))
}

if (textDiff	|	graphDiff)	{
	results$MsDifferencePresents		=	unlist(by(results$MsBetweenPresents, GROUPS, diff.CONS))
	results$MsDifferenceDisplayChange	=	unlist(by(results$MsBetweenDisplayChange, GROUPS, diff.CONS))
}

DESC	=	function(ITEM = NULL)	{
	descs	=	list(GPU = unique(as.character(results$GPU)), API = unique(as.character(results$API)), Location = unique(as.character(results$Location)), Quality = unique(as.character(results$Quality)))
	
	if	(length(descs$GPU)		> 1)	descs$GPU		=	NULL
	if	(length(descs$API)		> 1)	descs$API		=	NULL
	if	(length(descs$Location)	> 1)	descs$Location	=	NULL
	if	(length(descs$Quality)	> 1)	descs$Quality	=	NULL
	
	gameQ	=	game
	if	(!is.null(descs$Quality))	gameQ	=	paste0(game,	" - ",	descs$Quality,	" Quality")
	
	gameGAQF	=	paste0(gameF,	" - ",	paste0(descs,	collapse = " - ")	)
	gameGAQ		=	paste0(game,	" - ",	paste0(descs,	collapse = " - ")	)
	if	(!is.null(descs$Quality))	gameGAQ	=	paste0(gameGAQ, " Quality")
	
	if	(!is.null(ITEM))	{
		gameGAQF	=	paste0(gameGAQF,	" - ",	ITEM)
		gameGAQ		=	paste0(gameGAQ,		" - ",	ITEM)
	}
	return(c(gameGAQF,	gameGAQ, gameQ))
}
gameGAQF	=	DESC()[1]	;	gameGAQ		=	DESC()[2]	;	gameQ	=	DESC()[3]

INDIV	=	function(COL, SUBS, useSHORT = useSHORT, gWIDTH = gWIDTH, gHEIGH = gHEIGH)	{
	if	(COL != "GPU")	dir.create(paste0("@", COL))

	for	(ITEM in SUBS)	{
		# COL	<<-	COL
		# SUBS	<<-	SUBS
		# ITEM	<<-	ITEM
		#	helpful for troubleshooting
		
		
		message(paste0("\n", ITEM))
		results	=	resultsFull[resultsFull[, COL] == ITEM, ]
		if (nrow(results)	==	0)	next
		if (COL == "GPU" & length(unique(results$API)) == 1)	next

		if	(COL != "GPU")	{
			FOLD	=	paste0("@", COL, "/", ITEM)
			dir.create(FOLD)
		}

		gameGAQF	=	DESC(ITEM)[1]	;	gameGAQ		=	DESC(ITEM)[2]	;	gameQ	=	DESC(ITEM)[3]
		if (COL == "GPU")	{
			gameGAQF	=	paste0(ITEM, "/", gameGAQF)
		}	else	{
			gameGAQF	=	paste0(FOLD, "/", gameGAQF)
		}

		perGPU	=	FALSE
		source("@Combined - Output.r",	local = TRUE)
	}
}

perGPU	=	TRUE	#	used for creating stats files in each GPU in their folders
if (!multiGPU)	perGPU	=	FALSE
source("@Combined - Output.r")

if	(graphs_all)	{
# INDIV("GPU",		listGPU,	useSHORT = TRUE,	iWIDTH = gWIDTH * 1.25,	iHEIGH = gHEIGH * 2)
# INDIV("Location",	listLOC,	useSHORT = FALSE,	iWIDTH = gWIDTH * 1.25,	iHEIGH = gHEIGH * 1)
# INDIV("API",		listAPI,	useSHORT = TRUE,	iWIDTH = gWIDTH * 1.25,	iHEIGH = gHEIGH * 1)
}