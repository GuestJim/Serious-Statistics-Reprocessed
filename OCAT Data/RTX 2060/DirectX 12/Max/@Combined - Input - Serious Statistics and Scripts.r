library(readr)
library(ggplot2)
library(moments)

game	=	"Serious Statistics and Scripts"
cGPU	=	"RTX 2060"

gameF	=	gsub(":", "-", game)
gameF	=	unlist(strsplit(gameF, split=" [(]"))[1]

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

useSHORT	=	TRUE
testAPI		=	TRUE
listFPS		=	NULL
#	for adding to the FPS Percentile list
diffLim		=	NULL
QUAN		=	c(0.01, 0.99)
FtimeLimit	=	1000/60
yratesEXT	=	NULL

gWIDTH	=	12
gHEIGH	=	9
app.BREAK	=	FALSE
#	switch for if line breaks should be used in the labels
#		can be changed prior to graphs being created for selective application
testQUA		=	FALSE

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
	setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/OCAT Data/RTX 2060/DirectX 12/Max/")
}	else	{
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#		prevents rplots.pdf from being generated

relPath	=	paste0(unlist(strsplit(getwd(), "OCAT Data"))[1], "OCAT Data")

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


if (file.exists("@Combined - Max.csv.bz2"))	{
	resultsFull	=	read_csv("@Combined - Max.csv.bz2")
}	else	{
	resultsFull	=	read_csv("@Combined - Max.csv")
}


resultsFull$GPU		=	ordered(resultsFull$GPU,		levels = listGPU)
resultsFull$Quality	=	ordered(resultsFull$Quality)
if	(!is.null(listQUA))	resultsFull$Quality		=	ordered(resultsFull$Quality,	levels = listQUA)

resultsFull$Location	=	ordered(resultsFull$Location)
if	(!is.null(listLOC)) resultsFull$Location	=	ordered(resultsFull$Location,	levels = listLOC)

resultsFull$API		=	ordered(resultsFull$API)
if	(!is.null(listAPI)) resultsFull$API			=	ordered(resultsFull$API,		levels = listAPI)

lockBinding("resultsFull", .GlobalEnv)
#	locks the variable in the Global Environment, preventing it from being altered

results	=	resultsFull
#	protects the original data, after having been formatted
#	not so necessary, as all edits to it occur within function environments

multiGPU	=	is.null(cGPU)
labsGPU		=	labs(caption = cGPU)
if	(multiGPU)	labsGPU	=	labs()
if	(!testAPI)	testAPI		=	(length(unique(results$API)) >= 2)

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
# INDIV("GPU",		listGPU,	useSHORT = TRUE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 2)
# INDIV("Location",	listLOC,	useSHORT = FALSE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 1)
# INDIV("API",		listAPI,	useSHORT = TRUE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 1)
}