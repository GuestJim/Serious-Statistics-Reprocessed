library(readr)
library(ggplot2)
library(moments)

game = "Serious Statistics and Scripts"
cGPU = "RX Vega 64"

gameF = gsub(":", "-", game)
gameF = unlist(strsplit(gameF, split=" [(]"))[1]

theme_set(theme_grey(base_size = 16))
DPI = 120
ggdevice = "png"

textOUT		=	TRUE
HTMLOUT		=	TRUE
graphs		=	TRUE
graphs_all	=	FALSE
useSHORT	=	FALSE

textFRAM	=	TRUE
graphFRAM	=	TRUE

textDISP	=	FALSE
graphDISP	=	FALSE

textAPI		=	FALSE
textLOC		=	FALSE
#	will generate TXT and HTML, if HTML is enabled, files for each API/Location

textDIFF	=	FALSE
graphDIFF	=	FALSE

listFPS		=	NULL
#	for adding to the FPS Percentile list
QUAN		=	c(0.01, 0.99)
FtimeLimit	=	1000/60

gWIDTH	=	8
gHEIGH	=	9

if (!textOUT)	{
	textFRAM	=	FALSE
	textDISP	=	FALSE
	HTMLOUT		=	FALSE
	textDIFF	=	FALSE
}

if (!graphs){
	graphFRAM	=	FALSE
	graphDISP	=	FALSE
	graphDIFF	=	FALSE
}

if (interactive()) {
	setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/OCAT Data/RX Vega 64/DirectX 11 - Anti-Lag/Max/")
} else {
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#	prevents rplots.pdf from being generated

# resultsFull <- read_csv("@Combined - Max.csv", col_types = "????????????????????c")
resultsFull <- read_csv("@Combined - Max.csv")
results = resultsFull

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

listQUA		=	c(
"Max"
)

listLOC		=	c(
"Hatsheput",
"Dunes",
"Thebes - Karnak"
)

shortLOC	=	c(
"Hatsheput",
"Dunes",
"Karnak"
)

listAPI		=	c(
"DirectX 11",
"DirectX 12",
"Vulkan",
"DirectX 11 - Anti-Lag"
)

shortAPI	=	c(
"DX11",
"DX12",
"Vulkan",
"DX11 - Anti-Lag"
)

if (graphDIFF)	{
	DIFF = as.data.frame(NULL)
	cols = c("MsDifferencePresents", "MsDifferenceDisplayChange")

	for (gpu in unique(results$GPU))		{
	for (qua in unique(results$Quality))	{
	for (loc in unique(results$Location))	{
	for (api in unique(results$API))		{
		if (paste0(unique(results$API[1])) == "NA"){
			temp	=	results[results$GPU == gpu & results$Quality == qua & results$Location == loc, ]
		}	else	{
			temp	=	results[results$GPU == gpu & results$Quality == qua & results$Location == loc & results$API == api, ]
		}
		tempD	=	as.data.frame(cbind(c(diff(temp$MsBetweenPresents), 0), c(diff(temp$MsBetweenDisplayChange), 0)))

		if (dim(tempD)[1] > 1)	{
			DIFF	=	rbind(DIFF, tempD)
		}
	}	}	}	}
	colnames(DIFF) = cols
	
	resultsFull = cbind(resultsFull, DIFF)
	results = resultsFull
}


results$GPU = factor(results$GPU, levels = listGPU, ordered = TRUE)
results$Quality = factor(results$Quality, levels = listQUA)
if (length(listLOC[1]) != 0) {
	results$Location = factor(results$Location, levels = listLOC, ordered = TRUE)
}
results$API = factor(results$API, levels = listAPI, ordered = TRUE)


reLoc	=	function(DATA, shortLoc = NULL)	{
	if (!is.null(shortLoc)){
		# for (i in 1:length(shortLOC))	{
		for (i in length(shortLOC):1)	{
			DATA$Location = gsub(listLOC[i], shortLOC[i], DATA$Location)
		}
		DATA$Location = factor(DATA$Location, levels = shortLoc, ordered = TRUE)
	}
	return(DATA)
}

reAPI	=	function(DATA, shortAPI = NULL)	{
	if (!is.null(shortAPI)){
		# for (i in 1:length(shortAPI))	{
		for (i in length(shortAPI):1)	{
			DATA$API = gsub(listAPI[i], shortAPI[i], DATA$API, fixed=TRUE)
		}
		DATA$API = factor(DATA$API, levels = shortAPI, ordered = TRUE)
	}
	return(DATA)
}
#	reversed the order for going through the lists to address an issue with names being changed because they are a common substring


if (is.null(cGPU))	{
	multiGPU	=	TRUE
}	else	{
	multiGPU	=	FALSE
}

if (length(unique(results$API)) >= 2) {
	testAPI = TRUE
}	else	{
	testAPI = FALSE
}

if (levels(results$Quality)[1] != "Review")	{
	QUA = paste0(levels(results$Quality)[1], " Quality")
	qua = paste0(levels(results$Quality)[1])
}	else	{
	QUA = "Review"
}

gameQ		=	paste0(game, " - ", QUA)
gameGAQ		=	game
gameGAQF	=	gameF

if	(length(unique(results$GPU)) == 1 && !is.null(cGPU))	{
	gameGAQ		=	paste0(gameGAQ, " - ", cGPU)
	gameGAQF	=	paste0(gameGAQF, " - ", cGPU)	
}
if	(!testAPI)	{
	gameGAQ		=	paste0(gameGAQ, " - ", unique(results$API))
	gameGAQF	=	paste0(gameGAQF, " - ", unique(results$API))
}
gameGAQ		=	paste0(gameGAQ, " - ", QUA)
gameGAQF	=	paste0(gameGAQF, " - ", qua)

source("@Combined - Output.r")


if	(graphs_all)	{

for	(loc in listLOC)	{
	textFRAM	=	FALSE
	textDISP	=	FALSE
	HTMLOUT		=	FALSE
	textDIFF	=	FALSE
#	because I do not need statistics for the individual runs; just the graphs

	results = resultsFull[resultsFull$Location == loc, ]
#	would want to change this to be for the GPU when dealing with the multi-GPU data. Location would be for the single-GPU results and is the normal use for this
#		with a GPU version, having it change the work directory may not be a bad idea, so it sticks the files in the appropriate places

	gameQ		=	paste0(game, " - ", QUA, " - ", loc)
	gameGAQ		=	game
	gameGAQF	=	gameF

	if	(length(unique(results$GPU)) == 1 && cGPU != ""RX Vega 64"")	{
		gameGAQ		=	paste0(gameGAQ, " - ", cGPU)
		gameGAQF	=	paste0(gameGAQF, " - ", cGPU)	
	}
	if	(!testAPI)	{
		gameGAQ		=	paste0(gameGAQ, " - ", unique(results$API))
		gameGAQF	=	paste0(gameGAQF, " - ", unique(results$API))
	}
	gameGAQ		=	paste0(gameGAQ, " - ", QUA, " - ", loc)
	gameGAQF	=	paste0(gameGAQF, " - ", qua, " - ", loc)
	
	source("@Combined - Output.r")
	#	have tested it and the Output script is now appropriately abstracted that it will work
}
}