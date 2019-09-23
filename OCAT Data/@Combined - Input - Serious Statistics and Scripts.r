library(readr)
library(ggplot2)
library(moments)

game = "Serious Statistics and Scripts"
cGPU = NULL

gameF = gsub(":", "-", game)
gameF = unlist(strsplit(gameF, split=" [(]"))[1]

theme_set(theme_grey(base_size = 16))
DPI = 120
ggdevice = "png"

textOUT		=	TRUE
HTMLOUT		=	TRUE
graphs		=	TRUE
graphs_all	=	FALSE
useSHORT	=	TRUE

textFRAM	=	TRUE
graphFRAM	=	TRUE

textDISP	=	FALSE
graphDISP	=	FALSE

textREND	=	TRUE
graphREND	=	TRUE

textDRIV	=	TRUE
graphDRIV	=	FALSE

textAPI		=	FALSE
textLOC		=	FALSE
#	will generate TXT and HTML, if HTML is enabled, files for each API/Location

textDIFF	=	FALSE
graphDIFF	=	FALSE

listFPS		=	NULL
#	for adding to the FPS Percentile list
QUAN		=	c(0.01, 0.99)
FtimeLimit	=	1000/60

gWIDTH	=	9
gHEIGH	=	16

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
	setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/OCAT Data/")
} else {
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#	prevents rplots.pdf from being generated

# resultsFull <- read_csv("@Combined - High.csv", col_types = "????????????????????c")
resultsFull <- read_csv("@Combined - Max.csv")

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

if (textDIFF | graphDIFF)	{
	DIFF = as.data.frame(NULL)
	cols = c("MsDifferencePresents", "MsDifferenceDisplayChange")

	for (gpu in unique(resultsFull$GPU))		{
	for (qua in unique(resultsFull$Quality))	{
	for (loc in unique(resultsFull$Location))	{
	for (api in unique(resultsFull$API))		{
		if (paste0(unique(resultsFull$API)[1]) == "NA")	{
			temp	=	resultsFull[resultsFull$GPU == gpu & resultsFull$Quality == qua & resultsFull$Location == loc, ]
		}	else	{
			temp	=	resultsFull[resultsFull$GPU == gpu & resultsFull$Quality == qua & resultsFull$Location == loc & resultsFull$API == api, ]
		}
		tempD	=	as.data.frame(cbind(c(diff(temp$MsBetweenPresents), 0), c(diff(temp$MsBetweenDisplayChange), 0)))

		if (nrow(tempD) > 1)	{
			DIFF	=	rbind(DIFF, tempD)
		}
	}	}	}	}
	colnames(DIFF)	=	cols

	resultsFull	=	cbind(resultsFull, DIFF)
}


resultsFull$GPU		=	factor(resultsFull$GPU, levels = listGPU, ordered = TRUE)
resultsFull$Quality	=	factor(resultsFull$Quality, levels = listQUA)
if (length(listLOC[1]) != 0) {
	resultsFull$Location	=	factor(resultsFull$Location, levels = listLOC, ordered = TRUE)
}
resultsFull$API		=	factor(resultsFull$API, levels = listAPI, ordered = TRUE)

results = resultsFull

reLoc	=	function(DATA, shortLOC = NULL)	{
	if (!is.null(shortLOC)	&	length(unique(DATA$Location)) > 1)	{
		for (i in length(shortLOC):1)	{
			DATA$Location	=	gsub(listLOC[i], shortLOC[i], DATA$Location)
		}
		DATA$Location	=	factor(DATA$Location, levels = shortLOC, ordered = TRUE)
	}
	return(DATA)
}

reAPI	=	function(DATA, shortAPI = NULL)	{
	if (!is.null(shortAPI)	&	testAPI)	{
		for (i in length(shortAPI):1)	{
			DATA$API	=	gsub(listAPI[i], shortAPI[i], DATA$API, fixed=TRUE)
		}
		DATA$API	=	factor(DATA$API, levels = shortAPI, ordered = TRUE)
	}
	return(DATA)
}
#	reversed the order for going through the lists to address an issue with names being changed because they are a common substring


if (is.null(cGPU))	{
	multiGPU	=	TRUE
}	else	{
	multiGPU	=	FALSE
}

if (length(unique(results$API)) >= 2)	{
	testAPI	=	TRUE
}	else	{
	testAPI	=	FALSE
}

if (levels(results$Quality)[1] != "Review")	{
	QUA	=	paste0(levels(results$Quality)[1], " Quality")
	qua	=	paste0(levels(results$Quality)[1])
}	else	{
	QUA	=	"Review"
	qua	=	"Review"
}

gameQ		=	paste0(game, " - ", QUA)
gameGAQ		=	game
gameGAQF	=	gameF

if	(!multiGPU)	{
	gameGAQ		=	paste0(gameGAQ, " - ", cGPU)
	gameGAQF	=	paste0(gameGAQF, " - ", cGPU)
}
if	(!testAPI	&	!is.null(listAPI))	{
	gameGAQ		=	paste0(gameGAQ, " - ", unique(results$API))
	gameGAQF	=	paste0(gameGAQF, " - ", unique(results$API))
}
gameGAQ		=	paste0(gameGAQ, " - ", QUA)
gameGAQF	=	paste0(gameGAQF, " - ", qua)

source("@Combined - Output.r")


if	(graphs_all)	{
textFRAM	=	FALSE
textDISP	=	FALSE
textREND	=	FALSE
HTMLOUT		=	FALSE
textDIFF	=	FALSE
#	because the statistics for the individual runs are not needed; just the graphs

for	(loc in listLOC)	{

	message(paste0("\n", loc))
	results	=	resultsFull[resultsFull$Location == loc, ]
#	would want to change this to be for the GPU when dealing with the multi-GPU data. Location would be for the single-GPU results and is the normal use for this
#		with a GPU version, having it change the work directory may not be a bad idea, so it sticks the files in the appropriate places

	gameQ		=	paste0(game, " - ", QUA, " - ", loc)
	gameGAQ		=	game
	gameGAQF	=	gameF

	if	(!multiGPU)	{
		gameGAQ		=	paste0(gameGAQ, " - ", cGPU)
		gameGAQF	=	paste0(gameGAQF, " - ", cGPU)
	}
	if	(!testAPI	&	!is.null(listAPI))	{
		gameGAQ		=	paste0(gameGAQ, " - ", unique(results$API))
		gameGAQF	=	paste0(gameGAQF, " - ", unique(results$API))
	}
	gameGAQ		=	paste0(gameGAQ, " - ", QUA, " - ", loc)
	gameGAQF	=	paste0(gameGAQF, " - ", qua, " - ", loc)

	source("@Combined - Output.r")
}	}