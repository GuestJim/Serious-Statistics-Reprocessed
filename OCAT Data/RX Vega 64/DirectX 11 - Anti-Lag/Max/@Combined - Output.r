yrates	=	c(c(120, 60, 30, 20, 15, 12, 10), yratesEXT)
yrates	=	sort(c(yrates,-yrates))
ytimes	=	sort(1000/yrates)
ybreaks	=	sort(c(round(ytimes, 2), 0))
ms2FPS	=	function(DATA, r = 0)	round(1000/DATA, r)

labelBreak	=	function(breaks, SEC = FALSE)	{
	if (!app.BREAK)	return(breaks)
	BREAK	=	c("", "\n")
	if	(is.numeric(breaks)	&	0 %in% breaks)	if	((which(breaks %in% 0) %% 2) == 0)	BREAK	=	rev(BREAK)
	if	(!SEC)	return(	paste0(rep(BREAK, length.out = length(breaks)),	breaks)	)
	if	(SEC)	return(	paste0(breaks, rep(BREAK, length.out = length(breaks)))	)
}
#	can be disabled by setting app.BREAK to FALSE

# labelRound	=	function(breaks)	sprintf("%.1f", breaks)
labelRound	=	function(breaks)	round(breaks, 1)
labelRoundB	=	function(breaks)	labelBreak(labelRound(breaks))
ms2FPS.lab	=	function(breaks)	labelBreak(ms2FPS(breaks), SEC = TRUE)
labelBreakQQ=	function(breaks)	labelBreak(paste0(pnorm(breaks) * 100, "%"))
labelDisp	=	function(breaks)	round(breaks * 60/1000, 1)
labelDispB	=	function(breaks)	labelBreak(labelDisp(breaks))


meanMS	=	function(DATA)	{
	out			=	c(mean(DATA), median(DATA))
	names(out)	=	c("Mean", "Median")
	return(out)
}

meanGEO	=	function(DATA)	{
	out			=	exp(mean(log(DATA)))
	names(out)	=	"ms"
	return(out)
}

normGEO	=	function(DATA)	{
	out			=	DATA / max(DATA) * 100
	names(out)	=	"Normalized (%)"
	return(out)
}
#	this should only be used with special AGGREGATE functions with different GROUPS than normally used
#		for example, just GROUP by GPU to compare them, or by GPU and API to compare them
#	normGEO is for normalizing the performance based on the maximum/longest frame time
#		should be used on the AGGREGATE, not within it, and will require passing the specific column to the function

percMS	=	function(DATA, listPERC = c(0.1, 1, 99, 99.9))	{
	if	(max(listPERC) > 1)		listPERC	=	listPERC/100

	out			=	quantile(DATA, listPERC)
	names(out)	=	paste0(listPERC * 100, "%")
	return(out)
}

ecdfFPS	=	function(DATA, listFPS = NULL, r = 2)	{
	default		=	c(60, 50, 30, 20, 15)
	listFPS		=	unique(sort(c(default, listFPS), decreasing = TRUE))
	out			=	100 * (1 - ecdf(DATA)(1000 / listFPS))
	names(out)	=	paste0(listFPS, " FPS")

	return(round(out, r))
}

statMS	=	function(DATA, r = 2)	{
	out			=	c(mean(DATA), sd(DATA), sd(DATA)/mean(DATA) * 100, skewness(DATA), kurtosis(DATA))
	names(out)	=	c("Mean (ms)", "StDev (ms)", "CoV (%)", "Skew", "Kurtosis")
	return(round(out, r))
}

BoxPerc	=	function (DATA)	{
	out			=	quantile(DATA, c(0.001, 0.01, 0.5, 0.99, 0.999))
	names(out)	=	c("ymin", "lower", "middle", "upper", "ymax")
	return(out)
}
#	by using this with stat_summary I can have custom quantiles for the boxplot

qqslope	=	function (DATA, r = 2, quan = QUAN)	{
	y		=	quantile(DATA, quan)
	#x		=	qnorm(quan)
	x		=	100 * quan
	#	to make this be in percentile instead of Z-score
	slope	=	diff(y)/diff(x)
	return(round(slope, r))
}

statGRAPH	=	function(DATA, ...)	{
	out			=	c(mean(DATA), median(DATA), median(diff(DATA)), qqslope(DATA, ...), quantile(DATA, c(0.1, 1, 99, 99.9)/100))
	names(out)	=	c("Mean", "Median", "DiffMedian", "Slope", "0.1", "1", "99", "99.9")
	return(out)
}
#	DiffMedian can be used in graphDIFF to apply a Median-Median cross on the plots


sepCOL	=	function(aggOUT)	{
	matCOL	=	sapply(aggOUT, is.matrix)
	out	=	cbind(aggOUT[, !matCOL], as.data.frame(aggOUT[, matCOL]))
	return(out)
}

AGG	=	function(datatype, FUN, ..., COL = NULL, ITEM = NULL, DATA = results)	{
	if	(!is.null(COL) & !is.null(ITEM))	DATA	=	DATA[DATA[, COL] == ITEM, ]

	GROUPS	=	list(GPU = DATA$GPU, API = DATA$API, Quality = DATA$Quality, Location = DATA$Location)
	if	(!testAPI)	GROUPS$API		=	NULL
	if	(!testQUA)	GROUPS$Quality	=	NULL

	return(sepCOL(aggregate(DATA[, datatype], GROUPS, FUN, ...)))
}

addFPS	=	function(DATA, r = 2)	{
	numCOL	=	sapply(DATA, is.numeric)
	dataFPS		=	cbind(list("Unit" = "FPS"),	round(1000/DATA[, numCOL], 	r))
	dataMS		=	cbind(list("Unit" = "ms"),	round(DATA[, numCOL],		r))

	out	=	cbind(DATA[, !numCOL], rbind(dataFPS, dataMS))
	colnames(out)[grep("Unit", colnames(out))]	=	""
	return(out)
}

compTAB	=	function(MEAN, PERC, ECDF)	{
	if	(is.null(listFPS))	{
		listECDF	=	grep("60 FPS", colnames(ECDF))
	}	else	{
		begECDF		=	grep(paste0(max(c(listFPS, 60)), " FPS"), colnames(ECDF))
		endECDF		=	grep(paste0(min(c(listFPS, 60)), " FPS"), colnames(ECDF))

		listECDF	=	begECDF:endECDF
	}

	out	=	cbind(
		MEAN,
		PERC[, sapply(PERC, is.numeric)],
		ECDF[, listECDF]
	)

	colnames(out)[grep("Var.", colnames(out))]	=	""

	return(out)
}

dataSEL	=	function(datatype, COL = NULL, ITEM = NULL)	{
	if	(datatype == "MsBetweenPresents")		descs	=	c("Frame Time",		"data")
	if	(datatype == "MsBetweenDisplayChange")	descs	=	c("Display Time",	"disp")
	if	(datatype == "MsUntilRenderComplete")	descs	=	c("Render Time",	"rend")
	if	(datatype == "MsEstimatedDriverLag")	descs	=	c("Driver Lag",		"driv")

	type	<<-	descs[1];	typeSHORT	<<-	descs[2]

	MEAN	<<-	AGG(datatype,	meanMS,					COL = COL,	ITEM = ITEM)
	PERC	<<-	AGG(datatype,	percMS,					COL = COL,	ITEM = ITEM)
	ECDF	<<-	AGG(datatype,	ecdfFPS,	listFPS,	COL = COL,	ITEM = ITEM)
	STAT	<<-	AGG(datatype,	statMS,					COL = COL,	ITEM = ITEM)
}

dataSEL.rm	=	function()	{
	rm(type, typeSHORT, MEAN, PERC, ECDF, STAT, envir = .GlobalEnv)
}
#	used for removing the objects dataSEL makes, in case you need to clear them for troubleshooting purposes

sinkTXT	=	function(datatype, COL = NULL, ITEM = NULL)	{
	options(width = 1000)

	subSTR	=	""
	if	(!is.null(ITEM))	subSTR	=	paste0(" - ", ITEM, " - ")

	filePath	=	paste0(gameGAQF, " ", subSTR, type, ".txt")
	if	(!is.null(COL))	if	(COL	==	"GPU")	filePath	=	paste0(ITEM, "\\", filePath)

	sink(filePath, split = TRUE)

	writeLines(gameGAQ)
	writeLines(type)
	writeLines("\nMean")
	print(addFPS(MEAN), row.names = FALSE)
	writeLines("\nPercentiles")
	print(addFPS(PERC), row.names = FALSE)
	writeLines("\nPercentile of FPS")
	print(ECDF, 		row.names = FALSE)
	writeLines("\nDistribution Stats")
	print(STAT,			row.names = FALSE)
sink()
}

library(tableHTML)
OCCHTML	=	function(DATA)	{
	tableHTML(DATA, rownames = FALSE, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_header_\\d\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\\d\"', '', replace_all = TRUE)
}

writeOCC	=	function(DATA, dataNAME, name=gameGAQF, fold = "")	{
	filePath	=	paste0(name, " - ", dataNAME,".html")
	if	(fold != "")	filePath	=	paste0(fold, "\\", filePath)

	write_tableHTML(OCCHTML(DATA), file = filePath)
}

sinkHTML	=	function(datatype, COL = NULL, ITEM	= NULL)	{
	ITEM.f	=	""
	if	(!is.null(COL) & !is.null(ITEM))		ITEM.f		=	paste0(ITEM, " - ")

	FOLD	=	""
	if	(!is.null(COL)) if	(COL	==	"GPU")	FOLD	=	ITEM

	writeOCC(addFPS(MEAN),								dataNAME = paste0(ITEM.f, typeSHORT, "MEAN"),	fold = FOLD)
	writeOCC(addFPS(PERC),								dataNAME = paste0(ITEM.f, typeSHORT, "PERC"),	fold = FOLD)
	writeOCC(ECDF,										dataNAME = paste0(ITEM.f, typeSHORT, "ECDF"),	fold = FOLD)
	writeOCC(STAT,										dataNAME = paste0(ITEM.f, typeSHORT, "STAT"),	fold = FOLD)
	writeOCC(compTAB(addFPS(MEAN), addFPS(PERC), ECDF),	dataNAME = paste0(ITEM.f, typeSHORT, "COMP"),	fold = FOLD)
}

sinkOUT	=	function(datatype)	{
dataSEL(datatype)
				sinkTXT(datatype)
if	(HTMLOUT)	sinkHTML(datatype)

if	(perGPU)	{	for (GPU in listGPU)		{	if	(!file.exists(GPU))	next
	dataSEL(datatype, COL	=	"GPU", ITEM = GPU)
					sinkTXT(datatype,	COL	=	"GPU", ITEM	=	GPU)
	if	(HTMLOUT)	sinkHTML(datatype,	COL	=	"GPU", ITEM	=	GPU)
}	}

if	(textAPI)	{	for (API in listAPI)		{
	dataSEL(datatype, COL	=	"API", ITEM = API)
					sinkTXT(datatype,	COL	=	"API", ITEM	=	API)
	if	(HTMLOUT)	sinkHTML(datatype,	COL	=	"API", ITEM	=	API)
}	}

if	(textLOC)	{	for (Location in listLOC)	{
	dataSEL(datatype, COL	=	"Location", ITEM = Location)
					sinkTXT(datatype,	COL	=	"Location", ITEM	=	Location)
	if	(HTMLOUT)	sinkHTML(datatype,	COL	=	"Location", ITEM	=	Location)
}	}
}


customSave	=	function(type="", plot = last_plot(), device=ggdevice, width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if	(device	==	"png"	|	device == "both")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".png"), plot = plot, device="png", width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf"	|	device == "both")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".pdf"), plot = plot, device="pdf", width=width, height=height)
	}
}

graphOUT	=	function(datatype, graphtype, OUT = TRUE, SHOW = FALSE, diffLim = NULL, ...)	{
	if	(datatype == "MsBetweenPresents")			dataNAME	=	"Frame Time"
	if	(datatype == "MsBetweenDisplayChange")		dataNAME	=	"Display Time"
	if	(datatype == "MsUntilRenderComplete")		dataNAME	=	"Render Time"
	if	(datatype == "MsEstimatedDriverLag")		dataNAME	=	"Driver Lag"

	if	(substitute(graphtype) == "graphMEANS")		graphNAME	=	"Means"
	if	(substitute(graphtype) == "graphCOURSE")	graphNAME	=	"Course"
	if	(substitute(graphtype) == "graphFREQ")		graphNAME	=	"Freq"
	if	(substitute(graphtype) == "graphQQ")		graphNAME	=	"QQ"
	if	(substitute(graphtype) == "graphDIFF")		graphNAME	=	"Diff"

	if	(substitute(graphtype) == "graphMEANSbox")	graphNAME	=	"Means Labeled"

	PLOT	=	graphtype(datatype)
	if	(graphNAME == "Diff" & !is.null(diffLim))	{
		PLOT	=	graphtype(datatype, diffLim)
		graphNAME	=	paste0(graphNAME, " EXT")
	}

	message(paste0(graphNAME, " - ", dataNAME))

	if	(OUT)	customSave(paste0("@", graphNAME, " - ", dataNAME), plot = PLOT, ...)
	if	(SHOW)	PLOT
}

levels.rev		=	function(DATA, COL)	{
	DATA	=	as.data.frame(DATA)
	ordered(DATA[, COL],	levels = rev(levels(DATA[, COL])))
}

levels.short	=	function(DATA, COL, LIST, LEVS)	{
	DATA	=	as.data.frame(DATA)
	DATA[, COL]	=	ordered(DATA[, COL],	levels	=	LIST)
	levels(DATA[, COL])	=	LEVS
	return(DATA[, COL])
}

data.short	=	function(DATA)	{
	if	(!is.null(shortLOC))				DATA[, "Location"]	=	levels.short(DATA,	"Location",	listLOC,	levsLOC)
	if	(!is.null(shortAPI)	&	testAPI)	DATA[, "API"]		=	levels.short(DATA,	"API",		listAPI,	levsAPI)
	return(DATA)
}

graph.rev	=	function(DATA, rev.LOC = FALSE, rev.API = FALSE)	{
	if (rev.LOC)				DATA$Location	=	levels.rev(DATA, "Location")
	if (rev.API	&	testAPI)	DATA$API		=	levels.rev(DATA, "API")
	return(DATA)
}

#	spacing between facet panels can be set with  theme(panel.spacing.x = unit(1, "lines"))

FACET = function(graphtype)	{
	if	(any(substitute(graphtype)	==	c("graphMEANS")))	{
		if	(testAPI	&	!testQUA)	return(facet_grid(rows = vars(API),				cols = vars(Location), switch = "y"))
		if	(!testAPI	&	testQUA)	return(facet_grid(rows = vars(Quality),			cols = vars(Location), switch = "y"))
		if	(testAPI	&	testQUA)	return(facet_grid(rows = vars(API, Quality),	cols = vars(Location), switch = "y"))

		return(facet_grid(cols = vars(Location), switch = "y"))
	}

	if	(any(substitute(graphtype)	==	c("graphCOURSE", "graphFREQ", "graphQQ", "graphDIFF")))	{
		if	(multiGPU)	{
			if	(testAPI	&	!testQUA)	return(facet_grid(rows = vars(Location, API),			cols = vars(GPU), switch = "y"))
			if	(!testAPI	&	testQUA)	return(facet_grid(rows = vars(Location, Quality),		cols = vars(GPU), switch = "y"))
			if	(testAPI	&	testQUA)	return(facet_grid(rows = vars(Location, API, Quality),	cols = vars(GPU), switch = "y"))
		}	else	{
			if	(testAPI	&	!testQUA)	return(facet_grid(rows = vars(API),				cols = vars(Location, GPU), switch = "y"))
			if	(!testAPI	&	testQUA)	return(facet_grid(rows = vars(Quality),			cols = vars(Location, GPU), switch = "y"))
			if	(testAPI	&	testQUA)	return(facet_grid(rows = vars(API, Quality),	cols = vars(Location, GPU), switch = "y"))
		}

		return(facet_grid(rows = vars(Location), cols = vars(GPU), switch = "y"))
	}
}

graphMEANS	=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
			breaks		=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
			breaks		=	ybreaks,	labels	=	labelDisp,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Display Time (ms)",
				labels	=	ybreaks
			)
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
			breaks		=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
	if	(datatype == "MsEstimatedDriverLag")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Estimated Driver Lag (ms)",
			breaks		=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}

	# if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	# if (useSHORT)	STATS	=	data.short(STATS)	; STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)

	ggplot(data = results, aes(x = GPU, y = get(datatype))) +
	ggtitle(gameQ, subtitle = paste0(datatype, " - Means, Medians, and Percentiles")) + labsGPU +
	geom_hline(yintercept = 1000/60, color = "red") +
	# geom_boxplot(outlier.alpha = 0) +
	stat_summary(fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
	geom_bar(aes(fill = GPU), stat = "summary", fun.y = mean) + scale_fill_hue() +
	stat_summary(fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
	# geom_boxplot(alpha = 0.50, outlier.alpha = 0.1) +
	FACET(graphMEANS) +
	scale_x_discrete(labels = labelBreak) +
	scale_Y + coord_cartesian(ylim = c(0, FtimeLimit)) +
	guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
}

boxLABS		=	function(datatype)	{
	STATS	=	AGG(datatype, statGRAPH)

	ALPHA	=	0.65

	nudOUT	=	0.50
	nudIN	=	0.40
	nudMED	=	0.55

	list(
		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "99.9"],	label = round(STATS[, "99.9"], 2)),		alpha = ALPHA,
											vjust = 0,	nudge_y = nudOUT),
		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "0.1"],	label = round(STATS[, "0.1"], 2)),		alpha = ALPHA,
											vjust = 1,	nudge_y = -nudOUT),
		#	0.1% and 99.9%

		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "99"],		label = round(STATS[, "99"], 2)),		alpha = ALPHA,
			hjust = 1,	nudge_x = nudIN,	vjust = 0),
		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "1"],		label = round(STATS[, "1"], 2)),		alpha = ALPHA,
			hjust = 1,	nudge_x = nudIN,	vjust = 1),
		#	1% and 99%

		geom_label(data = STATS,	aes(x = GPU, y = Median,			label = round(Median, 2)),				alpha = ALPHA,
			hjust = 1,	nudge_x = nudMED),
		geom_text(data = STATS,		aes(x = GPU, y = Mean, 				label = round(Mean, 2)),
			hjust = 0,	nudge_x = -0.55,	vjust = 0)
		#	median and mean
		)
}

graphMEANSbox	=	function(datatype)	graphMEANS(datatype) + boxLABS(datatype)

graphCOURSE	=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
			breaks		=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
			breaks		=	ybreaks,	labels	=	labelDisp,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Display Time (ms)",
				labels	=	ybreaks
			)
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
			breaks		=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
	if	(datatype == "MsEstimatedDriverLag")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Estimated Driver Lag (ms)",
			breaks		=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}

	if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	# if (useSHORT)	STATS	=	data.short(STATS)	;	STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)

	ALPHA	=	0.05
	if	(length(unique(results$Location)) == 1)	ALPHA	=	1

	ggplot(data = results, aes(x = TimeInSeconds, y = get(datatype))) +
	ggtitle(gameQ, subtitle = paste0(datatype, " - Course")) + labsGPU +
	geom_hline(yintercept = 1000/60, color = "red") +
	geom_point(alpha = ALPHA) +
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) +
	FACET(graphCOURSE) +
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=max(results$TimeInSeconds), by=60), labels = labelBreak, expand=c(0.02, 0)) +
	scale_Y + coord_cartesian(ylim = c(0, FtimeLimit))
}


graphFREQ	=	function(datatype)	{
	STATS	=	AGG(datatype, statGRAPH)
	if	(datatype == "MsBetweenPresents")	{
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS.lab
			)
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	ybreaks,	labels	=	labelDispB,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Display Time (ms)",
				labels	=	ybreaks
			)
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS.lab
			)
		)
	}
	if	(datatype == "MsEstimatedDriverLag")	{
		scale_X	=	scale_x_continuous(
			name	=	"Estimated Driver Lag (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}

	if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	if (useSHORT)	STATS	=	data.short(STATS)	;	STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)

	ggplot(results, aes(get(x = datatype))) +
	ggtitle(gameQ, subtitle=paste0(datatype, " - Frequency Plot")) + labsGPU +
	geom_vline(xintercept = 1000/60, color = "red") +
	geom_freqpoly(binwidth=0.03, size=0) +
		geom_vline(data = STATS, aes(xintercept = Mean), color = "darkgreen") +
		geom_vline(data = STATS, aes(xintercept = Median), color = "darkcyan", linetype="dotdash") +
	FACET(graphFREQ) +
	scale_X + coord_cartesian(xlim = c(0, FtimeLimit)) +
	scale_y_continuous(name="Count", expand=c(0.02, 0))
}

graphQQ	=	function(datatype, PERCS = c(.001, .01, .5, .99, .999))	{
	PERCS	=	sort(unique(c(PERCS, QUAN)))
	STATS	=	AGG(datatype, statGRAPH)
	if	(datatype == "MsBetweenPresents")	{
		scale_Y	=	scale_y_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_Y	=	scale_y_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	ybreaks,	labels	=	labelDisp,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Display Time (ms)",
				labels	=	ybreaks
			)
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_Y	=	scale_y_continuous(
			name	=	"Render Time (ms)",
			breaks	=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
	if	(datatype == "MsEstimatedDriverLag")	{
		scale_Y	=	scale_y_continuous(
			name	=	"Estimated Driver Lag (ms)",
			breaks	=	ybreaks,	labels	=	labelRound,	expand	=	c(0.02, 0)
		)
	}

	if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	if (useSHORT)	STATS	=	data.short(STATS)	;	STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)

#	sec.axis	=	sec_axis(~.,
#		breaks	=	STATS[c("0.1", "1", "Median", "99", "99.9")],
#		labels	=	paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
#	)
#		this can be used to add a secondary axis that shows the values for the percentiles
#			code remains for reference as now Rates are shown for the second axis

	ggplot(data = STATS, aes(ymin = -Inf, xmin = -Inf)) +
	ggtitle(gameQ, subtitle = paste0(datatype, " - QQ Distribution")) + labsGPU +
	geom_hline(yintercept = 1000/60, color	=	"red") +
		geom_rect(aes(ymax = get("0.1"),	xmax = qnorm(.001)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = get("1"),		xmax = qnorm(.010)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = get("Median"),	xmax = qnorm(.500)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = get("99"),		xmax = qnorm(.990)), alpha=0.1, fill=c("red"), color = "grey") +
		geom_rect(aes(ymax = get("99.9"),	xmax = qnorm(.999)), alpha=0.1, fill=c("red"), color = "grey") +
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", size = 1.1, linetype = "dotted") +
	stat_qq(data = results, aes(sample=get(datatype))) +
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", alpha = 0.5, size = 1.1, linetype = "dotted") +
	geom_label(data = STATS, aes(x = Inf, y = -Inf, label = paste0("Slope: ", Slope)), parse = TRUE, hjust="right", vjust="bottom", fill = "darkgrey", color = "green") +
	FACET(graphQQ) +
	scale_Y + coord_cartesian(ylim = c(0, FtimeLimit)) +
	scale_x_continuous(name = "Percentile", breaks = qnorm(PERCS), labels = labelBreakQQ, minor_breaks = NULL, expand = c(0.02, 0))
}

graphDIFF	=	function(datatype, diffLim = 1000/50)	{
	if	(datatype == "MsBetweenPresents")	{
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Frame Time Difference (ms)",
			breaks	=	ybreaks,	labels	=	labelRound,		limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	ybreaks,	labels	=	labelDisp,		limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Display Time Difference (ms)",
			breaks	=	ybreaks,
			limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Render Time Difference (ms)",
			breaks	=	ybreaks,	labels	=	labelRound,		limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
	if	(datatype == "MsEstimatedDriverLag")	{
		scale_X	=	scale_x_continuous(
			name	=	"Estimated Driver Lag (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Lag Difference (ms)",
			breaks	=	ybreaks,	labels	=	labelRound,		limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}

	if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	# if (useSHORT)	STATS	=	data.short(STATS)	;	STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)

	# ggplot(data = results, aes(x = get(datatype), y = c(diff(as.data.frame(results)[, datatype]), 0)) ) +
	ggplot(data = results, aes(x = get(datatype), y = diff.CONS(get(datatype))) ) +
	ggtitle(gameQ, subtitle=paste0(datatype, " Consecutive Differences")) + labsGPU +
	geom_point(alpha = 0.1) +
	stat_density_2d(geom = "polygon", aes(fill = stat(nlevel)), show.legend = FALSE) + scale_fill_viridis_c() +
	# stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
	FACET(graphDIFF) +
	scale_X +
	scale_Y
}
#	using coord_cartesian to apply limits breaks the heatmap for some reason

#	text outputs
if	(textOUT)	{
	if	(textFRAM)	sinkOUT("MsBetweenPresents")
	if	(textDISP)	sinkOUT("MsBetweenDisplayChange")
	if	(textREND)	sinkOUT("MsUntilRenderComplete")
	if	(textDRIV)	sinkOUT("MsEstimatedDriverLag")
	message("")
}

if	(graphs)	{
rev.LOC	=	FALSE	;	rev.API	=	TRUE

#Means
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphMEANS)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphMEANS)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphMEANS)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphMEANS)

#Means with Boxplot Lables
#				graphOUT("MsBetweenPresents",		graphMEANSbox)

rev.LOC	=	TRUE	;	rev.API	=	TRUE

#Course
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphCOURSE)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphCOURSE)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphCOURSE)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphCOURSE)

#Frequency
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphFREQ)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphFREQ)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphFREQ)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphFREQ)

#QQ
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphQQ)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphQQ)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphQQ)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphQQ)

#Difference
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphDIFF)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphDIFF)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphDIFF)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphDIFF)

#Difference - Extended
if (!is.null(diffLim))	{
	if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphDIFF,	diffLim = diffLim)
	if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphDIFF,	diffLim = diffLim)
	if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphDIFF,	diffLim = diffLim)
	if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphDIFF,	diffLim = diffLim)
}
}