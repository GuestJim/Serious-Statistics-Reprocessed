yrates	=	c(120, 60, 30, 20, 15, 12, 10)
yrates	=	sort(c(yrates,-yrates))
ytimes	=	1000/yrates

# labelRound	=	function(x)	sprintf("%.1f", x)
labelRound	=	function(x)			round(x, 1)
labelBreak	=	function(input)		paste0(rep(c("", "\n"), length.out = length(input)), input)
labelDisp	=	function(breaks)	round(breaks * 60/1000, 1)

BoxPerc	=	function (DATA)	{
	out			=	quantile(DATA, c(0.001, 0.01, 0.5, 0.99, 0.999))
	names(out)	=	c("ymin", "lower", "middle", "upper", "ymax")
	return(out)
}
#	by using this with stat_summary I can have custom quantiles for the boxplot

meanMS	=	function(DATA)	{
	out			=	c(mean(DATA), median(DATA))
	names(out)	=	c("Mean", "Median")
	return(out)
}
#	custom function to return both the arithmetic mean and the median of the data

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
#	this should only be used with special AGGREGATE functions with different GROUPS than we normally use
#		for example, just GROUP by GPU to compare them, or by GPU and API to compare them
#	normGEO is for normalizing the performance based on the maximum/longest frame time
#		should be used on the AGGREGATE, not within it, and will require passing the specific column to the function

percMS	=	function(DATA, listPERC = c(0.1, 1, 99, 99.9))	{
	if	(max(listPERC) > 1)	listPERC = listPERC/100
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

qqslope	=	function (DATA, r = 2, quan = QUAN)	{
	y		=	quantile(DATA, quan)
	x		=	qnorm(quan)
	slope	=	diff(y)/diff(x)
	return(round(slope, r))
}

statGRAPH	=	function(DATA, r = 2, quan = QUAN)	{
	out	=	c(mean(DATA), median(DATA), qqslope(DATA, quan = quan), quantile(DATA, c(0.1, 1, 99, 99.9)/100))
	names(out)	=	c("Mean", "Median", "Slope", "0.1", "1", "99", "99.9")
	return(out)
}

sepCOL	=	function(tab)	{
	out	=	as.data.frame(as.matrix(tab))
	for (i in grep("x", names(out)))	{
		out[, i]	=	as.numeric(as.character(out[, i]))
	}
	colnames(out)	=	gsub("x.", "", colnames(out))
	return(out)
}

addFPS	=	function(DATA, r = 2)	{
	lab	=	DATA[1:grep("Location", colnames(DATA))]
	val	=	DATA[-(1:grep("Location", colnames(DATA)))]

	tFPS	=	cbind(lab, rep("FPS", nrow(DATA)), round(1000/val, r))
	names(tFPS)[ncol(lab) + 1]	=	""
	tMS		=	cbind(lab, rep("ms", nrow(DATA)), round(val, r))
	names(tMS)[ncol(lab) + 1]	=	""

	out	=	rbind(tFPS, tMS)
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
		addFPS(MEAN),
		addFPS(PERC)[-(1:grep("0.1%", colnames(addFPS(PERC))) - 1)],
		ECDF[listECDF]
	)

	return(out)
}

customSave	=	function(type="", plot = last_plot(), device=ggdevice, width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if	(device	==	"png")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".png"), plot = plot, device=device, width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".pdf"), plot = plot, device=device, width=width, height=height)
	}
}
#	it is possible to directly save a plot without having to render it first by using the plot argument
#		customSave("@Means - Frame Time", plot = graphMEANS("MsBetweenPresents"))

if	(testAPI)	{
	GROUPS	=	list(GPU = results$GPU, API = results$API, Location = results$Location)
}	else	{
	GROUPS	=	list(GPU = results$GPU, Location = results$Location)
}

if	(textFRAM	|	graphFRAM)	{
	dataMEAN	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, meanMS))
	dataPERC	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, percMS))
	dataECDF	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, ecdfFPS, listFPS))
	dataSTAT	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statMS))
	graphSTATS	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statGRAPH))
	graphSTATS$GPU	=	factor(graphSTATS$GPU, levels = listGPU, ordered = TRUE)
}
if	(textDISP	|	graphDISP)	{
	dispMEAN	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, meanMS))
	dispPERC	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, percMS))
	dispECDF	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, ecdfFPS, listFPS))
	dispSTAT	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statMS))
	dispgSTATS	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statGRAPH))
	dispgSTATS$GPU	=	factor(dispgSTATS$GPU, levels = listGPU, ordered = TRUE)
}
if	(textREND	|	graphREND)	{
	rendMEAN	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, meanMS))
	rendPERC	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, percMS))
	rendECDF	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, ecdfFPS, listFPS))
	rendSTAT	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, statMS))
	rendgSTATS	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, statGRAPH))
	rendgSTATS$GPU	=	factor(rendgSTATS$GPU, levels = listGPU, ordered = TRUE)
}

#	it is worth noting that using a list when passing the data to aggregate allows you to set the name of the output column
#		aggregate(list(Hello = data, groups, function)) will label the column Hello
#	it is also possible to run the function on more columns by placing them all in a list (not a vector, but a list like GROUPS)

subOUT	=	function(DATA, COL = "")	{
	if	(COL == "")	{
		out	=	DATA
	}	else	{
		SUB	=	eval(parse(text = COL))
		out	=	DATA[DATA[, COL] == SUB, ]
	}
	return(out)
}

dataSEL	=	function(datatype, COL = "")	{
	if	(datatype == "MsBetweenPresents"		|	datatype == "Frame Time")	{
		type		<<-	"Frame Time"
		typeSHORT	<<-	"data"
		MEAN		<<-	subOUT(dataMEAN, COL)
		PERC		<<-	subOUT(dataPERC, COL)
		ECDF		<<-	subOUT(dataECDF, COL)
		STAT		<<-	subOUT(dataSTAT, COL)
	}
	if	(datatype == "MsBetweenDisplayChange"	|	datatype == "Display Time")	{
		type		<<-	"Display Time"
		typeSHORT	<<-	"disp"
		MEAN		<<-	subOUT(dispMEAN, COL)
		PERC		<<-	subOUT(dispPERC, COL)
		ECDF		<<-	subOUT(dispECDF, COL)
		STAT		<<-	subOUT(dispSTAT, COL)
	}
	if	(datatype == "MsUntilRenderComplete"	|	datatype == "Render Time")	{
		type		<<-	"Render Time"
		typeSHORT	<<-	"rend"
		MEAN		<<-	subOUT(rendMEAN, COL)
		PERC		<<-	subOUT(rendPERC, COL)
		ECDF		<<-	subOUT(rendECDF, COL)
		STAT		<<-	subOUT(rendSTAT, COL)
	}
}

sinkTXT	=	function(datatype, COL = "")	{
	options(width = 1000)

	dataSEL(datatype, COL)

	if	(COL == "")	{
		subSTR	=	""
	}	else	{
		SUB		=	eval(parse(text = COL))
		subSTR	=	paste0(" - ", SUB, " - ")
	}

	if	(COL	==	"GPU")	{
		sink(paste0(SUB, "\\", gameGAQF, " ", subSTR, type, ".txt"), split = TRUE)
	}	else	{
		sink(paste0(gameGAQF, " ", subSTR, type, ".txt"), split = TRUE)
	}

	writeLines(gameGAQ)
	writeLines(type)
	writeLines("\nMean")
	print(addFPS(MEAN), row.names = FALSE)
	writeLines("\nPercentiles")
	print(addFPS(PERC), row.names = FALSE)
	writeLines("\nPercentile of FPS")
	print(ECDF, row.names = FALSE)
	writeLines("\nDistribution Stats")
	print(STAT, row.names = FALSE)
sink()
}

library(tableHTML)
OCCHTML	=	function(DATA)	{
	tableHTML(DATA, rownames = FALSE, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
}

writeOCC	=	function(DATA, dataNAME, name=gameGAQF, fold = FOLD)	{
	if	(fold != "")	{
		write_tableHTML(OCCHTML(DATA), file = paste0(fold, "\\", name, " - ", dataNAME,".html"))
	}	else	{
		write_tableHTML(OCCHTML(DATA), file = paste0(name, " - ", dataNAME,".html"))
	}
}

sinkHTML	=	function(datatype, COL = "")	{
	dataSEL(datatype, COL)

	if	(COL	==	"")	{
		SUB	=	""
	}	else	{
		SUB	=	paste0(eval(parse(text = COL)), " - ")
	}

	if	(COL	==	"GPU")	{
		FOLD	<<-	eval(parse(text = COL))
	}	else	{
		FOLD	<<-	""
	}

	writeOCC(addFPS(MEAN),				dataNAME = paste0(SUB, typeSHORT, "MEAN"))
	writeOCC(addFPS(PERC),				dataNAME = paste0(SUB, typeSHORT, "PERC"))
	writeOCC(ECDF,						dataNAME = paste0(SUB, typeSHORT, "ECDF"))
	writeOCC(STAT,						dataNAME = paste0(SUB, typeSHORT, "STAT"))
	writeOCC(compTAB(MEAN, PERC, ECDF),	dataNAME = paste0(SUB, typeSHORT, "COMP"))
}

sinkOUT	=	function(datatype)	{
if	(textOUT)	sinkTXT(datatype)
if	(HTMLOUT)	sinkHTML(datatype)

for (GPU in listGPU)	{	if	(file.exists(GPU))	{	GPU	<<-	GPU
	if	(textOUT)	sinkTXT(datatype, "GPU")
	if	(HTMLOUT)	sinkHTML(datatype, "GPU")
}	}

if	(testAPI)			{	for (API in listAPI)	{	API	<<-	API
	if	(textOUT)	sinkTXT(datatype, "API")
	if	(HTMLOUT)	sinkHTML(datatype, "API")
}	}
}

if	(textFRAM)	sinkOUT("MsBetweenPresents")
if	(textDISP)	sinkOUT("MsBetweenDisplayChange")
if	(textREND)	sinkOUT("MsUntilRenderComplete")
message("")


if	(multiGPU)	{
	labsGPU	=	labs()
}	else	{
	labsGPU	=	labs(caption = cGPU)
}

#	spacing between facet panels can be set with  theme(panel.spacing.x = unit(1, "lines"))

graphMEANS	=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
			breaks		=	c(0, round(ytimes, 2)),
			labels		=	labelDisp,
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}

	ggplot(data = results, aes(x = GPU, y = get(datatype))) +
	ggtitle(gameQ, subtitle = paste0(datatype, " - Means, Medians, and Percentiles")) + labsGPU +
	geom_hline(yintercept = 1000/60, color = "red") +
	# geom_boxplot(outlier.alpha = 0) +
	stat_summary(fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
	geom_bar(aes(fill = GPU), stat = "summary", fun.y = "mean") + scale_fill_hue() +
	stat_summary(fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
	# geom_boxplot(alpha = 0.50, outlier.alpha = 0.1) +
	facet_grid(rows = vars(API), cols = vars(Location), switch = "y") +
	scale_x_discrete(labels = labelBreak) +
	scale_Y +
	guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
}

graphCOURSE	=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
			breaks		=	c(0, round(ytimes, 2)),
			labels		=	labelDisp,
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
	}

	if	(length(unique(results$Location)) == 1)	{
		ALPHA	=	1
	}	else	{
		ALPHA	=	0.05
	}

	ggplot(data = results, aes(x = TimeInSeconds, y = get(datatype))) +
	ggtitle(gameQ, subtitle = paste0(datatype, " - Course")) + labsGPU +
	geom_hline(yintercept = 1000/60, color = "red") +
	geom_point(alpha = ALPHA) +
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) +
	facet_grid(rows = vars(Location, API), cols = vars(GPU), switch = "y") +
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=max(results$TimeInSeconds), by=60), labels = labelBreak, expand=c(0.02, 0)) +
	scale_Y
}

graphDIFF	=	function(datatype, diffLim = 1000/50)	{
	if	(datatype == "MsBetweenPresents")	{
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Frame Time Difference (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelDisp,
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Display Time Difference (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Render Time Difference (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}

	temp	=	eval(parse(text = paste0("results$", datatype)))
	#	this is to grab the desired column from the data frame
	#	the [1,] is needed because it otherwise just gets the list of row names
	#		cannot use the subsetting method because it has the wrong data type

	ggplot(data = results, aes(x = get(datatype), y = rbind(c(diff(temp), 0))[1,]) ) +
	ggtitle(gameQ, subtitle=paste0(datatype, " Consecutive Differences")) + labsGPU +
	geom_point(alpha = 0.1) +
	stat_density_2d(geom = "polygon", aes(fill = stat(nlevel)), show.legend = FALSE) + scale_fill_viridis_c() +
	# stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
	facet_grid(rows = vars(Location, API), cols = vars(GPU), switch = "y") +
	scale_X +
	scale_Y
}

graphFREQ	=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		STATS	=	graphSTATS
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			limits	=	c(0,  FtimeLimit),
			expand	=	c(0.02, 0)
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		STATS	=	dispgSTATS
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelDisp,
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		STATS	=	rendgSTATS
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			limits	=	c(0,  FtimeLimit),
			expand	=	c(0.02, 0)
		)
	}
	STATS$GPU	=	factor(STATS$GPU, levels = listGPU, ordered = TRUE)

	ggplot(results, aes(get(datatype))) +
	ggtitle(gameQ, subtitle=paste0(datatype, " - Frequency Plot")) + labsGPU +
	geom_vline(xintercept = 1000/60, color = "red") +
	geom_freqpoly(binwidth=0.03, size=0) +
		geom_vline(data = STATS, aes(xintercept = Mean), color = "darkgreen") +
		geom_vline(data = STATS, aes(xintercept = Median), color = "darkcyan", linetype="dotted") +
	facet_grid(rows = vars(Location, API), cols = vars(GPU), switch = "y") +
	scale_X +
	scale_y_continuous(name="Count", expand=c(0.02, 0))
}

graphQQ	=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		STATS	=	graphSTATS
		scale_Y	=	scale_y_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			expand	=	c(0.02, 0),
			# sec.axis	=	sec_axis(~.,
				# breaks	=	STATS[c("0.1", "1", "Median", "99", "99.9")],
				# labels	=	paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
				# )
			#	this is to add a secondary axis with the percentile values labeled
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		STATS	=	dispgSTATS
		scale_Y	=	scale_y_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelDisp,
			expand	=	c(0.02, 0),
			# sec.axis	=	sec_axis(~.,
				# breaks	=	STATS[c("0.1", "1", "Median", "99", "99.9")],
				# labels	=	paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
			# )
			#	this is to add a secondary axis with the percentile values labeled
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
		STATS	=	rendgSTATS
		scale_Y	=	scale_y_continuous(
			name	=	"Render Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			expand	=	c(0.02, 0),
			# sec.axis	=	sec_axis(~.,
				# breaks	=	STATS[c("0.1", "1", "Median", "99", "99.9")],
				# labels	=	paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
				# )
			#	this is to add a secondary axis with the percentile values labeled
		)
	}
	STATS$GPU	=	factor(STATS$GPU, levels = listGPU, ordered = TRUE)

	ggplot(data = STATS, aes(ymin = -Inf, xmin = -Inf)) +
	ggtitle(gameQ, subtitle = paste0(datatype, " - QQ Distribution")) + labsGPU +
	geom_hline(yintercept = 1000/60, color	=	"red") +
		geom_rect(aes(ymax = STATS[, "0.1"],	xmax = qnorm(.001)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = STATS[, "1"],		xmax = qnorm(.010)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = STATS[, "Median"],	xmax = qnorm(.500)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = STATS[, "99"],		xmax = qnorm(.990)), alpha=0.1, fill=c("red"), color = "grey") +
		geom_rect(aes(ymax = STATS[, "99.9"],	xmax = qnorm(.999)), alpha=0.1, fill=c("red"), color = "grey") +
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", size = 1.1, linetype = "dotted") +
	stat_qq(data = results, aes(sample=get(datatype))) +
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", alpha = 0.5, size = 1.1, linetype = "dotted") +
	geom_label(data = STATS, aes(x = Inf, y = -Inf, label = paste0("Slope: ", Slope)), parse = TRUE, hjust="right", vjust="bottom", fill = "darkgrey", color = "green") +
	facet_grid(rows = vars(Location, API), cols = vars(GPU), switch = "y") +
	scale_Y + coord_cartesian(ylim = c(0, FtimeLimit)) +
	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=labelBreak(c("0.1", "1", "50", "99", "99.9")), minor_breaks=NULL, expand=c(0.02, 0))
}


results$API	=	factor(results$API, levels = rev(listAPI))


#Means
if	(graphFRAM)	{
message("Means - Frame Time")

graphMEANS("MsBetweenPresents")
customSave("@Means - Frame Time")
}
if	(graphDISP)	{
message("Means - Display Time")

graphMEANS("MsBetweenDisplayChange")
customSave("@Means - Display Time")
}
if	(graphREND)	{
message("Means - Render Time")

graphMEANS("MsUntilRenderComplete")
customSave("@Means - Rend Time")
}

if	(useSHORT)	{
results		=	reLoc(results, shortLOC)
results		=	reAPI(results, shortAPI)

if	(graphFRAM)	{
	graphSTATS	=	reLoc(graphSTATS, shortLOC)
	graphSTATS	=	reAPI(graphSTATS, shortAPI)
}
if	(graphDISP)	{
	dispgSTATS	=	reLoc(dispgSTATS, shortLOC)
	dispgSTATS	=	reAPI(dispgSTATS, shortAPI)
}
if	(graphREND)	{
	rendgSTATS	=	reLoc(rendgSTATS, shortLOC)
	rendgSTATS	=	reAPI(rendgSTATS, shortAPI)
}	}

results$Location	=	factor(results$Location, levels = rev(levels(results$Location)))

if	(graphFRAM)	{
	graphSTATS$Location	=	factor(graphSTATS$Location, levels = rev(levels(graphSTATS$Location)))
}
if	(graphDISP)	{
	dispgSTATS$Location	=	factor(dispgSTATS$Location, levels = rev(levels(dispgSTATS$Location)))
}
if	(graphREND)	{
	rendgSTATS$Location	=	factor(rendgSTATS$Location, levels = rev(levels(rendgSTATS$Location)))
}
#	reverses the levels so they go in the order I want

#Course
if	(graphFRAM)	{
message("Course - Frame Time")

graphCOURSE("MsBetweenPresents")
customSave("@Course - Frame Time")
}
if	(graphDISP)	{
message("Course - Display Time")

graphCOURSE("MsBetweenDisplayChange")
customSave("@Course - Display Time")
}
if	(graphREND)	{
message("Course - Render Time")

graphCOURSE("MsUntilRenderComplete")
customSave("@Course - Rend Time")
}

#Frequency
if	(graphFRAM)	{
message("Frequency - Frame Time")

graphFREQ("MsBetweenPresents")
customSave("@Freq - Frame Time")
}
if	(graphDISP)	{
message("Frequency - Display Time")

graphFREQ("MsBetweenDisplayChange")
customSave("@Freq - Display Time")
}
if	(graphREND)	{
message("Frequency - Render Time")

graphFREQ("MsUntilRenderComplete")
customSave("@Freq - Rend Time")
}

#QQ
if	(graphFRAM)	{
message("QQ - Frame Time")

graphQQ("MsBetweenPresents")
customSave("@QQ - Frame Time")
}
if	(graphDISP)	{
message("QQ - Display Time")

graphQQ("MsBetweenDisplayChange")
customSave("@QQ - Display Time")
}
if	(graphREND)	{
message("QQ - Render Time")

graphQQ("MsUntilRenderComplete")
customSave("@QQ - Rend Time")
}

#Differnce
if	(graphFRAM)	{
message("Diff - Frame Time")

graphDIFF("MsBetweenPresents")
customSave("@Diff - Frame Time")
}
if	(graphDISP)	{
message("Diff - Display Time")

graphDIFF("MsBetweenDisplayChange")
customSave("@Diff - Display Time")
}
if	(graphREND)	{
message("Diff - Render Time")

graphDIFF("MsUntilRenderComplete")
customSave("@Diff - Rend Time")
}