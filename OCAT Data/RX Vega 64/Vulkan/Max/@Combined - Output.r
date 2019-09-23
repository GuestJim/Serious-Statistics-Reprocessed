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

meanMS	=	function(x)	{
	out			=	c(mean(x), median(x))
	names(out)	=	c("Mean", "Median")
	return(out)
}

# meanMS	=	function(x)	{
	# out	=	as.list(mean(x))
	# names(out) <- "Mean"
	# return(out)
# }
#	if I use this, then I must use
#		aggregate(list(Mean	=	results$MsBetweenPresents), GROUPS, meanMS)
#	to get the proper naming

meanGEO	=	function(x)	{
	out			=	c(1000/exp(mean(log(x))), exp(mean(log(x))))
	names(out)	=	c("FPS", "ms")
	return(round(out, r))
}

percMS	=	function(x, listPERC = c(0.1, 1, 99, 99.9))	{
	if	(max(listPERC) > 1) listPERC = listPERC/100
	out	=	c()
	for (i in listPERC)	{
		temp		=	quantile(x, i)
		names(temp)	=	paste0(i * 100, "%")
		out			=	append(out, temp)
	}

	return(out)
}

ecdfFPS	=	function(x, listFPS=NULL, r = 2)	{
	listFPS		=	unique(sort(append(c(60, 50, 30, 20, 15), listFPS), decreasing = TRUE))
	out			=	100 * (1 - ecdf(x)(1000 / listFPS))
	names(out)	=	paste0(listFPS, " FPS")

	return(round(out, r))
}

statMS	=	function(x, r = 2)	{
	out			=	c(mean(x), sd(x), sd(x)/mean(x) * 100, skewness(x), kurtosis(x))
	names(out)	=	c("Mean (ms)", "StDev (ms)", "CoV (%)", "Skew", "Kurtosis")
	return(round(out, r))
}

qqslope	=	function (data, r = 2, quan = QUAN)	{
	y		=	quantile(data, quan)
	x		=	qnorm(quan)
	slope	=	diff(y)/diff(x)
	return(round(slope, r))
}

statGRAPH	=	function(x, r = 2, quan = QUAN)	{
	out	=	c(mean(x), median(x), qqslope(x, quan = quan))
	for (i in c(0.1, 1, 99, 99.9)/100)	{
		temp		=	c(quantile(x, i))
		names(temp)	=	paste0(i * 100)
		out			=	append(out, temp)
	}
	names(out)	=	c("Mean", "Median", "Slope", "0.1", "1", "99", "99.9")
	return(out)
}

sepCOL	=	function(tab)	{
	out	=	as.data.frame(as.matrix(tab))
	for (i in grep("x", names(out)))	{
		out[, i]	=	as.numeric(as.character(out[, i]))
	}
	colnames(out)	=	sub("x.", "", colnames(out))
	return(out)
}

addFPS	=	function(x, r = 2)	{
	lab	=	x[1:grep("Location", colnames(x))]
	val	=	x[-(1:grep("Location", colnames(x)))]

	tFPS	=	cbind(lab, rep("FPS", nrow(x)), round(1000/val, 2))
	names(tFPS)[ncol(lab) + 1]	=	""
	tMS	=	cbind(lab, rep("ms", nrow(x)), round(val, 2))
	names(tMS)[ncol(lab) + 1]	=	""

	out	=	rbind(tFPS, tMS)
	return(out)
}

compTAB	=	function(MEAN, PERC, ECDF, endECDF = NULL)	{
	if	(is.null(endECDF) && !is.null(listFPS))	{
		endECDF	=	grep(paste0(min(listFPS), " FPS"), colnames(ECDF))
	}	else if	(is.null(endECDF) && is.null(listFPS))	{
		endECDF	=	grep("60 FPS", colnames(ECDF))
	}

	out	=	cbind(
		addFPS(MEAN),
		addFPS(PERC)[-(1:grep("0.1%", colnames(addFPS(PERC))) - 1)],
		ECDF[grep("60 FPS", colnames(ECDF)):endECDF]
	)

	colnames(out)[grep("Var", colnames(out))]	=	""
	return(out)
}

customSave	=	function(type="", device=ggdevice, width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if	(exists("recording"))	{
		if	(device=="png")	{
			ggsave(filename=paste0(gameGAQF, " - ", recording, " - ", type, ".png"), device=device, width=width, height=height, scale=scale, dpi=dpi)
		}	else if	(device=="pdf")	{
			ggsave(filename=paste0(gameGAQF, " - ", recording, " - ", type, ".pdf"), device=device, width=width, height=height, scale=scale)
		}
	}	else {
		if	(device=="png")	{
			ggsave(filename=paste0(gameGAQF, " - ", type, ".png"), device=device, width=width, height=height, dpi=dpi)
		}	else if	(device=="pdf")	{
			ggsave(filename=paste0(gameGAQF, " - ", type, ".pdf"), device=device, width=width, height=height)
		}
	}
}

if	(testAPI)	{
	GROUPS	=	list(GPU = results$GPU, API = results$API, Location = results$Location)
	# namesF	=	c("GPU", "API", "Location", "_")
	# namesD	=	c("GPU", "API", "Location", "_")
}	else	{
	GROUPS	=	list(GPU = results$GPU, Location = results$Location)
	# namesF	=	c("GPU", "Location", "_")
	# namesD	=	c("GPU", "Location", "_")
}

dataMEAN	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, meanMS))
dataPERC	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, percMS))
dataECDF	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, ecdfFPS, listFPS))
dataSTAT	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statMS))
graphSTATS	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statGRAPH))
graphSTATS$GPU	=	factor(graphSTATS$GPU, levels = listGPU, ordered = TRUE)
if	(textDISP)	{
	dispMEAN	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, meanMS))
	dispPERC	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, percMS))
	dispECDF	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, ecdfFPS, listFPS))
	dispSTAT	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statMS))
	dispgSTATS	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statGRAPH))
}
#	it is worth noting that using a list when passing the data to aggregate allows you to set the name of the output column
#		aggregate(list(Hello = data, groups, function)) will label the column Hello

if	(textFRAM)	{
options(width = 1000)
sink(paste0(gameGAQF, " Frame Data.txt"), split = TRUE)
writeLines(paste0(gameGAQ))
writeLines("Frame Time")
writeLines("\nMean")
print(addFPS(dataMEAN), row.names = FALSE)
writeLines("\nPercentiles")
print(addFPS(dataPERC), row.names = FALSE)
writeLines("\nPercentile of FPS")
print(dataECDF, row.names = FALSE)
writeLines("\nDistribution Stats")
print(dataSTAT, row.names = FALSE)
sink()

for (GPU in listGPU)	{	if	(file.exists(GPU))	{
	options(width = 1000)
	sink(paste0(GPU, "\\", game, " - ", GPU, " - ", QUA, " Frame Data.txt"), split = TRUE)
		writeLines(game)
		writeLines("Frame Time")
		writeLines("\nMean")
		print(dataMEAN[dataMEAN$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dataPERC[dataPERC$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dataECDF[dataECDF$GPU==GPU,], row.names = FALSE)
		writeLines("\nDistribution Stats")
		print(dataSTAT[dataSTAT$GPU==GPU,], row.names = FALSE)
	sink()
	}	}

if	(textAPI)	{
for (API in listAPI)	{
	options(width = 1000)
	sink(paste0(game, " - ", API, " - ", QUA, " Frame Data.txt"), split = TRUE)
		writeLines(game)
		writeLines("Frame Time")
		writeLines("\nMean")
		print(dataMEAN[dataMEAN$API==API,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dataPERC[dataPERC$API==API,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dataECDF[dataECDF$API==API,], row.names = FALSE)
		writeLines("\nDistribution Stats")
		print(dataSTAT[dataSTAT$API==API,], row.names = FALSE)
	sink()
}	}	}

if	(textDISP)	{
	options(width = 1000)
	sink(paste0(game, " - ", QUA, " Display Data.txt"), split = TRUE)
	writeLines(game)
	writeLines("Display Time")
	writeLines("\nMean")
	print(dispMEAN, row.names = FALSE)
	writeLines("\nPercentiles")
	print(dispPERC, row.names = FALSE)
	writeLines("\nPercentile of FPS")
	print(dispECDF, row.names = FALSE)
	writeLines("\nDistribution Stats")
	print(dispSTAT, row.names = FALSE)
	sink()

	for (GPU in listGPU)	{	if	(file.exists(GPU))	{
	options(width = 1000)
	sink(paste0(GPU, "\\", game, " - ", GPU, " - ", QUA, " Display Data.txt"), split = TRUE)
		writeLines(game)
		writeLines("Display Time")
		writeLines("\nMean")
		print(dispMEAN[dispMEAN$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dispPERC[dispPERC$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dispECDF[dispECDF$GPU==GPU,], row.names = FALSE)
		writeLines("\nDistribution Stats")
		print(dispSTAT[dispSTAT$GPU==GPU,], row.names = FALSE)
	sink()
	}	}

	if	(textAPI)	{
	for (API in listAPI)	{
		options(width = 1000)
		sink(paste0(game, " - ", API, " - ", QUA, " Frame Data.txt"), split = TRUE)
			writeLines(game)
			writeLines("Frame Time")
			writeLines("\nMean")
			print(dispMEAN[dispMEAN$API==API,], row.names = FALSE)
			writeLines("\nPercentiles")
			print(dispPERC[dispPERC$API==API,], row.names = FALSE)
			writeLines("\nPercentile of FPS")
			print(dispECDF[dispECDF$API==API,], row.names = FALSE)
			writeLines("\nDistribution Stats")
			print(dispSTAT[dispSTAT$API==API,], row.names = FALSE)
		sink()
}	}	}
message("")

library(tableHTML)
OCCHTML	=	function(tab)	{
	tableHTML(tab, rownames = FALSE, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
}

writeOCC	=	function(type, typeName=substitute(type), name=gameGAQF)	{
	write_tableHTML(OCCHTML(type), file = paste0(name, " - ", typeName,".html"))
}

writeGPU	=	function(type, GPU, typeName=substitute(type), name=gameGAQF)	{
	write_tableHTML(OCCHTML(type[type$GPU==GPU,]), file = paste0(GPU, "\\", name, " - ", typeName,".html"))
}

writeSUB	=	function(type, SUB, typeName=substitute(type), name=gameGAQF)	{
	COL	=	deparse(substitute(SUB))
	write_tableHTML(OCCHTML(type[type[,COL]==SUB,]), file = paste0(name, " - ", SUB, " - ", typeName,".html"))
}

if	(HTMLOUT)	{
writeOCC(addFPS(dataMEAN), typeName = "dataMEAN")
writeOCC(addFPS(dataPERC), typeName = "dataPERC")
writeOCC(dataECDF)
writeOCC(dataSTAT)
writeOCC(compTAB(dataMEAN, dataPERC, dataECDF), typeName = "dataCOMP")

if	(textAPI)	{
	for	(API in listAPI)	{
	writeSUB(dataMEAN, API)
	writeSUB(dataPERC, API)
	writeSUB(dataECDF, API)
	writeSUB(dataSTAT, API)
	writeSUB(compTAB(dataMEAN, dataPERC, dataECDF), API, typeName = "dataCOMP")
	}	}

for (GPU in listGPU)	{	if	(file.exists(GPU))	{
	writeGPU(dataMEAN, GPU)
	writeGPU(dataPERC, GPU)
	writeGPU(dataECDF, GPU)
	writeGPU(dataSTAT, GPU)
	writeGPU(compTAB(dataMEAN, dataPERC, dataECDF), GPU, typeName = "dataCOMP")
	}	}


if	(textDISP)	{
writeOCC(dispMEAN)
writeOCC(dispPERC)
writeOCC(dispECDF)
writeOCC(dispSTAT)
writeOCC(compTAB(dispMEAN, dispPERC, dispECDF), typeName = "dispCOMP")

if	(textAPI)	{
	for	(API in listAPI)	{
	writeSUB(dispMEAN, API)
	writeSUB(dispPERC, API)
	writeSUB(dispECDF, API)
	writeSUB(dispSTAT, API)
	writeSUB(compTAB(dispMEAN, dispPERC, dispECDF), API, typeName = "dispCOMP")
	}	}

for (GPU in listGPU)	{	if	(file.exists(GPU))	{
	writeGPU(dispMEAN, GPU)
	writeGPU(dispPERC, GPU)
	writeGPU(dispECDF, GPU)
	writeGPU(dispSTAT, GPU)
	writeGPU(compTAB(dispMEAN, dispPERC, dispECDF), GPU, typeName = "dispCOMP")
	}	}
}
}

if	(multiGPU)	{
	labsGPU	=	labs()
}	else	{
	labsGPU	=	labs(caption = cGPU)
}

graphMEANS	=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		scale_Y	=	scale_y_continuous(name="Frame Time (ms)",
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

	ggplot(data = results) +
	ggtitle(gameQ, subtitle = paste0(datatype, " - Means, Medians, and Percentiles")) + labsGPU +
	geom_hline(yintercept = 1000/60, color = "red") +
	# geom_boxplot(aes(x = GPU, y = get(datatype)), outlier.alpha = 0) +
	stat_summary(aes(x = GPU, y = get(datatype)), fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
	geom_bar(aes(x = GPU, y = get(datatype), fill = GPU), stat = "summary", fun.y = "mean") + scale_fill_hue() +
	stat_summary(aes(x = GPU, y = get(datatype)), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
	# geom_boxplot(aes(x = GPU, y = get(datatype)), alpha = 0.50, outlier.alpha = 0.1) +
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
	
	if	(length(unique(results$Location)) == 1)	{
		ALPHA	=	1
	}	else	{
		ALPHA	=	0.05
	}

	ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenPresents)) +
	ggtitle(gameQ, subtitle = paste0(datatype, " - Course")) + labsGPU +
	geom_hline(yintercept = 1000/60, color = "red") +
	geom_point(alpha = ALPHA) +
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) +
	facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), labels = labelBreak, expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) +
	scale_Y +
	guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
}


graphDIFF	=	function(datatype)	{
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
			limits	=	c(-1000/50, 1000/50),
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
			limits	=	c(-1000/50, 1000/50),
			expand	=	c(0, 0)
		)
	}

	temp	=	eval(parse(text = paste0("results$", datatype)))
	#	this is to grab the desired column from the data frame
	#	the [1,] is needed because it otherwise just gets the list of row names

	ggplot(data = results, aes(x = temp, y = rbind(c(diff(temp), 0))[1,]) ) +
	ggtitle(gameQ, subtitle=paste0(datatype, " consecutive differences")) + labsGPU +
	geom_point(alpha = 0.1) +
	stat_density_2d(geom = "polygon", aes(fill = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
	# stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
	facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
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

	ggplot(results, aes(get(datatype))) +
	ggtitle(gameQ, subtitle=paste0(datatype, " - Frequency Plot")) + labsGPU +
	geom_vline(xintercept = 1000/60, color = "red") +
	geom_freqpoly(binwidth=0.03, size=0) +
		geom_vline(data = STATS, aes(xintercept = Mean), color = "darkgreen") +
		geom_vline(data = STATS, aes(xintercept = Median), color = "darkcyan", linetype="dotted") +
	facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
	scale_X +
	expand_limits(x=c(1000/60, 1000/30)) +
	scale_y_continuous(name="Count", expand=c(0.02, 0)) + theme(panel.spacing.x = unit(1, "lines"))
}

graphQQ	=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		STATS	=	graphSTATS
		scale_Y	=	scale_y_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			# limits	=	c(0,  FtimeLimit),
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
			# limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0),
			# sec.axis	=	sec_axis(~., 
				# breaks	=	STATS[c("0.1", "1", "Median", "99", "99.9")],
				# labels	=	paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
			# )
			#	this is to add a secondary axis with the percentile values labeled
		)
	}
	STATS$GPU	=	factor(STATS$GPU, levels = listGPU, ordered = TRUE)

	ggplot() +
	ggtitle(gameQ, subtitle = paste0(datatype, " - QQ Distribution")) + labsGPU +
	geom_hline(yintercept = 1000/60, color	=	"red") +
		geom_rect(data = STATS, aes(ymin = -Inf, ymax = STATS[, c("0.1")],		xmin = -Inf, xmax = qnorm(c(.001))), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(data = STATS, aes(ymin = -Inf, ymax = STATS[, c("1")],		xmin = -Inf, xmax = qnorm(c(.010))), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(data = STATS, aes(ymin = -Inf, ymax = STATS[, c("Median")],	xmin = -Inf, xmax = qnorm(c(.500))), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(data = STATS, aes(ymin = -Inf, ymax = STATS[, c("99")],		xmin = -Inf, xmax = qnorm(c(.990))), alpha=0.1, fill=c("red"), color = "grey") +
		geom_rect(data = STATS, aes(ymin = -Inf, ymax = STATS[, c("99.9")],		xmin = -Inf, xmax = qnorm(c(.999))), alpha=0.1, fill=c("red"), color = "grey") +
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", size = 1.1, linetype = "dotted") +
	stat_qq(data = results, aes(sample=get(datatype))) +
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", alpha = 0.5, size = 1.1, linetype = "dotted") +
	geom_label(data = STATS, aes(x = Inf, y = -Inf, label = paste0("Slope: ", Slope)), parse = TRUE, hjust="right", vjust="bottom", fill = "darkgrey", color = "green") +
	facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
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

if	(useSHORT)	{
results		=	reLoc(results, shortLOC)
graphSTATS	=	reLoc(graphSTATS, shortLOC)
	if (any(colnames(graphSTATS) == "API"))	{
	results		=	reAPI(results, shortAPI)
	graphSTATS	=	reAPI(graphSTATS, shortAPI)
}
	if	(graphDISP)	{
	dispgSTATS	=	reLoc(dispgSTATS, shortLOC)
	dispgSTATS	=	reAPI(dispgSTATS, shortAPI)
	}
}

results$Location	=	factor(results$Location, levels = rev(levels(results$Location)))
graphSTATS$Location	=	factor(graphSTATS$Location, levels = rev(levels(graphSTATS$Location)))

if	(graphDISP)	{
	dispgSTATS$Location	=	factor(dispgSTATS$Location, levels = rev(levels(dispgSTATS$Location)))
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