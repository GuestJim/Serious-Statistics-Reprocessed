if (testAPI){
	FACETS	=	facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y")
}	else	{
	FACETS	=	facet_grid(cols = vars(GPU), rows = vars(Location), switch = "y")
}
#	not using the above, but could

if	(multiGPU)	{
	labsGPU	=	labs()
}	else	{
	labsGPU	=	labs(caption = cGPU)
}

graphMEANS = function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
			scale_Y	=	scale_y_continuous(name="Frame Time (ms)",
			breaks=c(0, round(ytimes, 2)),
			limits=c(0, FtimeLimit),
			expand=c(0.02, 0),
			sec.axis = dup_axis()
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_Y	=	scale_y_continuous(
			name="Refresh Cycles Later (1/60 s)",
			breaks=c(0, round(ytimes, 2)),
			labels = labelDisp,
			limits=c(0, FtimeLimit),
			expand=c(0.02, 0),
			sec.axis = dup_axis()
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


graphCOURSE = function(datatype)	{
	if	(datatype == "MsBetweenPresents")	{
		scale_Y	=	scale_y_continuous(
			name="Frame Time (ms)",
			breaks=c(0, round(ytimes, 2)),
			limits=c(0, FtimeLimit),
			expand=c(0.02, 0),
			sec.axis = dup_axis()
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_Y	=	scale_y_continuous(
			name="Refresh Cycles Later (1/60 s)",
			breaks=c(0, round(ytimes, 2)),
			labels = labelDisp,
			limits=c(0, FtimeLimit),
			expand=c(0.02, 0),
			sec.axis = dup_axis()
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
			name="Frame Time (ms)",
			breaks=c(0, round(ytimes, 2)),
			limits=c(0, FtimeLimit),
			expand=c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name="Consecutive Frame Time Difference (ms)",
			breaks=c(0, round(ytimes, 2)),
			limits=c(-1000/50, 1000/50),
			expand=c(0, 0)
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_X	=	scale_x_continuous(
			name="Refresh Cycles Later (1/60 s)",
			breaks=c(0, round(ytimes, 2)),
			labels = labelDisp,
			limits=c(0, FtimeLimit),
			expand=c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name="Consecutive Display Time Difference (ms)",
			breaks=c(0, round(ytimes, 2)),
			limits=c(-1000/50, 1000/50),
			expand=c(0, 0)
		)
	}

	temp = eval(parse(text = paste0("results$", datatype)))
	#	the [1,] is needed because it otherwise just gets the list of row names

	ggplot(data=results, aes(x=temp, y=rbind(c(diff(temp), 0))[1,]) ) +
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
		scale_X	=	scale_x_continuous(
			name="Frame Time (ms)",
			breaks=c(0, round(ytimes, 2)),
			labels=labelRound,
			limits = c(0,  FtimeLimit),
			expand=c(0.02, 0)
		)
		STATS = graphSTATS
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_X	=	scale_x_continuous(
			name="Refresh Cycles Later (1/60 s)",
			breaks=c(0, round(ytimes, 2)),
			labels = labelDisp,
			limits=c(0, FtimeLimit),
			expand=c(0.02, 0)
		)
		STATS = dispgSTATS
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
		STATS = graphSTATS
		scale_Y	=	scale_y_continuous(
			name="Frame Time (ms)",
			breaks=c(0, round(ytimes, 2)),
			labels=labelRound,
			limits = c(0,  FtimeLimit),
			expand=c(0.02, 0),
			# sec.axis = sec_axis(~., 
				# breaks = STATS[c("0.1", "1", "Median", "99", "99.9")],
				# labels = paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
				# )
			#	this is to add a secondary axis with the percentile values labeled
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		STATS = dispgSTATS
		scale_Y	=	scale_y_continuous(
			name="Refresh Cycles Later (1/60 s)",
			breaks=c(0, round(ytimes, 2)),
			labels = labelDisp,
			limits=c(0, FtimeLimit),
			expand=c(0.02, 0),
			# sec.axis = sec_axis(~., 
				# breaks = STATS[c("0.1", "1", "Median", "99", "99.9")],
				# labels = paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
			# )
			#	this is to add a secondary axis with the percentile values labeled
		)
	}

	ggplot() +
	ggtitle(gameQ, subtitle=paste0(datatype, " - QQ Distribution")) + labsGPU +
	geom_hline(yintercept = 1000/60, color = "red") +
		geom_rect(data = STATS, aes(ymin=-Inf, ymax=STATS[, c("0.1")],	xmin=-Inf, xmax=qnorm(c(.001))), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(data = STATS, aes(ymin=-Inf, ymax=STATS[, c("1")],	xmin=-Inf, xmax=qnorm(c(.010))), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(data = STATS, aes(ymin=-Inf, ymax=STATS[, c("Median")],	xmin=-Inf, xmax=qnorm(c(.500))), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(data = STATS, aes(ymin=-Inf, ymax=STATS[, c("99")],	xmin=-Inf, xmax=qnorm(c(.990))), alpha=0.1, fill=c("red"), color = "grey") +
		geom_rect(data = STATS, aes(ymin=-Inf, ymax=STATS[, c("99.9")],	xmin=-Inf, xmax=qnorm(c(.999))), alpha=0.1, fill=c("red"), color = "grey") +
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", size = 1.1, linetype = "dotted") +
	stat_qq(data = results, aes(sample=get(datatype))) +
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", alpha = 0.5, size = 1.1, linetype = "dotted") +
	geom_label(data = STATS, aes(x = Inf, y = -Inf, label = paste0("Slope: ", Slope), fontface = "bold"), parse = TRUE, hjust="right", vjust="bottom", fill = "darkgrey", color = "green") +
	facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
	scale_Y +
	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))
}