library(ggplot2)

if (interactive()) {
		setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/Graphs")
} else {
	pdf(NULL)
}

labelPer = function(x) paste0(x, "\n", round(pnorm(x)*100, 2), "%")
labelCDF = function(x)	paste0(x*100, "%")

CDFPLOT = function(valu){
	ggplot(data = data.frame(x = c(-10, 10)), aes(x)) + ggtitle(paste0("Value: ", valu, ", Percentile: ", round(pnorm(valu)*100, 4), "%")) + 
	stat_function(fun = pnorm, n = 1001, xlim=c(-10, valu), geom="area", fill = "darkgreen", color = "darkgreen") + 
	stat_function(fun = dnorm, n = 1001, xlim=c(-10, valu), geom="area", fill = "darkgrey", color = "darkgrey") + 
	stat_function(fun = dnorm, n = 1001, color = "red", size = 1) + 
	stat_function(fun = pnorm, n = 1001, xlim=c(-10, valu), geom="area", fill = "darkgreen", alpha = 0.25) + 
	stat_function(fun = pnorm, n = 1001, color = "green", size = 1) + 
	scale_x_continuous(breaks = seq(-5, 5, 1), labels = labelPer, limits = c(-4, 4), expand = c(0, 0.2), name = NULL) + 
	scale_y_continuous(breaks = 0:10/10, labels=labelCDF, expand = c(0.01, 0), name = NULL)

	ggsave(filename=paste0("ECDF Example - ", valu, ".pdf"), width=4, height=4)
	# ggsave(filename=paste0("ECDF Example - ", valu, ".png"), width=4, height=4, dpi=300)
}

CDFPLOT(0)

CDFPLOT(2)
CDFPLOT(3)

CDFPLOT(-2)
CDFPLOT(-3)