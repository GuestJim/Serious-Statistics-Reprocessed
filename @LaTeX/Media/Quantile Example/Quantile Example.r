library(ggplot2)

if (interactive()) {
		setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/Graphs")
} else {
	pdf(NULL)
}

labelPer = function(x) paste0(x, "\n", round(pnorm(x)*100, 2), "%")

QPLOT = function(quan){
	ggplot(data = data.frame(x = c(-10, 10)), aes(x)) + ggtitle(paste0("Percentile: ", quan*100, "%, Value: ", round(qnorm(quan), 4))) + 
	stat_function(fun = dnorm, n = 1001, xlim=c(-10, qnorm(quan)), geom="area", fill = "darkgrey", color = "darkgrey") + 
	stat_function(fun = dnorm, n = 1001, color = "red", size = 1) + 
	scale_x_continuous(breaks = seq(-5, 5, 0.5), labels = labelPer, limits = c(-4, 4), expand = c(0, 0.2), name = NULL) + 
	scale_y_continuous(breaks = NULL, expand = c(0.01, 0), name = NULL)

	ggsave(filename=paste0("Quantile Example - ", quan, ".pdf"), width=8, height=8)
	ggsave(filename=paste0("Quantile Example - ", quan, ".png"), width=8, height=8, dpi=300)
}

QPLOT(0.5)

QPLOT(0.99)
QPLOT(0.999)

QPLOT(0.01)
QPLOT(0.001)