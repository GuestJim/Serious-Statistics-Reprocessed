library(ggplot2)

if (interactive()) {
		setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/Graphs")
} else {
	pdf(NULL)
}

labelRound = function(x) sprintf("%.2f", x)
labelRoundMS = function(x) sprintf("%.2f", x * 1000)
labelRound = function(x) round(x, 2)
labelRoundMS = function(x) round(x * 1000, 2)

frametimes = seq(0, .18, 1 / 120)
framerates = 1000 / (frametimes * 1000)

framerates = c(seq(0, 120, 30), seq(180, 1000, 60))

upperlim = Inf
upperlim = 1e9
lowerlim = 10

POI = c(30, 60, 90, 120)

POIframe = data.frame(t(rbind(1/POI, POI)))
colnames(POIframe) = c("x", "y")

insert = ggplotGrob(
	ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
	geom_line(data = POIframe, aes(x = x, y = y), color = "green", size = 1) + 
	stat_function(fun = function (x) 1 / x) + 
	geom_point(data = POIframe, aes(x = x, y = y), color = "green", shape = "x", size = 3) + 
	scale_x_continuous(limits = 1 / c(160, 25), breaks = 1 / POI, labels = labelRoundMS, expand = c(0,0), name = "") + 
	scale_y_continuous(limits = c(25, 160), breaks = POI, expand = c(0, 0), name = "") + 
	expand_limits(x=0, y=0) + 
	# theme_void() + 
	theme(panel.background = element_rect(fill = "#f1a4a4")) + 
	theme(panel.grid.minor = element_blank()) + 
	theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0), "point"), plot.background = element_rect(fill = "#fcfcfc"))
	# theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0), "point"), plot.background = element_rect(fill = "#ffffffDD"))
)

lowX = 1/27
widX = 22/1000
lowY = 350
heiY = 90
scal = 2.8

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
annotate("rect", ymin = 30, ymax = 120, xmin = 1/120, xmax = 1/30, alpha = 0.3, fill = "red") +
stat_function(fun = function (x) 1 / x) + 
annotation_custom(grob = insert, xmin = lowX, xmax = lowX + widX * scal, ymin = lowY, ymax = lowY + heiY * scal) + 
scale_x_continuous(limits = 1 / c(upperlim, lowerlim), breaks = frametimes, labels = labelRoundMS, name = "Milliseconds per Frame", expand = c(0.01, 0)) + 
scale_y_continuous(limits = c(0, 1000), breaks = framerates, name = "Frames per Second", expand = c(0,0))

ggsave(filename="Rate Graph.pdf", width=8, height=8)
ggsave(filename="Rate Graph.png", width=8, height=8, dpi=300)
