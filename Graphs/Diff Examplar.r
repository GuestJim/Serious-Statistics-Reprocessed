library(ggplot2)

if (interactive()) {
		setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics and Scripts/Graphs")
} else {
	pdf(NULL)
}

yrates	=	c(120, 60, 30, 20, 15, 12, 10)
yrates	=	sort(c(yrates,-yrates))
ytimes	=	sort(1000/yrates)

# labelRound	=	function(x)	sprintf("%.1f", x)
labelRound	=	function(x)			round(x, 1)
labelBreak	=	function(input)		paste0(rep(c("", "\n"), length.out = length(input)), input)
labelDisp	=	function(breaks)	round(breaks * 60/1000, 1)

FtimeLimit	=	1000/15
diffLim 	=	1000 / 50
gameQ		=	"Example"

DATA	=	as.data.frame(1000 / c(60, 60, 30, 60))

DATA	=	as.data.frame(c(10, 10, 20, 5, 10))

results	=	cbind(DATA, c(diff(DATA[,1]), 0))
colnames(results) = c("Data", "Difference")

COORDS	=	paste("(", round(results$Data, 2), ", ", round(results$Difference, 2), ")")

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


ggplot(data = results, aes(x = Data, y = Difference)) +
ggtitle(gameQ, subtitle=paste0("Consecutive Differences")) + 
geom_path(data = results[1:2,], aes(x = Data, y = Difference), arrow = arrow(type = "closed")) + 
geom_path(data = results[2:3,], aes(x = Data, y = Difference), arrow = arrow(type = "closed")) + 
geom_path(data = results[3:4,], aes(x = Data, y = Difference), arrow = arrow(type = "closed")) + 
geom_path(data = results[4:5,], aes(x = Data, y = Difference), arrow = arrow(type = "closed")) + 
geom_point(color = "red", size = 3) +
geom_label(aes(label = COORDS), hjust = "left", nudge_x = 1) + 
scale_X +
scale_Y

ggsave(filename = "Diff Example.pdf", width = 8, height = 8)
ggsave(filename = "Diff Example.png", width = 8, height = 8, dpi = 300)
