library(ggplot2)
library(reshape2)
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))
logDir <- "~/projects/fdur/target/log/"

ll <- read.table(paste(logDir,"ll.log",sep=""))$V1
df <- data.frame(recursion = c(1:length(ll)), logLikelihood=ll)
g <- ggplot(df,aes(x = recursion,y = logLikelihood))
g <- g + geom_line(colour = "magenta",linetype = 1,size = 0.5)
g <- g + xlab("Time")
g <- g + ylab("Log Likelihood") 
g <- g + ggtitle("Log Likelihood")
print(g, vp=viewport(layout.pos.row=1, layout.pos.col=1))
#------------------------------b--------------------------------------
x <- read.table(paste(logDir,"b.log",sep=""),header = FALSE)
colnames(x) <- c("a","b","c","d","e","f")
x <- cbind(1:nrow(x), x)
colnames(x)[1] <- "time"
df <- melt(x,id.var="time")
g <- ggplot(df,aes(x = time,y = value,group = variable,colour = variable))
g <- g + geom_line(linetype = 1,size = 0.5)
g <- g + xlab("Time")
g <- g + ylab("Substitution Parameters")
g <- g + ggtitle("Substitution Rate")
print(g, vp=viewport(layout.pos.row=1, layout.pos.col=2))
#---------------------------------------------------------------------
#-----------------------------pi--------------------------------------
x <- read.table(paste(logDir,"pi.log",sep=""),header = FALSE)
colnames(x) <- c("A","C","G","T")
x <- cbind(1:nrow(x), x)
colnames(x)[1] <- "Time"
df <- melt(x,id.var="Time")
g <- ggplot(df,aes(x = Time,y = value,group = variable,colour = variable))
g <- g + geom_line(linetype = 1,size = 0.5)
g <- g + ylab("Base Frequency")
g <- g + ggtitle("Base Frequency")
print(g, vp=viewport(layout.pos.row=2, layout.pos.col=1))
#---------------------------------------------------------------------
#---------------------------time--------------------------------------
x <- read.table(paste(logDir,"tm.log",sep=""),header = FALSE)
colnames(x) <- c("job","msec")
g <- ggplot(x,aes(x = msec,fill = job))
g <- g + geom_histogram(alpha=0.5)
g <- g + ggtitle("Consumption time")
print(g, vp=viewport(layout.pos.row=2, layout.pos.col=2)) 
#---------------------------------------------------------------------