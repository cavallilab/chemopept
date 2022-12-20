#TSNE
library(shiny)
library(shinydashboard)
library(pheatmap)
library(RColorBrewer)
library(reshape)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(gridExtra)
library(splines)
library(grid)
library(ggsignif)
library(gtools)
library(Rtsne)
#READ DATA
chemo <- read.delim("data_covid-19.txt",header = TRUE,sep = "\t")


df <- chemo %>%
  filter(Group_code <3)
chemo <- df




group_name<- ifelse(chemo$Group_code==0,"H","C")
colors = rainbow(length(unique(group_name)))
names(colors) = unique(group_name)
colors["H"] = "#000000"
colors["C"] = "#0000FF"
t_fit<-Rtsne(chemo[,5:45], theta = 0, perplexity = 20)
plot(t_fit$Y,main="tSNE", t='n',xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
points(t_fit$Y,pch = 16, col =colors[group_name])
legend("topright", legend = c("Controls","COVID19"), col = c("black",'blue'),pch=16, cex=0.8 )

