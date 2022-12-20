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
  filter(Group_code == 1 |Group_code == 2)
chemo <- df



group_name<- ifelse(chemo$Group_code==1,"O","H")
colors = rainbow(length(unique(group_name)))
names(colors) = unique(group_name)
colors["O"] = "#00FF00"
colors["H"] = "#FF0000"
t_fit<-Rtsne(chemo[,5:45], theta = 0, perplexity = 20)
plot(t_fit$Y,main="tSNE", t='n',xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
points(t_fit$Y,pch = 16, col =colors[group_name])
legend("topright", legend = c("Outpatients","Hospitalized"), col = c("green",'red'),pch=16, cex=0.8 )

