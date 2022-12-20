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
chemo <- read.delim("data_tsne_all.txt",header = TRUE,sep = "\t")

df <- chemo %>%
  filter(Group_code >0)
chemo <- df

chemo$Group_code<- ifelse(chemo$Group_code==2,1,chemo$Group_code) #ALL COVID 1
chemo$Group_code<- ifelse(chemo$Group_code==3,2,chemo$Group_code) #HIV 
chemo$Group_code<- ifelse(chemo$Group_code==4,3,chemo$Group_code) #AS
chemo$Group_code<- ifelse(chemo$Group_code==5,4,chemo$Group_code) #RA
chemo$Group_code<- ifelse(chemo$Group_code==6,5,chemo$Group_code) #SjS

colors = rainbow(length(unique(chemo$Group_code)))
colors[1] = "black" #COVID
colors[2] = "red"   #HIV
colors[3] = "darkgreen" #AS
colors[4] = "blue" #RA
colors[5] = "lightblue" #SjS


t_fit<-Rtsne(chemo[,5:45], theta = 0, perplexity = 10)
plot(t_fit$Y,main="tSNE", t='n',xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
points(t_fit$Y,pch = 16, col = colors[chemo$Group_code])
legend("topright", legend = c("COVID","HIV","AS","RA","SjS"), col = c("black",'red',"darkgreen","blue","lightblue"),pch=16, cex=0.8 )
