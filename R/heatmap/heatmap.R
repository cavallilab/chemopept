
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
#library(svglite)
library(grid)
#library(mclust)
library(ggsignif)
library(gtools)
#READ DATA
chemo <- read.delim("heatmap.txt",header = TRUE,sep = "\t")

df <- chemo %>%
  filter(Group_code <3)
chemo <- df

m_data <-as.matrix(chemo[,5:46])


chemo$Group_code <- ifelse(chemo$Group_code>1, 1, chemo$Group_code)
chemo$Group_name <- ifelse(chemo$Group_name =="COVID_Hospitalized","COVID-19",chemo$Group_name)
chemo$Group_name <- ifelse(chemo$Group_name =="COVID_Mild","COVID-19",chemo$Group_name)
chemo$Group_name <- ifelse(chemo$Group_name =="Healthy_control","Control",chemo$Group_name)

rownames(m_data)<-chemo$Short
name_df <- data.frame(chemo$Group_name)


rownames(name_df)<-chemo$Short
colnames(name_df)<-"Samples"
name_df <- data.frame(chemo$Group_name)

rownames(name_df)<-chemo$Short
colnames(name_df)<-"Samples"


name_df <- cbind(name_df,chemo$Total.AUC.Sum..all.chemokines.)
n<- ncol(name_df)
colnames(name_df)[n] <-"AUC Sum"


name_df <- cbind(name_df,log(chemo$IC50_ELISA_RBD))
n<- ncol(name_df)
colnames(name_df)[n] <-"logRBD"


name_df <- cbind(name_df,chemo$logNT50_neutralization_assay)
n<- ncol(name_df)
colnames(name_df)[n] <-"logNT50"




clustering_distance_rows = "correlation"
clustering_method = "mcquitty"
n_clusters = 3
cluster_patients = FALSE
cluster_chemo = TRUE
cluster_scale = FALSE
title = "Chemokine"


if(cluster_scale == TRUE){
  s_data <- scale(m_data)
}else {
  s_data <- m_data
}



pres<-pheatmap(s_data,main= title,
               cluster_rows = cluster_patients,
               clustering_distance_rows = clustering_distance_rows,
               clustering_method = clustering_method,
               cluster_cols = cluster_chemo,
               scale = 'none',
               show_rownames = TRUE,
               cutree_rows = n_clusters,
               cutree_cols = 1,
               annotation_row = name_df,
               annotation_names_row = TRUE,
               fontsize_row = 5, fontsize_col = 5
)

