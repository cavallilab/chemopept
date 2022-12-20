
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
#READ DATA
chemo <- read.delim("heatmap-2.txt",header = TRUE,sep = "\t")
ann <- read.delim("annotations.txt")

df <- chemo %>%
  filter(Group_code >0)
chemo <- df

m_data <-as.matrix(chemo[,5:46])

rownames(m_data)<-chemo$Short
name_df <- data.frame(chemo$Group_name)




rownames(name_df)<-chemo$Short
colnames(name_df)<-"Samples"






clustering_distance_rows = "correlation"
clustering_method = "ward.D"
n_clusters = 3
cluster_patients = TRUE
cluster_chemo = FALSE
cluster_scale = TRUE
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

pres.clust <- cbind(pres, cluster = cutree(pres$tree_row, k = n_clusters))
pres.clust <- as.data.frame(unlist(pres.clust[,2]))
colnames(pres.clust) <- c("Cluster")


pheatmap(s_data,main= title,
         cluster_rows = cluster_patients,
         clustering_distance_rows = clustering_distance_rows,
         clustering_method = clustering_method,
         cluster_cols = cluster_chemo,
         scale = 'none',
         show_rownames = TRUE,
         cutree_rows = n_clusters,
         cutree_cols = 1,
         annotation_row = cbind(name_df,pres.clust),
         #annotation_colors = ann.cols,
         annotation_names_row = F,
         fontsize_row = 5, fontsize_col = 5
)