setwd("G:/Mi unidad/Investigación Trejo-Castro/E31 - DBC Metabolic Health and Disease/E31.2 Bibliometric Lipidome and Metabolome/6. Bibliometrix Results/4. Documents/MostFrequentWords")
disease<-read.csv("Word_Dynamics_bibliometrix_2025-08-16.csv")
data <-  reshape2::melt(disease,id.vars = 'Year', variable.name = 'Disease')

library(ggplot2)
library(RColorBrewer)

pal <- c(
  RColorBrewer::brewer.pal(12, "Paired"),
  "#000000" # por ejemplo negro para la última categoría
)

ggplot(data,aes(
  y = value ,
  x = Year,
  fill = Disease
)) + geom_bar(position="stack", stat="identity") +
  ggtitle("Disease Author Keywords Dynamics")+
  ylab("Number of documents")+
  theme_bw()+
  scale_fill_manual(values = pal)
       

  
