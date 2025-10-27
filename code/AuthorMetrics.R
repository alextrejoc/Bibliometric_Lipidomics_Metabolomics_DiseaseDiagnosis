# Calculate m-index and g-index
setwd("G:/Mi unidad/Investigación Trejo-Castro/E31 - DBC Metabolic Health and Disease/E31.2 Bibliometric Lipidome and Metabolome")
author<-read.csv("7. Results Scopus Data Authors/Kastenmuller.csv")
author<-author[-1,]
author$Total<-as.numeric(author$Total)
totalcites<-sum(author$Total)
YFP<-min(author$Year)
h_index = function(Total) {
  if(max(Total) == 0) return(0) # assuming this is reasonable
  Total = Total[order(Total, decreasing = TRUE)]
  tail(which(Total >= seq_along(Total)), 1)
}
h<-h_index(author$Total)


author$square <- author$X^2
author$sums <- cumsum(author$Total)
g <- max(which(author$square<author$sums))

m_years<-2025-min(author$Year)+1

m<-round(h/m_years,3)
