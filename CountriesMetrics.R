setwd("G:/Mi unidad/Investigación Trejo-Castro/E31 - DBC Metabolic Health and Disease/E31.2 Bibliometric Lipidome and Metabolome")

db<-read.csv("1. Database/2. Filtered Dataset/08_02_2025/Bibliometrix-Export-File-2025-02-15.csv", fileEncoding = "latin1")

countries<-c("United States","China","India","United Kingdom", "Italy","Germany",
             "South Korea","Canada","Spain","Brazil","Iran","Australia","Saudi Arabia",
             "France","Turkey","Japan","Egypt","Sweden","Netherlands",
             "Taiwan","Pakistan","Mexico","Switzerland","Greece","Belgium","Russia",
             "Hong Kong","South Africa","Finland","Argentina","Malaysia","Denmark",
             "Singapore","Austria","Bangladesh","Israel","Poland","Portugal",
             "Hungary","Indonesia","Nigeria","Thailand","Romania","United Arab Emirates",
             "Norway","Cyprus","Czech Republic","Colombia","Chile","Ireland",
             "New Zealand","Qatar","Serbia","Slovenia","Jordan","Vietnam",
             "Luxembourg","Algeria","Macao","Croatia","Ecuador","Iraq",
             "Sudan","Ghana","Oman","Cuba","Estonia","Tunisia","Ukraine",
             "Uruguay","Bahrain","Bulgaria","Kenya","Lebanon","Palestine",
             "Sri Lanka","Iceland","Mauritius","Philippines","Slovakia","Venezuela",
             "Yemen","Cameroon","Kazakhstan","Libya","Morocco","Tanzania",
             "Afghanistan","Albania","Armenia","Democratic Republic Congo",
             "Ethiopia","Kuwait","Kyrgyzstan","Latvia","Paraguay","Puerto Rico",
             "Uzbekistan","Barbados","Brunei","Curaçao","Dominican Republic","Gabon",
             "Georgia","Lithuania","Malawi","Malta","Mozambique","Namibia","North Macedonia",
             "Peru","Rwanda","Saint Vincent and the Grenadines","Samoa","Senegal",
             "Syrian Arab Republic","Uganda","Zambia")
countries<-toupper(countries)

np<-c()
ca<-c()

for(i in 1:length(countries))
{
  np[i]<-sum(regexpr(countries[i],db$C1)!=-1)
  ca[i]<-sum(regexpr(countries[i],db$RP)!=-1)
 
}
res1<-cbind(countries,np,ca)


library(countrycode)
codes<-unique(countrycode(countryname_dict$country.name.en,"country.name","iso3c"))
names<-countrycode(codes,"iso3c","country.name")
names<-toupper(names)
countries<-names[!is.na(names)]
np<-matrix(nrow=nrow(db),ncol=length(countries))
ca<-matrix(nrow=nrow(db),ncol=length(countries))

for(i in 1:nrow(db))
{
  for(j in 1:length(countries))
  {
    np[i,j]<-ifelse(regexpr(countries[j],db$C1[i])==-1,0,1)
    ca[i,j]<-ifelse(regexpr(countries[j],db$RP[i])==-1,0,1)
  }
}
colnames(np)<-countries
colnames(ca)<-countries
top10np<-head(sort(colSums(np),decreasing=TRUE),247)
top10names<-names(top10np)
top10ca<-colSums(ca)[top10names]

res2<-cbind(top10names,top10np,top10ca)
scp<-c()
mcp<-c()
for(i in 1:247)
{
  temp<-which(ca[,top10names[i]]==1)
  if(length(temp)!=1 & length(temp)!=0){
    scp[i]<-sum(rowSums(np[temp,])==1)
    mcp[i]<-sum(rowSums(np[temp,])!=1)
  } else{
    scp[i]<-"Not possible"
    mcp[i]<-"Not possible"
  }
}
names(scp)<-top10names
names(mcp)<-top10names
out<-cbind(np=top10np,ca=top10ca,scp,mcp,ratio=NA)
out<-data.frame(out)
out[,1:5]<-sapply(out[,1:5], as.numeric)

for(i in 1:247){
  ifelse(out[i,"scp"]>0,out[i,"ratio"]<-round(out[i,"mcp"]/out[i,"ca"],3),NA)
}


tc<-c()
for(i in 1:247)
{
  temp<-which(np[,top10names[i]]==1)
  tc[i]<-sum(db$TC[temp])
}

tcca<-c()
for(i in 1:247)
{
  temp<-which(ca[,top10names[i]]==1)
  tcca[i]<-sum(db$TC[temp])
}
out<-cbind(out,tc,tcca)
write.csv(out,file = "8. Results countries/countries results.csv")

