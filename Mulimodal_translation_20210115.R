setwd("~/1 PhD/Y3-RUC/Projects/Multimodal Translation/2 R_analysis")
library(bibliometrix)
library(ggplot2)
library(forcats)
library(readr)
library(Matrix)
#biblioshiny()

#windowsFonts(
#  arial = windowsFont(family = "Arial"),
#  Helvetica = windowsFont(family = "Helvetica"),
#  yahei = windowsFont(family = "Microsoft YaHei")
#)


## import .txt file from Web of Science searching
file <- "WoS_72_20210115_full.txt"
M <- convert2df(file, dbsource = "wos", format = "plaintext")
#head(M['TC'])
write.csv(M,"Source_file_72_20210115.csv",fileEncoding = "UTF-8")

#levels(factor(M$SO))

#ggplot(M,aes(x=fct_infreq(SO)))+
  #geom_bar()
  

## 导入CNKI数据
# file_ZH <- "download_0706_converted.txt"
# C <- convert2df(file_ZH, dbsource = "wos", format = "plaintext")
# head(C['TC'])

## 1. descriptive results
results <- biblioAnalysis(M, sep=';')

# cited papers
cited_papers <- results$MostCitedPapers
colnames(cited_papers)[1] <- "Paper"

# countries

country <- M[,c("TI","PY","DE","C1")]
country$CO <- NA
country$Paper <- NA
country$Paper <- rownames(country)

country$CO[grep(pattern="^.*CHINA.*$",country$C1)] <- "China"
country$CO[grep(pattern="^.*ENGLAND.*$",country$C1)] <- "England"
country$CO[grep(pattern="^.*SPAIN.*$",country$C1)] <- "Spain"
country$CO[grep(pattern="^.*USA.*$",country$C1)] <- "America"

country <- merge(country, cited_papers)
country$CO
china <- unique(country[country$CO == "China",c("TI","DE","TC","CO")])

write.csv(country, "./20210115_results/country list.csv")

# keyword breakdown
keyword <- NULL
top_10_cited <- country[country$TC > 9,c("DE","TC")]

for (i in 1:10){
  keyword <- c(keyword, rep(c(unlist(strsplit(top_10_cited$DE[i],";"))),times = top_10_cited$TC[i]))
}


keyword_table <- table(keyword)
keyword_df <- as.data.frame(keyword_table)

# keyword breakdown-- method 2:
keyword_2 <- NULL
for (i in 1:10){
  keyword_2 <- c(keyword_2, unlist(strsplit(top_10_cited$DE[i],";")))
}
keyword_table_2 <- table(keyword_2)
keyword_df_2 <- as.data.frame(keyword_table_2)

## 2. Summary and plot
  # k = number of rows, pause = whether allow pause in screen scrolling
Sum_all <- summary(results, k =25)
# MainInformation <- write.csv(Sum_all$MainInformation,file ="MainInformation.csv",fileEncoding = "UTF-8")


# 2.1 Annual Production
AnnualProduction <- data.frame(Sum_all$AnnualProduction)
colnames(AnnualProduction)[1] <- c('Year')
AnnualProduction$Year <- as.character(AnnualProduction$Year)

add_year <- data.frame("Year" = c(2001,2002,2003,2004,2005,2006,2008),"Articles" =c(0,0,0,0,0,0,0))
annual_production <- rbind(AnnualProduction, add_year)
annual_production$Year <- as.numeric(annual_production$Year)
write.csv(annual_production,"./20210115_results/Annual production.csv")

ggplot(annual_production,aes(x=Year, y=Articles))+
  geom_line()+
  scale_x_continuous(breaks = annual_production$Year,labels = annual_production$Year)+
  geom_text(aes(label = Articles),nudge_y=0.5)


# 2.2 Most relevant sources
MostRelSources <- Sum_all$MostRelSources
WC <- M[,c('SO','WC','SC')]

MostRelSources$Research_areas <- NA
for (i in trimws(MostRelSources$Sources)){
  MostRelSources$Research_areas[which(trimws(MostRelSources$Sources) == i)] <- unique(WC$SC[which(WC$SO == i)])
}

MostRelSources$Research_areas <- tolower(MostRelSources$Research_areas)
write.csv(MostRelSources,"plots/Most relevant sources.csv",fileEncoding = "UTF-8")

  # Research areas chart
Sum_sources <- summary(results, k = 100)
Sources_all <- Sum_sources$MostRelSources
for (i in trimws(Sources_all$Sources)){
  Sources_all$Research_areas[which(trimws(Sources_all$Sources) == i)] <- unique(WC$SC[which(WC$SO == i)])
}

Source_pie_data <- NULL
for (i in 1:length(Sources_all$Research_areas)){
  Source_pie_data <- c(Source_pie_data, unlist(strsplit(Sources_all$Research_areas[i],";")))
}
Source_pie_data <- trimws(Source_pie_data)
print(Source_pie_data)

pie_table <- table(Source_pie_data)
pie_df <- as.data.frame(pie_table)
write.csv(pie_df,file="plots/research areas frequency.csv")
#freq <- prop.table(pie_table)
#print(freq)
ggplot(Source_pie_data,aes(Freq))+
  geom_bar()

#pie(freq,paste(names(freq)),clockwise=T,
#   main="Percetange of research areas")

# 2.3 Most productive countries
MostProdCountries <- Sum_all$MostProdCountries
write.csv(MostProdCountries,"Most productive countries.csv",fileEncoding = "UTF-8")

# 2.4 Top citation per country
TCperCountries <- Sum_all$TCperCountries
write.csv(TCperCountries,"Most cited countries.csv",fileEncoding = "UTF-8")

# basci plot
plot(x=results, k = 10, pause=FALSE)


## 3. Analysis of core citations, keywords and authors

MostProdAuthors <- Sum_all$MostProdAuthors
MostRelKeywords <- Sum_all$MostRelKeywords

  # Most frequent cited manuscripts

CR_article <- citations(M, field = "article", sep = ";")
Top_cited <- as.data.frame(cbind(CR_article$Cited[1:20]))
Top_cited$citation <- row.names(Top_cited)
# for (i in 1:length(Top_cited$citation)){
#   Top_cited$Author[i] <- unlist(strsplit(Top_cited$citation[i],","))[1]
#   Top_cited$Year[i] <- unlist(strsplit(Top_cited$citation[i],","))[2]
#   Top_cited$Source[i] <- unlist(strsplit(Top_cited$citation[i],","))[3]
# }

colnames(Top_cited) <- c("Citations","Cited references")
write.csv(Top_cited[,c(2,1)],"Most frequent cited references.csv", fileEncoding = "UTF-8")



  # most frequent cited first authors: 
CR_author <- citations(M, field = "author", sep = ";")
cbind(CR_author$Cited[1:20])

  # most frequent local cited authors:
CR_local <- localCitations(M, sep = ";")
CR_local$Authors[1:10,]
CR_local$Papers[1:10,]


### 4. Analysis of authors

  # Author's dominance ranking
DF <- dominance(results, k = 10)
DF

  # Top authors' productivity over time
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
head(topAU$dfAU)


### 5. Bibliographic networks
# 5.1 Country collaboration network

Country <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")


# 5.2 University collaboration network
UniNet <- biblioNetwork(M, analysis = "collaboration", network = "universities", sep = ";")
UniPlot <- networkPlot(UniNet, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.5)

uni_info <- metaTagExtraction(M, Field="AU_UN", sep=";", aff.disamb = TRUE)

# 5.3 Reference co-citation network
Ref_cocite <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

net = networkPlot(Ref_cocite,  n = 20, weighted = T,
                  Title = "Co-citation of References", type = "fruchterman",
                  cluster = "louvain", label.cex = T, labelsize= 2,
                  edgesize = 5,edges.min = 1,
                    size = T,remove.multiple = T, alpha = 0.75)

netstat <- networkStat(Ref_cocite)
summary(netstat,k=10)

# 5.4 Keyword co-occurence network

## biblioshiny has different node shape, has opacity for nodes, 
Key_cooccur <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
net = networkPlot(Key_cooccur, n = 15,Title = "Co-occurrence of Keywords",
                  type = "circle", size = T, alpha = 0.75, remove.multiple = T) 



### 6. Co-occurrence matrix 
## options: AU, SO, JI, DE (author keywords),ID (WoS keywords),CR (cited references) 
# 6.1 author matrix
AU_comat <- cocMatrix(M, Field= "AU",type = "matrix", n= 10, sep = ";", binary = FALSE)

# 6.2 citation matrix
CR_comat <- cocMatrix(M, Field = "CR", type = "matrix", n = 10, sep = ";" , binary = T)



### 7. Historical network
## 7.1 Co-citation historical network
histResults <- histNetwork(M, min.citations = 1, sep = ";")
Most_cited_hist <- histResults$histData

histPlot(histResults, n = 20, size = 5, labelsize = 5)



### 8. Conceptual structure
CS <- conceptualStructure(M, field = "ID", method = "CA", stemming = F, minDegree = 3, k.max = 5)
