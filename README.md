# Bibliometrix-multimodal-translation

This project examines the bibliographic network and descriptive statistics of literature on multimodal translation. 

## Source of data

The literature are collected from the core databases (SSCI and A&HCI) of Web of Science. The searching keywords are "multimodal AND translation" while excluding confounding items such as "therapy" and "clinical". Other miscellaneous articles from areas of medical science, psychology and computer vision are manually excluded. At last, 72 articles were involved in the database for analysis up to January 15, 2021. Among these articles, 2986 citations are also identified and analyzed.

## Tools

R package `Bibliometrix 3.0.3` (updated Sept 28, 2020) was used to extract the meta information from WoS items, to conduct descriptive analyses on the most cited papers and countries etc, and to visualize the bibliographic network. 

## Step-by-step analysis

1. data import
```
M <- convert2df(file, dbsource = "wos", format = "plaintext")
results <- biblioAnalysis(M, sep=';')
```
2. descriptive analysis

    1. most cited papers and correponding countries

    2. most frequent keywords

    3. annual production line plot
    
    ![annual scientific production](https://github.com/Chezvivian/Bibliometrix-multimodal-translation/blob/main/plots/Annual%20scientific%20production.png)

    4. most productive countries

    5. top citation per country

3. bibilographic network

- country collaboration network

```
Country <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")
```
- university collaboration network
```
UniNet <- biblioNetwork(M, analysis = "collaboration", network = "universities", sep = ";")
UniPlot <- networkPlot(UniNet, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.5)
uni_info <- metaTagExtraction(M, Field="AU_UN", sep=";", aff.disamb = TRUE)
```

- reference co-citation network

![co-citation reference](https://github.com/Chezvivian/Bibliometrix-multimodal-translation/blob/main/plots/Co-citation%20references.png)

```
Ref_cocite <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

net = networkPlot(Ref_cocite,  n = 20, weighted = T,
                  Title = "Co-citation of References", type = "fruchterman",
                  cluster = "louvain", label.cex = T, labelsize= 2,
                  edgesize = 5,edges.min = 1,
                    size = T,remove.multiple = T, alpha = 0.75)

netstat <- networkStat(Ref_cocite)
summary(netstat,k=10)
```

- keyword co-occurrence network

plot 1: more nodes

![co-occurrence of author keywords](https://github.com/Chezvivian/Bibliometrix-multimodal-translation/blob/main/plots/Co-occurrence%20of%20author%20keywords.png)

plot 2: fewer nodes

![co-occurrence of author keywords](https://github.com/Chezvivian/Bibliometrix-multimodal-translation/blob/main/plots/Co-occurrence%20of%20keywords.png)

```
Key_cooccur <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
net = networkPlot(Key_cooccur, n = 15,Title = "Co-occurrence of Keywords",
                  type = "circle", size = T, alpha = 0.75, remove.multiple = T) 
```

- historical network

```
histResults <- histNetwork(M, min.citations = 1, sep = ";")
Most_cited_hist <- histResults$histData
histPlot(histResults, n = 20, size = 5, labelsize = 5)
```

- conceptual structure

```
CS <- conceptualStructure(M, field = "ID", method = "CA", stemming = F, minDegree = 3, k.max = 5)
```
