library(readr)
library(tm)


articles <- read_csv("string 1.3.csv") #file che vuoi


# dimensioni - gruppi
gr1<-c("feasibility", "modelling", "hypothetical", "scenario")
gr2<-c("legal", "institutional", "law") 
gr3<-c("Land-use") 
gr4<-c("Management", "effectiveness", "valuation", "impact", "evaluation")
gr5<-c("Policy", "policy-making", "CAP")
gr6<-c("practice", "case study", "primary research")


# Stemmiamo anche le parole nei gruppi
gr1<-c(stemDocument("feasibility"), stemDocument("modelling"), stemDocument("hypothetical"), stemDocument("scenario"))
gr2<-c(stemDocument("legal"), stemDocument("institutional"), stemDocument("law"), stemDocument("regulation"), stemDocument("lawmaker"), stemDocument("liability")) 
gr3<-c(stemDocument("land-use"), stemDocument("land"), stemDocument("spatial"), stemDocument("forest cover")) 
gr4<-c(stemDocument("effectiveness"), stemDocument("impact"), stemDocument("evaluation"))
gr5<-c(stemDocument("policy"), stemDocument("policy-making"), stemDocument("CAP"), stemDocument("governance"))
gr6<-c(stemDocument("practice"), stemDocument("case study"), stemDocument("primary research"), stemDocument("on the ground"))

# Creo 5 colonne dove conto lo "score che ogni articolo ha in ogni gruppo"
articles$score_gr1 <- c()
articles$score_gr2 <- c()
articles$score_gr3 <- c()
articles$score_gr4 <- c()
articles$score_gr5 <- c()
articles$score_gr6 <- c()
articles$docs <- c()


#per ogni articolo conto le parole

for(i in 1:nrow(articles)) {        # per ogni riga del csv
  
  content <- paste(articles$`Title`[i],articles$`Author Keywords`[i],articles$`Abstract`[i]) # il content di un articolo lo prendo come unione di Titolo, Author Keywords e Abstract
  
  docs <- Corpus(VectorSource(content))         #prendo il content di ogni abstract e lo chiudo in un oggetto (Corpus) che piace alla libreria, su cui poi ci opero easy 
  
  # Convert the text to lower case   (da questo tutorial https://www.springboard.com/blog/data-science/text-mining-in-r/)
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords('english'))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  # Stemmiamo le parole (tipo connect,connections,connection, connected diventano tutte connect)
  docs <- tm_map(docs, stemDocument, language = "english")
  
  # contiamo le parole
  dtm <- TermDocumentMatrix(docs)
  
  
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  #  head(d, 10) # head e' una lista dei termini piu frequenti, in ordine di frequenza
  
  
  articles[i,"score_gr1"] <- sum(v[names(v)==intersect(names(v), gr1)])
  articles[i,"score_gr2"] <- sum(v[names(v)==intersect(names(v), gr2)])
  articles[i,"score_gr3"] <- sum(v[names(v)==intersect(names(v), gr3)])
  articles[i,"score_gr4"] <- sum(v[names(v)==intersect(names(v), gr4)])
  articles[i,"score_gr5"] <- sum(v[names(v)==intersect(names(v), gr5)])
  articles[i,"score_gr6"] <- sum(v[names(v)==intersect(names(v), gr6)])
  articles[i,"docs"]<-docs$content
  
  
}


# Creo per semplicitå una tabella in cui tengo solo titolo, DOI e gli score dei cinque gruppi
grouping<-articles[,c("Title", "DOI", "score_gr1", "score_gr2", "score_gr3", "score_gr4", "score_gr5", "score_gr6")]


#per vedere quanto contano i vari gruppi
colSums(articles[,c("score_gr1", "score_gr2", "score_gr3", "score_gr4", "score_gr5", "score_gr6")])









#runna fino a qui per avere il csv con la conta dei gruppi
write.xlsx(articles, 'articoli contati_s1.xlsx', sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE) #per avere xlsx visto che csv non divide colonne bene
write.csv(articles,'articoli contati_s1.csv', sep = ",")  # se vuoi risalvare in excel
View(articles)

#più semplicemnte
# Creo per semplicitå una tabella in cui tengo solo titolo, DOI e gli score dei cinque gruppi
grouping<-articles[,c("Title", "DOI", "score_gr1", "score_gr2", "score_gr3", "score_gr4", "score_gr5", "score_gr6")]

write.xlsx(grouping, 'articoli contati_s1.xlsx', sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE) #per avere xlsx visto che csv non divide colonne bene

View(grouping)









# Per vedere cosa sono le robe puoi runnare questo riga per riga
# articles è il csv 
#head(articles,10)
# d 
#head(d, 10) # d e' una lista dei 10 termini piu frequenti di un articolo (se i=584L è l'ultimo articolo)
# sum(v[names(v)==intersect(names(v), gr1)])
#v[names(v)==intersect(names(v), gr5)]
# grouping è la tabella con articoli, DOI e conti dei 5 ambiti/temi/dimensioni
#head(grouping,10)





# clustering

#install.packages("factoextra")
library(factoextra)
library(cluster)

df<-articles[,c("Title", "score_gr1", "score_gr2", "score_gr3", "score_gr4", "score_gr5", "score_gr6","docs")]


# remove duplicati
df<-df[!duplicated(df$Title),]

# indice come righe
rownames(df) <- df$Title

# remove Title
dfcut <- subset(df, select = -c(Title,docs))


#guardo l'ottimo numero di clusters
fviz_nbclust(dfcut, kmeans, method = "wss")

#faccio k mean con 3 gruppi
km <- kmeans(dfcut, centers = 5)

#guardo i gruppi
fviz_cluster(km, data = dfcut)


#kmeans(x[!is.na(x)], 3) 
#dfclust[!is.na(dfclust)]
#which(is.na(dfclust), arr.ind=TRUE)




aggregate(dfcut, by=list(cluster=km$cluster), mean)






# contare parole dei gruppi

clusters<-data.frame(matrix(0,ncol = 3, nrow = length(km$size)))
colnames(clusters)<-c("narticles","content","first20words")

for(i in 1:nrow(df)) { 
  clusters[km$cluster[i],"narticles"]  <-  clusters[km$cluster[i],"narticles"]+1
  clusters[km$cluster[i],"content"]  <-  paste(clusters[km$cluster[i],"content"],df[i,"docs"])
}

for(i in 1:nrow(clusters)) { 
  dtm <- TermDocumentMatrix(Corpus(VectorSource(clusters[i,"content"])))
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  clusters[i,"first20words"]= c(head(d,20))
  print(head(d,20))
}


clusters[1,"first20words"]













