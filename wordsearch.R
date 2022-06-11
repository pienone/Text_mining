library(readr)
library(tm)


articles <- read_csv("string 1.3.csv") #file che vuoi


# dimensioni - gruppi
gr1<-c("feasibility", "modelling")
gr2<-c("legal", "institutional", "law") 
gr3<-c("Land-use") 
gr4<-c("Management", "effectiveness", "valuation", "impact", "evaluation")
gr5<-c("Policy", "policy-making")


# Stemmiamo anche le parole nei gruppi
gr1<-c(stemDocument("feasibility"), stemDocument("modelling"))
gr2<-c(stemDocument("legal"), stemDocument("institutional"), stemDocument("law")) 
gr3<-c(stemDocument("Land-use")) 
gr4<-c(stemDocument("Management"), stemDocument("effectiveness"), stemDocument("valuation"), stemDocument("impact"), stemDocument("evaluation"))
gr5<-c(stemDocument("Policy"), stemDocument("policy-making"))


# Creo 5 colonne dove conto lo "score che ogni articolo ha in ogni gruppo"
articles$score_gr1 <- c()
articles$score_gr2 <- c()
articles$score_gr3 <- c()
articles$score_gr4 <- c()
articles$score_gr5 <- c()


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
  
}


#runna fino a qui per avere il csv con la conta dei gruppi

write.xlsx(articles, 'articoli contati_s1.xlsx', sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE) #per avere xlsx visto che csv non divide colonne bene
write.csv(articles,'articoli contati_s1.csv', sep = ",")  # se vuoi risalvare in excel

View(articles)

#più semplicemnte
# Creo per semplicitå una tabella in cui tengo solo titolo, DOI e gli score dei cinque gruppi
grouping<-articles[,c("Title", "DOI", "score_gr1", "score_gr2", "score_gr3", "score_gr4", "score_gr5")]

write.xlsx(grouping, 'articoli contati_s1.xlsx', sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE) #per avere xlsx visto che csv non divide colonne bene

View(grouping)






# Per vedere cosa sono le robe puoi runnare questo riga per riga

#articles è il csv 
head(articles,10)

#d 
head(d, 10) # d e' una lista dei 10 termini piu frequenti di un articolo (se i=584L è l'ultimo articolo)

#sum(v[names(v)==intersect(names(v), gr1)])
v[names(v)==intersect(names(v), gr5)]

#grouping è la tabella con articoli, DOI e conti dei 5 ambiti/temi/dimensioni
head(grouping,10)








































for(p in 1:length(Words)){                      # per ogni parola
  
  colname<- paste(Words[p],"_count", sep = "")   

    articles[, colname] <- 0      # aggiungi colonna=parola_count
  
  for(i in 1:nrow(articles)) {        # per ogni riga del csv

      content=paste(articles$`Title`[i],articles$`Author Keywords`[i],articles$`Abstract`[i]) # il contenuto di un articolo lo prendo come unione di Titolo, Author Keywords e Abstract
  
      docs=Corpus(VectorSource(content)) #prendo il content di ogni abstract e lo chiudo in un oggetto (Corpus) che piace alla libreria, su cui poi ci opero easy 

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

      dtm <- TermDocumentMatrix(docs)
      
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      head(d, 10) # head e' una lista dei termini piu frequenti, in ordine di frequenza
      
      group1=
      
      
      
      articles[i, colname]<- sum(scan_tokenizer(content) == Words[p])  # conta quante volte l'Abstract contiene la parola
      
      
  }
}




M=TermDocumentMatrix(jj)

scan_tokenizer(jj)













Words<-c("Incentives", "fire")    # lista di parole che vuoi


#write.csv(articles,'articoli contati.csv')  # se vuoi risalvare in excel



### per trovare parole piu frequenti

tot_content=""
for(i in 1:nrow(articles)){
    content=paste(articles$`Author Keywords`[i],articles$`Index Keywords`[i],articles$`Abstract`[i])
    tot_content=paste(tot_content,content)
}  

# cuttiamo le stopwords si possono vedere runnando - stopwords(kind = "en") -

tot_content <- tm_map(tot_content, removeWords, stopwords("english"))
findMostFreqTerms(as.DocumentTermMatrix(termFreq(tot_content, control = list())),n=60)

Corpus(VectorSource(tot_content))
jj <- tm_map(j, removeWords, stopwords('english'))
jj[[1]]


# dimensioni - gruppi
gr1<-c("feasibility", "modelling")
gr2<-c("legal", "institutional", "law") 
gr3<-c("Land-use") 
gr4<-c("Management", "effectiveness", "valuation", "impact", "evaluation")
gr5<-c("Policy", "policy-making")