## Buscador de palabras
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Read the text file from internet
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)
 #text <- readLines(file.choose()) # Selecci�n de archivos 
# Load the data as a corpus
docs <- Corpus(VectorSource(text))
inspect(docs) #Abre el documento a analizar 

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/") #Sustituci�n de caracteres en el texto
docs <- tm_map(docs, toSpace, "@") # por un espacio 
docs <- tm_map(docs, toSpace, "\\|")

# Convertir  el texto en minuscular
docs <- tm_map(docs, content_transformer(tolower))
# Remueve n�meros
docs <- tm_map(docs, removeNumbers)
# Elimina las palabras m�s comunes en ingles
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remover puntuaciones
docs <- tm_map(docs, removePunctuation)
# Elimina espacios en blanco extra
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
findFreqTerms(dtm, lowfreq = 4)

findAssocs(dtm, terms = "freedom", corlimit = 0.3)
head(d, 10)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


library(tidyverse)
library(tokenizers)

texto<-paste("Tambi�n entiendo que como es temporada de elecciones, 
               las expectativas para lo que lograremos este a�o son bajas.
               A�n as�, se�or Presidente de la C�mara de Representantes, 
               aprecio el enfoque constructivo que usted y los otros l�deres
               adoptaron a finales del a�o pasado para aprobar un presupuesto,
               y hacer permanentes los recortes de impuestos para las familias 
               trabajadoras. As� que espero que este a�o podamos trabajar juntos
               en prioridades bipartidistas como la reforma de la justicia penal
               y ayudar a la gente que est� luchando contra la adicci�n a f�rmacos
               de prescripci�n. Tal vez podamos sorprender de nuevo a los c�nicos.")

palabras <- tokenize_words(texto)



