library(bslib)
library(tidyverse)
library(tm)
library(tidytext)
library(magrittr)
library(stringr)
library(stringi)
library(RColorBrewer)
library(ggraph)
library(igraph)
library(widyr)
library(dplyr)
library(lubridate)
library(topicmodels)
library(stopwords)
library(shinyjs)
library(shiny)
library(scales)
library(wordcloud)
library(shiny)
library(sentimentr)
library(syuzhet)
library(wordcloud2)
library(igraph)

shinyServer(
  
  server <- function(input, output, session){
    
     texto1 <- reactive({
      
      pdftools::pdf_text(input$file1$datapath)
      
     })
    
    
    tabla <- reactive({
      texto2 <- tolower(texto1())
      texto2 <- gsub("\\r", " ", texto2)
      texto2 <- gsub("\\n", "", texto2)
      texto2 <- gsub("\\d\\K\\.(?=\\d)", "", texto2, perl = TRUE)
      texto2 <- paste(texto2, collapse = ' ')
      
      vector <- c()
      for(i in 1:length(texto2)){
        temp <- (strsplit(texto2[[i]], "\\.")[[1]])
        vector <- c(vector,temp)
      }
      frases_texto <- as.data.frame(vector)
      colnames(frases_texto)[1] <- "frase"
      frases_texto$frase <- trimws(frases_texto$frase, "both")
      frases_texto$frase <- as.character(frases_texto$frase)
      lexiconSW<-stopwords("en")
      lexiconSW <- append(lexiconSW,c("capítulo","d"," ","of","the",
                                      "a","is","and","the","in","that",
                                      "this","can","to"))
      lexiconSW<-as.data.frame(lexiconSW) 
      names(lexiconSW)<-"word"
      lexiconSW$word<-as.character(lexiconSW$word)
      
      df <- tibble::rowid_to_column(frases_texto, "ID")
      
      review_words <- df %>%
        distinct(frase, .keep_all = TRUE) %>%
        unnest_tokens(word, frase, drop = FALSE) %>%
        distinct(ID, word, .keep_all = TRUE) %>%
        anti_join(lexiconSW) %>%
        filter(str_detect(word, "[^\\d]")) %>%
        group_by(word) %>%
        dplyr::mutate(word_total = n()) %>%
        ungroup() 
      
      word_counts <- review_words %>%
        dplyr::count(word, sort = TRUE)
     
     word_counts %>% 
       head(input$a) 
  
    }) 
    
    tabla2 <- reactive({ #Eston son los Bigramas. 
      texto2 <- tolower(texto1())
      texto2 <- gsub("\\r", " ", texto2)
      texto2 <- gsub("\\n", "", texto2)
      texto2 <- gsub("\\d\\K\\.(?=\\d)", "", texto2, perl = TRUE)
      texto2 <- paste(texto2, collapse = ' ')
      
      vector <- c()
      for(i in 1:length(texto2)){
        temp <- (strsplit(texto2[[i]], "\\.")[[1]])
        vector <- c(vector,temp)
      }
      frases_texto <- as.data.frame(vector)
      colnames(frases_texto)[1] <- "frase"
      frases_texto$frase <- trimws(frases_texto$frase, "both")
      frases_texto$frase <- as.character(frases_texto$frase)
      lexiconSW<-stopwords("en")
      lexiconSW <- append(lexiconSW,c("capítulo","d"," ","of","the",
                                      "a","is","and","the","in","that",
                                      "this","can","to"))
      lexiconSW<-as.data.frame(lexiconSW) 
      names(lexiconSW)<-"word"
      lexiconSW$word<-as.character(lexiconSW$word)
      
      df <- tibble::rowid_to_column(frases_texto, "ID")
      review_bigrams <- df %>% 
        unnest_tokens(bigram, frase, token = "ngrams", n = 2) #Separamos token
      bigrams_separated <- review_bigrams %>% 
        separate(bigram, c("word1", "word2"), sep =" ")
      bigrams_filtered <- bigrams_separated %>% 
        filter(!word1 %in% lexiconSW$word) %>%
        filter(!word2 %in% lexiconSW$word)
      bigrams_counts <- bigrams_filtered %>% 
        unite(bigram, word1, word2, sep = " ")
      bigrams_united <- bigrams_filtered %>% 
        unite(bigram, word1, word2, sep = " ")
      big <-  bigrams_united %>% 
        dplyr::count(bigram, sort = TRUE)
      colnames(big) <- c("word", "n")
      big %>% head(input$a) 
    })
    
    
    tabla3 <- reactive({ ####Estos son los TRIGRAMAS!!
      texto2 <- tolower(texto1())
      texto2 <- gsub("\\r", " ", texto2)
      texto2 <- gsub("\\n", "", texto2)
      texto2 <- gsub("\\d\\K\\.(?=\\d)", "", texto2, perl = TRUE)
      texto2 <- paste(texto2, collapse = ' ')
      
      vector <- c()
      for(i in 1:length(texto2)){
        temp <- (strsplit(texto2[[i]], "\\.")[[1]])
        vector <- c(vector,temp)
      }
      frases_texto <- as.data.frame(vector)
      colnames(frases_texto)[1] <- "frase"
      frases_texto$frase <- trimws(frases_texto$frase, "both")
      frases_texto$frase <- as.character(frases_texto$frase)
      lexiconSW<-stopwords("en")
      lexiconSW <- append(lexiconSW,c("capítulo","d"," ","of","the",
                                      "a","is","and","the","in","that",
                                      "this","can","to"))
      lexiconSW<-as.data.frame(lexiconSW) 
      names(lexiconSW)<-"word"
      lexiconSW$word<-as.character(lexiconSW$word)
      
      df <- tibble::rowid_to_column(frases_texto, "ID")
      review_trigrams <- df %>% 
        unnest_tokens(trigram, frase, token = "ngrams", n = 3) #Separamos token
      trigrams_separated <- review_trigrams %>% 
        separate(trigram, c("word1", "word2", "word3"), sep =" ")
      trigrams_filtered <- trigrams_separated %>% 
        filter(!word1 %in% lexiconSW$word) %>%
        filter(!word2 %in% lexiconSW$word) %>%
        filter(!word3 %in% lexiconSW$word)
      trigrams_counts <- trigrams_filtered %>% 
        unite(trigram, word1, word2,word3, sep = " ")
      trigrams_united <- trigrams_filtered %>% 
        unite(trigram, word1, word2, word3, sep = " ")
      trig <-  trigrams_united %>% 
        dplyr::count(trigram, sort = TRUE)
      colnames(trig) <- c("word", "n")
      trig %>% head(input$a) 
      
    })
    
    
    plot1 <- reactive({
      rda() %>%
        head(input$a) %>%
        ggplot(aes(word, n)) +
        geom_col(fill = "green") +
        scale_y_continuous(labels = comma_format()) +
        coord_flip() +
        labs(title = paste0("Palabras más utilizadas"),
             subtitle = "Stopwords removidas",
             x = "Ngrama",
             y = "Número de veces usada") }) 
    
   
    
    wc <- reactive({
      wordcloud2(data = rda(), shape = circle)})
    
    
    
    
    Lda <- reactive({
      texto3 <- texto1()
      texto3 <- tolower(texto3)
      texto3 <- gsub("\\r", " ", texto3)
      texto3 <- gsub("\\n", "", texto3)
      texto3 <- gsub("\\d\\K\\.(?=\\d)", "", texto3, perl = TRUE)
      texto3 <- paste(texto3, collapse = ' ')
      length(texto3)
      vector <- c()
      for(i in 1:length(texto3)){
        temp <- (strsplit(texto3[[i]], "\\.")[[1]])
        vector <- c(vector,temp)
      }
      
      frases_texto <- as.data.frame(vector)
      colnames(frases_texto)[1] <- "frase"
      frases_texto$frase <- trimws(frases_texto$frase, "both")
      frases_texto$frase <- as.character(frases_texto$frase)
      lexiconSW<-stopwords("en")
      lexiconSW <- append(lexiconSW,c("capítulo","d"," ","of","the",
                                      "a","is","and","the","in","that",
                                      "this","can","to","not","for","are", 
                                      "with","but","any","such","these"))
      lexiconSW<-as.data.frame(lexiconSW) 
      names(lexiconSW)<-"word"
      lexiconSW$word<-as.character(lexiconSW$word)
      
      df <- tibble::rowid_to_column(frases_texto, "ID")
      df <- gsub("the", " ", df)
      df <- gsub("that", " ", df)
      df <- gsub("or", " ", df)
      df <- gsub("but", " ", df)
      df <- gsub("any", " ", df)
      df <- gsub("these", " ", df)
      df <- gsub("and", " ", df)
      df <- gsub("this"," ",df)
      df <- gsub("when", " ", df)
      df <- gsub("with", " ", df)
      df <- gsub("non"," ", df)
      df <- gsub("not"," ", df)
      df <- gsub("which", " ", df)
      myCorpus <- Corpus(VectorSource(df))
      myCorpus <- tm_map(myCorpus, removePunctuation)
      myCorpus <- tm_map(myCorpus, removeNumbers)
      
      
      dtm <- DocumentTermMatrix(myCorpus)
      
      raw.sum=apply(dtm,1,FUN=sum) 
      dtm=dtm[raw.sum!=0,]
      
      
      ap_lda <- LDA(x = dtm, k = 2, control = list(seed = 1234))
      
      ap_topics <- tidy(x = ap_lda, matrix = "beta")
      
      ap_top_terms <- ap_topics %>%
        group_by(topic) %>%
        top_n(15, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
      
      ap_top_terms %>%
        mutate(term = reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip()
    })
    
    
    bigram_graph <- reactive({
      rda() %>%
      #bigram_graph <- setNames(bigram_graph, c("word", "number")) %>%
        head(input$a) %>%
        graph_from_data_frame()})
      
      
     plot2 <- reactive({
      set.seed(2016)
      a <- grid::arrow(type = 'closed', length = unit(.18, "inches"))
      ggraph(bigram_graph(), layout = "fr") + 
        geom_edge_link(aes(edge_alpha = input$a), show.legend = FALSE,
                       arrow = a, end_cap = circle(0.7, 'inches')) + 
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
     })
      
    
    
      #sentimientos_df <- reactive({get_nrc_sentiment(rda$word(), lang = "english")})

      
      
      
    output$texto <- renderText(texto1())  
    
    output$tabla <- renderTable({
      
      rda()
    })
    
    tabla_S <- reactive({
    texto4 <- texto1()
    texto4 <- tolower(texto4)
    texto4 <- gsub("\\r", " ", texto4)
    texto4 <- gsub("\\n", "", texto4)
    texto4 <- gsub("\\d\\K\\.(?=\\d)", "", texto4, perl = TRUE)
    texto4 <- paste(texto4, collapse = ' ')
    
    removeNumbers(texto4)
    
    removeWords(texto4, words =c("she", "he"))
    
    stemDocument(texto4)
    
    length(texto4)
    vector <- c()
    for(i in 1:length(texto4)){
      temp <- (strsplit(texto4[[i]], "\\.")[[1]])
      vector <- c(vector,temp)
    }
    frases_texto <- as.data.frame(vector)
    df <- tibble::rowid_to_column(frases_texto, "ID")
    colnames(df) <- c("doc_id", "text")
    
    ds <- DataframeSource(df)
    
    corpus <- VCorpus(ds)
    
    tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTf))
    
    tdm.tweets.m <- as.matrix(tdm)
    
    term.freq <- rowSums(tdm.tweets.m)
    
    tweets_cv <- as.vector(df$text)
    
    sentimientos_df <- get_nrc_sentiment(tweets_cv, lang = "english")
  
    })
    
    
    plotS <- reactive({
      barplot(
        colSums(tabla_S()), 
        space = 0.2,
        horiz = FALSE,
        las = 1,
        cex.names = 1.2,
        col = brewer.pal(n = 8, name = "Set3"),
        main = "Análisis de Sentimiento NRC",
        xlab="Rango de emociones", ylab = NULL)
      
    })
    ##CLustering
    plotK <- reactive({
      A <- get.adjacency(bigram_graph(), sparse = F)
      B <- A + t(A)
      isSymmetric(B)
      table(B)
      B[B!=0] <- 1
      B[B!=1] <- 0
      
      g <- graph_from_adjacency_matrix(adjmatrix = B, mode = "undirected")
      kc <- fastgreedy.community(g)
    
    })
    
    
    #observe({
    ##input$a
    #output$sent <- renderPlot(plot22())
      
    #})
    output$sentT <- renderTable({
      tabla_S()
    })
    output$sentH <- renderPlot({
        plotS()
    })
    
    output$KMIN <- renderPlot({
      plot(plotK(), bigram_graph())
    })
    
    
    
    observe({
      input$a
      output$frecuencias <- renderPlot(plot1())               
    }) 
    
    observe({
      input$a
      output$mapaw <- renderWordcloud2(wc())
    })
    
    output$demo <- renderPlot(Lda())
    
    observe({
      input$a
      output$k <- renderPlot(plot2()) 
    })
    
    
    
    
  }) 
    
    
    
  





