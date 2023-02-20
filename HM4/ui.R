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
peluquero <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
options(shiny.maxRequestSize = 30*1024^2)


shinyUI(fluidPage(
  
  headerPanel(title = "Hermeneuticon v.2.2"),
  theme = bs_theme(bootswatch = "superhero"),
  
  
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", label = "Seleccionar PDF", accept = "pdf", multiple = F),
      
      h6("Shushetusoftware, V.1-22-23"),
      
      br(),
      
      sliderInput("a", label = "Word Frequency Ranking", min = 0, max = 100, value = 50), 
      
      br(), 
      radioButtons("radiol", "Choose N-Gram", 
                   c("Unigram" = "Uni",
                     "Bigram" = "Bi",
                     "Trigram" = "Tri")),
      h6("Powered by"),
      tags$img(src = 'foto25.png', height = 50, width = 50)

      
     ),
    
    
    mainPanel(
      tabsetPanel(id = "panel",type = ("tab"),
                  tabPanel("Print", verbatimTextOutput("texto")),
                  tabPanel("Freq Table", tableOutput("tabla")),
                  tabPanel("Histogram", plotOutput("frecuencias")),
                  tabPanel("Networks", plotOutput("k")),
                  tabPanel("MLA",
                           tabsetPanel(
                            tabPanel("LDA",
                                    fluidRow(
                                      column(width = 6,
                                             plotOutput("demo"))
                                    )),
                            tabPanel("K-Means",
                                     fluidRow(
                                       column(width = 6,
                                              plotOutput("KMIN") 
                                       ) 
                                     )
                            )
                           ) #Close inner tabsetPanel
                  ),
                           
                           
                       
                  tabPanel("Sentiment",
                           tabsetPanel(
                             tabPanel("Term-Matrix",
                                      fluidRow(
                                        column(width = 6,
                                               tableOutput("sentT"))
                                        
                                      )),
                             tabPanel("Plot",
                                      fluidRow(
                                        column(width = 6,
                                               plotOutput("sentH") 
                                        ) 
                                      )
                             )
                           ) #Close inner tabsetPanel
                  ),
                  #tabPanel("Word Cloud", wordcloud2Output("mapaw")),
                 
                  
                  
                  
                  #tabPanel("Ngram", plotOutput()
                  
                  
                  
                  
                  
                  
                  
                  
      )
    )
  )
)
)











