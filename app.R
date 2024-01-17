# andrew.corbin.ngs@gmail.com


library(shiny)
library(tm)
library(wordcloud)
library(SnowballC)
library(shinythemes)


ui <- shinyUI(fluidPage(theme = shinytheme("yeti"),
  titlePanel("Forecast: cloudy with a chance of words."),
  sidebarLayout(
    sidebarPanel(
      fileInput("wc", "Upload a test file to generate a wordcloud.", multiple = F, accept = "text/plain"),
      actionButton("update", "Generate Word Cloud"),
      downloadButton("downloadBtn", "Download Word Cloud")
    ),
    mainPanel(
      plotOutput("wcplot")
    )
  )
)
)


server <- shinyServer(function(input, output) {
  
  wc_data <- reactive({
    
    input$update
    
    isolate({
      
      withProgress({
        setProgress(message = "Processing a cloud of word to make it rain letters...")
        wc_file <- input$wc
        if(!is.null(wc_file)){
          wc_text <- readLines(wc_file$datapath)
        }
        else
        {
          wc_text <- "A word cloud is an image made of words that together resemble 
          a cloud. The size of the word inidicates how often the word is represented 
          in the input text (frequency). People make these for reports and stuff, 
          but I usually just make them for fun. Try your favorite book of the bible, 
          or emails from your boss lol..."
        }
        wc_corpus <- Corpus(VectorSource(wc_text))
        wc_corpus_clean <- tm_map(wc_corpus, tolower)
        wc_corpus_clean <- tm_map(wc_corpus_clean, removeNumbers)
        wc_corpus_clean <- tm_map(wc_corpus_clean, removeWords, stopwords())
        wc_corpus_clean <- tm_map(wc_corpus_clean, stripWhitespace)
        wc_corpus_clean <- tm_map(wc_corpus_clean, removePunctuation)
        
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wcplot <- renderPlot({
    withProgress({
      setProgress(message = "Making a word cloud!")
      wc_corpus <- wc_data()
      wordcloud(wc_corpus, min.freq = 2, colors = brewer.pal(8, "Set1"), random.order = F, rot.per = .20)
    })
  })
  
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste("wordcloud", ".png", sep = "")
    },
    content = function(file) {
      png(file)
      wordcloud(wc_data(), min.freq = 2, colors = brewer.pal(8, "Set1"), random.order = F, rot.per = .20)
      dev.off()
    }
  )
  
})


shinyApp(ui = ui, server = server)