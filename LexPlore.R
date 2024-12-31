# Load necessary libraries
library(shiny)
library(shinythemes) # For colorful themes
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(DT) # For interactive tables

# Shiny App
ui <- fluidPage(
  theme = shinytheme("lumen"), # Apply a colorful theme
  titlePanel("Lexplore: Your Text Analysis Companion"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a Text File", accept = ".txt"),
      textAreaInput("text", "Or Paste Text Below:", value = "", rows = 5),
      sliderInput("top_n", "Select Top N Words to Display:", min = 5, max = 50, value = 20),
      actionButton("analyze", "Analyze Text", class = "btn btn-primary"),
      hr(),
      h3(" or Analyze my favorite poem by Blake William"),
      actionButton("default", "Load Poem", class = "btn btn-success")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Frequency", plotOutput("word_freq_plot")),
        tabPanel("Sentiment Analysis", plotOutput("sentiment_plot")),
        tabPanel("Word Cloud", plotOutput("wordcloud")),
        tabPanel("Top Keywords", DTOutput("top_keywords"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to track text source
  text_source <- reactiveVal(NULL)
  
  # Default Poem as a String
  default_poem <- c(
    "Tyger Tyger, burning bright,",
    "In the forests of the night;",
    "What immortal hand or eye,",
    "Could frame thy fearful symmetry?",
    "",
    "In what distant deeps or skies.",
    "Burnt the fire of thine eyes?",
    "On what wings dare he aspire?",
    "What the hand, dare seize the fire?",
    "",
    "And what shoulder, & what art,",
    "Could twist the sinews of thy heart?",
    "And when thy heart began to beat.",
    "What dread hand? & what dread feet?",
    "",
    "What the hammer? what the chain,",
    "In what furnace was thy brain?",
    "What the anvil? what dread grasp.",
    "Dare its deadly terrors clasp?",
    "",
    "When the stars threw down their spears",
    "And watered heaven with their tears:",
    "Did he smile his work to see?",
    "Did he who made the Lamb make thee?",
    "",
    "Tyger Tyger burning bright,",
    "In the forests of the night:",
    "What immortal hand or eye,",
    "Dare frame thy fearful symmetry?"
  )
  
  # Trigger Analysis when Default Poem is Loaded
  observeEvent(input$default, {
    text_source(default_poem)
    updateActionButton(session, "analyze", label = "Analyze")
  })
  
  # Observe File Upload
  observeEvent(input$file, {
    if (!is.null(input$file)) {
      text_source(readLines(input$file$datapath))
    }
  })
  
  # Observe Pasted Text
  observeEvent(input$text, {
    if (nchar(input$text) > 0) {
      text_source(strsplit(input$text, "\n")[[1]])
    }
  })
  
  # Reactive Analysis
  analysis <- reactive({
    text <- text_source()
    if (is.null(text)) return(NULL)
    
    text_df <- tibble(line = 1:length(text), text = text)
    tokenized <- text_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word")
    
    word_freq <- tokenized %>%
      count(word, sort = TRUE)
    
    sentiment <- tokenized %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(sentiment, sort = TRUE)
    
    list(word_freq = word_freq, sentiment = sentiment)
  })
  
  # Word Frequency Plot with Number Labels
  output$word_freq_plot <- renderPlot({
    data <- analysis()
    if (is.null(data)) return(NULL)
    top_n <- input$top_n
    ggplot(data$word_freq[1:top_n, ], aes(x = reorder(word, n), y = n)) +
      geom_bar(stat = "identity", fill = "cornflowerblue") +
      geom_text(aes(label = n), hjust = -0.2, size = 4, color = "black") +
      coord_flip() +
      labs(
        title = paste("Top", top_n, "Words by Frequency"),
        x = "Words",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12), title = element_text(size = 14))
  })
  
  # Sentiment Plot with Number Labels
  output$sentiment_plot <- renderPlot({
    data <- analysis()
    if (is.null(data)) return(NULL)
    ggplot(data$sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), vjust = -0.5, size = 4, color = "black") +
      scale_fill_manual(values = c("positive" = "seagreen3", "negative" = "tomato")) +
      labs(
        title = "Sentiment Analysis",
        x = "Sentiment",
        y = "Count"
      ) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12), title = element_text(size = 14))
  })
  
  # Word Cloud
  output$wordcloud <- renderPlot({
    data <- analysis()
    if (is.null(data)) return(NULL)
    wordcloud(
      words = data$word_freq$word,
      freq = data$word_freq$n,
      min.freq = 1,
      max.words = input$top_n,
      colors = brewer.pal(8, "Set2")
    )
  })
  
  # Top Keywords Table
  output$top_keywords <- renderDT({
    data <- analysis()
    if (is.null(data)) return(NULL)
    datatable(data$word_freq[1:input$top_n, ], options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
