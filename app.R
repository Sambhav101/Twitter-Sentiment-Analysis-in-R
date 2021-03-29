library(shiny)
library(shinycssloaders)

#import libraries and files
source("helpers.R")
source("text.R")
source("tweet.R")

# Define UI ----
ui <- navbarPage(
  "Sentiment Classification",
  div(
    includeCSS("www/app.css", class = "css"),
    shinyjs::useShinyjs(),
  ),        
  selected = "Twitter",
  collapsible = TRUE,
  
  tabPanel("Twitter",
           sidebarLayout(
             sidebarPanel(
                      helpText("Enter any topic and view what people are talking about it in twitter"),
                      
                      br(),
                      
                      textInput("topic",
                                label = NULL,
                                value = "",
                                placeholder = "Type something ..."),
                      br(),
                      
                      sliderInput("range",
                                  label="search from top n tweets",
                                  min = 0,
                                  max = 1000,
                                  value = 100,
                                  step = 200),
                      actionButton("search", "Search"),
                      br(),
                      hr(id = "line"),
                      radioButtons("sentiments",
                                   label="Select Plots",
                                   choices = c("Wordcloud" = "cloud",
                                               "Bubble" = "bubble",
                                               "Top words" = "top",
                                               "Sentiment" = "bing",
                                               "Contribution" = "afinn",
                                               "Emotions" = "emotion"),
                                   selected = "cloud")
             ),
             mainPanel(
                 div(id = "cloud", wordcloud2Output("comparisoncloud") %>% withSpinner(color="#0dc5c1")),
                 div(id = "plots", plotOutput("sentimentplots") %>% withSpinner(color="#0dc5c1")),
             )
           )
  
    ),
  
  tabPanel("Text",
           div( id = "text-container",
             wellPanel(
               helpText("Find out the sentiments and information about any text"),
               
               br(),
               textAreaInput("textarea",
                             label="Type or Paste your text inside the textfield",
                             value = "",
                             width = "70%",
                             height = "50%",
                             placeholder = "type something..."),
               actionButton("analyze", "Analyze text"),
             )
           ),
           
           div( id = "text-plots",
            div( class = "text-plot",
              plotOutput("topwords") %>% withSpinner(color="#0dc5c1"),
              ),
             div ( class = "text-plot",
               plotOutput("bingplot"),
             ),
             div( class = "text-plot",
               plotOutput("afinnplot"),
             ),
             div( class = "text-plot",
               plotOutput("nrcplot")
             )
           )
  ),
  tabPanel("URL"),
  tabPanel("Books")
)


# Define server logic ----
server <- function(input, output) {
  
  observe({
    if (input$sentiments != "cloud") {
      shinyjs::show("sentimentplots")
      shinyjs::hide("comparisoncloud")
      shinyjs::show("plots")
      shinyjs::hide("cloud")
    } else {
      shinyjs::hide("sentimentplots")
      shinyjs::show("comparisoncloud")
      shinyjs::hide("plots")
      shinyjs::show("cloud")
    }
  })
  
  
  # ************************** Twitter Tab ********************************#
  
  # get tweet data on button click
  tweet_data <- eventReactive(input$search, {
      get_tweets_data(input$topic, input$range)
  })
  
  # generate wordcloud of tweet_data
  output$comparisoncloud <- renderWordcloud2({
    generate_wordcloud2(tweet_data())
  })
  
  # get sentiment data (positive and negative)
  bing_sentiment_data <- reactive({
    bing_sentiments(tweet_data())
  })
  
  # get tweet-data with emotions
  emotion_data <- reactive({
    emotion_sentiments(tweet_data()) 
  })
  
  # get tweet_data with positive and negative values assigned
  afinn_data <- reactive({
    afinn_sentiments(tweet_data())
  })
  
  # generate plots according to radio buttons
  output$sentimentplots <- renderPlot({
    switch(input$sentiments,
           bubble = generate_bubbles(tweet_data()),
           top = top_words(tweet_data()),
           bing = bar_sentiments(bing_sentiment_data()),
           afinn = pn_plot(afinn_data()),
           emotion = bar_sentiments(emotion_data()))
  })
  
  
  
  # **************************** Text Tab **************************#
  
  # get data from text on botton click
  text_data <- eventReactive(input$analyze, {
    get_text_data(input$textarea)
  })
  
  # get sentiment data (positive and negative)
  bing_data <- reactive({
    bing_sentiments(text_data())
  })
  
  # get tweet-data with emotions
  nrc_data <- reactive({
    emotion_sentiments(text_data()) 
  })
  
  # get tweet_data with positive and negative values assigned
  afinn_pn_data <- reactive({
    afinn_sentiments(text_data())
  })
  
  output$topwords <- renderPlot({
    top_words(text_data())
  })
  
  output$bingplot <- renderPlot({
    bar_sentiments(bing_data())
  })
  
  output$afinnplot <- renderPlot({
    pn_plot(afinn_pn_data())
  })
  
  output$nrcplot <- renderPlot({
    bar_sentiments(nrc_data())
  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)