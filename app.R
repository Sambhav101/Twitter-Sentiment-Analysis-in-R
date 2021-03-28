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
  
  tabPanel("Twitter",
           sidebarLayout(
             
             sidebarPanel(
                      helpText("Select any trending topic and view what people are talking about in twitter"),
                      
                      br(),
                      
                      textInput("topic",
                                label = NULL,
                                value = "",
                                placeholder = "Type something ..."),
                      br(),
                      
                      sliderInput("range",
                                  label="search from last n tweets",
                                  min = 0,
                                  max = 1000,
                                  value = 100,
                                  step = 200),
                      
                      actionButton("search", "Search"),
                      
                      br(),
                      hr(),
                      checkboxInput("sentiments_tweet",
                                label="View Sentiments",
                                value=FALSE)
             ),
             
             mainPanel(
               wordcloud2Output("comparisoncloud") %>% withSpinner(color="#0dc5c1"),
               shinyjs::hidden(div(
                 id="sentimentplots",
                 plotOutput("topwordsplot")  %>% withSpinner(color="#0dc5c1"),
                 plotOutput("bingsentimentplot"),
                 plotOutput("topsentimentwords"),
                 plotOutput("emotionplot")
               ))
             )
             
           )
  
    ),
  
  tabPanel("Text",
           fluidRow(
             wellPanel(
               helpText("Find out the sentiments and information about any text"),
               
               br(),
               textAreaInput("textarea",
                             label="Type or Paste your text inside the textfield",
                             value = "",
                             width = "80%",
                             height = "50%",
                             placeholder = "type something..."),
               br(),
               hr(),
               checkboxInput("sentiments",
                             label="View Sentiments",
                             value=TRUE)
               
             )
           ),
           
           fluidRow(
             plotOutput("text_output")
           )
  ),
  tabPanel("URL"),
  tabPanel("Books")
)

# Define server logic ----
server <- function(input, output) {
      shinyjs::onclick("sentiments_tweet",
                      shinyjs::toggle(id = "sentimentplots", anim = TRUE) &
                      shinyjs::toggle(id = "comparisoncloud", anim = TRUE)
                      )
  
  
                   
  
  
  # ************************** Twitter Tab ********************************#
  
  # get tweet data on button click
  tweet_data <- eventReactive(input$search, {
    get_tweets_data(input$topic, input$range)
  })
  
  output$comparisoncloud <- renderWordcloud2({
    generate_wordcloud2(tweet_data())
  })
  
  
  # get top 20 words from the tweet
  output$topwordsplot <- renderPlot({
      top_words(tweet_data())
   
  })
  
  # get sentiment data (positive and negative)
  bing_sentiment_data <- reactive({
    bing_sentiments(tweet_data())
  })
  
  # get tweet-data with emotions
  emotion_data <- reactive({
    emotion_sentiments(tweet_data()) 
  })
  
  # get sentimenet bar plot 
  output$bingsentimentplot <- renderPlot({
    bar_sentiments(bing_sentiment_data())
  })
  
  afinn_data <- reactive({
    afinn_sentiments(tweet_data())
  })
  
  output$topsentimentwords <- renderPlot({
    pn_plot(afinn_data())
  })
  
  # get sentiment emotion plot
  output$emotionplot <- renderPlot({
    bar_sentiments(emotion_data())
  })
  
  
  # **************************** Text Tab **************************#
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
