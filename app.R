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
                      hr(id = "line"),
                      radioButtons("sentiments",
                                   label="Select Plots",
                                   choices = c("wordcloud" = "cloud",
                                               "top unique words" = "top",
                                               "Positive to negative ratio" = "bing",
                                               "top sentiment words" = "afinn",
                                               "emotion sentiment" = "emotion"),
                                   selected = "cloud")
             ),
             mainPanel(
                 div(id = "cloud", wordcloud2Output("comparisoncloud") %>% withSpinner(color="#0dc5c1")),
                 div(id = "plots", plotOutput("sentimentplots") %>% withSpinner(color="#0dc5c1")),
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
           top = top_words(tweet_data()),
           bing = bar_sentiments(bing_sentiment_data()),
           afinn = pn_plot(afinn_data()),
           emotion = bar_sentiments(emotion_data()))
  })
  
  
  
  # **************************** Text Tab **************************#
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
