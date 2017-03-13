library(shiny)
require(devtools)
library(rCharts)
## app.R ##
library(shinydashboard)
base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19"



ui <- dashboardPage(
  dashboardHeader(title = "Tonitude"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Compare Aritcles by Tone Used", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Tone Definitions  ", tabName = "tonedef", icon =icon("list-alt", lib = "glyphicon")),
      menuItem("How to Use ", tabName = "howto", icon =icon("cog", lib = "glyphicon"))
      
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard", 
              
              fluidPage(
                
                h4("First Article To Compare ... "),
                tags$style(type="text/css", "textarea {width:100%}"),
                tags$textarea(id = 'input_text1', placeholder = 'Copy Paste Article Text Here', rows = 8, ""),
                
                h4("Second Article To Compare ... "),
                tags$style(type="text/css", "textarea {width:100%}"),
                tags$textarea(id = 'input_text2', placeholder = 'Copy Paste Article Text Here', rows = 8, "")
                ,
               
                
                div(actionButton("Go", "Compare the tone of the Articles  ~ / ~" ,style = " color:blue; text-align: center; margin-top: 30px; width: 100%;")),
                
                div(style = 'overflow-x: scroll', showOutput("myChart", "nvd3")),
                
                div(style = 'overflow-x: scroll',tableOutput('mytable')) 
              
                  
              )
      ),
      
      # Second tab content
      tabItem(tabName = "howto",
              h2("How to Use the Tonitude App to compare the tone of written Articles."),
              div("The Tonitude app can be used to calculate the emotional tone of an written article with the emotional tone of another written article.", style = "color:blue"),
              div(strong("Step 1.", style = "color:black")),
              p("Copy paste the First written text into the Area provided"),
              img(src = "s1.png", height = "100%", width = "100%"),
              div(strong("Step 2.", style = "color:black")),
              p("Copy paste the Second written text into the Area provided"),
              img(src = "s2.png", height = "100%", width = "100%"),
              div(strong("Step 3.", style = "color:black")),
              p("Please Click 'Compare the Tone of Articles' Button"),
              img(src = "s3.png", height = "100%", width = "100%"),
              div(strong("Step 5.", style = "color:black")),
              p("Scroll down to View Multi bar chart comparison and table comparison of the emotional tone present in the two Articles"),
              img(src = "s4.png", height = "100%", width = "100%")
              
      ),
      
      # third tab content
      tabItem(tabName = "tonedef",
              h2("Different Types of Tones : "),
              br(),
              div("The Tonitude app calculates the following emotional tones of an written article. 
                  Emotional tone measures different types of emotions and feelings that people express.

                  For each emotion, a score of less 0.5 indicates that the emotion is unlikely to be perceived in the content. Likewise, a score greater than 0.75 indicates high likelihood that the emotion will be perceived.", style = "color:blue"),
              div(strong("Joy", style = "color:black")),
              p("Joy or happiness has shades of enjoyment, satisfaction and pleasure. There is a sense of well-being, inner peace, love, safety and contentment."),
              
              br(),
              div(strong("Fear", style = "color:black")),
              p("A response to impending danger. It is a survival mechanism that is a reaction to some negative stimulus. It may be a mild caution or an extreme phobia."),
     
              br(),
              div(strong("Sadness", style = "color:black")),
              p("Indicates a feeling of loss and disadvantage. When a person can be observed to be quiet, less energetic and withdrawn, it may be inferred that sadness exists."),
              
              br(),
              div(strong("Disgust", style = "color:black")),
              p("An emotional response of revulsion to something considered offensive or unpleasant. It is a sensation that refers to something revolting."),
              
              br(),
              div(strong("Anger", style = "color:black")),
              p("Evoked due to injustice, conflict, humiliation, negligence or betrayal. If anger is active, the individual attacks the target, verbally or physically. If anger is passive, the person silently sulks and feels tension and hostility."),
              
              
              br(),
              div("The Tonitude app calculates the following Scocial tones of an written article. 
                  Social tone measures the social tendencies in people's writing on five categories that are adopted from the Big Five personality model.
                  ", style = "color:blue"),
              div(strong("Openness", style = "color:black")),
              p("The extent a person is open to experience a variety of activities."),
              
              br(),
              div(strong("Conscientiousness", style = "color:black")),
              p("The tendency to act in an organized or thoughtful way."),
              
              br(),
              div(strong("Extraversion", style = "color:black")),
              p("The tendency to seek stimulation in the company of others."),
              
              br(),
              div(strong("Agreeableness", style = "color:black")),
              p("The tendency to be compassionate and cooperative towards others."),
              
              br(),
              div(strong("Emotional range or Neuroticism", style = "color:black")),
              p("The extent a person's emotion is sensitive to the environment."),
              
              
              br(),
              div("The Tonitude app calculates the following Language tones of an written article. 
                  Language tone describes perceived writing style.
                  ", style = "color:blue"),
              div(strong("Analytical", style = "color:black")),
              p("A person's reasoning and analytical attitude about things."),
              
              br(),
               div(strong("Confidence", style = "color:black")),
              p("A person's degree of certainty."),
              
              br(),
              div(strong("Tentative", style = "color:black")),
              p("A person's degree of inhibition.")
              
              
               )
    )
  )
)

server <- function(input, output, session) {
  library(RCurl) # install.packages("XML") # if the package is not already installed
  library(httr)
  library(XML)
  library(data.table)
  library(reshape2)
  library(tidyr)
  library(dplyr)
  library(sqldf)
  
  
  ### Function to process output from API and table
  tidyResponse <- function(data)
  {
    data <- as.data.frame(strsplit(as.character(data),"\"score\""))
    data <- data[-c(1), ] # remove dud first row
    data  <- gsub("\"tone_id\":","",data)
    data  <- gsub(":","",data)
    data  <- gsub("\"","",data)
    data  <- gsub("_big5","",data)
    data <- data.frame(data)
    data
    data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
    data <- data[,-c(3:6), ] # remove dud first row
    data <- data[c("X2", "X1")]
    data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
    data$X1 <- as.numeric(data$X1)
    data$X1 <- round((data$X1),2)
    setnames(data,c("trait","signal"))
    return(data)
  }
  
 
  
  observeEvent(input$Go, {
    text1<-input$input_text1
    text2<-input$input_text2
    
   if(input$input_text1 != "" || input$input_text2 != ""){
    
    response1 <- POST(url=base_url_TON,
                     authenticate("7fe7995d-8d87-4730-a178-309f411e1670","Swzej2GkIetP"),
                     add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                     body=text1 )
    
    response_text1 <- content(response1, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
    abc1 <- tidyResponse(response_text1)
    first<-sqldf("select trait, sum(signal)/count(signal)*100 as measure  from abc1
                  group by trait
                  order by measure desc")
    
    
    
    response2 <- POST(url=base_url_TON,
                     authenticate("7fe7995d-8d87-4730-a178-309f411e1670","Swzej2GkIetP"),
                     add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                     body=text2 )
    
    response_text2 <- content(response2, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
    abc2 <- tidyResponse(response_text2)
    second<-sqldf("select trait, sum(signal)/count(signal)*100 as measure  from abc2
                  group by trait
                  order by measure desc")
    
    result<-sqldf("select first.trait as Trait_of_the_Tone,
      first.measure as First_Article_Tone,
                  second.measure as Second_Article_Tone
                  from first left join second  on first.trait = second.trait
                  order by first.measure desc")
    
    
    output$myChart <- renderChart({
      t<-melt(result, id=c("Trait_of_the_Tone"))
      p6 <- nPlot( value ~ Trait_of_the_Tone , 
                   group = c("variable"), data = t, 
                   type = 'multiBarChart')
      #p6$chart(color = c('brown', 'blue', '#594c26', 'green'), stacked = input$stack)
      return(p6)
    })
    
  
    output$myChart <- renderChart({
      t<-melt(result, id=c("Trait_of_the_Tone"))
      p6<-nPlot( y="value" , x="Trait_of_the_Tone" , 
             group = "variable", data = t, 
             type = 'multiBarChart',
      width = session$clientData[["output_plot1_width"]])
      p6$addParams(dom = 'myChart')
      return(p6)
      
    })
    
    output$mytable <- renderTable(result)
   }
  })
  
  
  
  
 
}

shinyApp(ui, server)