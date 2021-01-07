
library(shiny)
library(shinythemes)
library(ngram)
library(stringr)
library(NLP)
ngrams4<-readRDS("Data/ngrams4.rds")
ngrams3<-readRDS("Data/ngrams3.rds")
ngrams2<-readRDS("Data/ngrams2.rds")

ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Predict the next word"),
                sidebarLayout(
                    sidebarPanel(
                        h2("Input"),
                        textInput(inputId = "text", "" ,"")
                    ),
                    
                    # Output: Description, lineplot, and reference
                    mainPanel(
                        h2("Output"),
                        verbatimTextOutput("desc", placeholder = TRUE),
                        verbatimTextOutput("desc1", placeholder = TRUE),
                        verbatimTextOutput("desc2", placeholder = TRUE),
                        verbatimTextOutput("desc3", placeholder = TRUE),
                        tags$a(href = "https://github.com/mausam3407/Swiftkey-WordPredictor", "Click here for codes and data", target = "_blank",align="bottom")
                    )
                )
)



server <- function(input, output) {
    predict<-reactive({
        req(input$text)
        strin<-str_split(tolower(input$text)," ")
        strin<-strin[[1]]
        words<-paste("^",tolower(input$text),sep="")
        if (length(strin)>3){
            k<-tail(strin,3)
            k<-as.String(k)
            k<-as.character(k)
            k<-str_replace_all(k,"[^\'[a-z]]"," ")
            words<-paste("^",k,sep="")
        }
        if (wordcount(words)==1){
            m=grep(words,ngrams2)
            pred = c(ngrams2[m[1]],ngrams2[m[2]],ngrams2[m[3]],ngrams2[m[4]])
        }
        else if(wordcount(words)==2){
            m=grep(words,ngrams3)
            d<-str_split(words," ")
            d<-d[[1]]
            m1=grep(paste("^",d[2],sep=""),ngrams2)
            pred = c(ngrams2[m1[1]],ngrams2[m1[2]],ngrams3[m[1]],ngrams3[m[2]])
        }
        else if(wordcount(words)==3){
            m=grep(words,ngrams4)
            d<-str_split(words," ")
            d<-d[[1]]
            m1=grep(paste("^",d[3],sep=""),ngrams2)
            d1<-paste(d[2],d[3])
            m2=grep(paste("^",d1,sep=""),ngrams3)
            pred = c(ngrams2[m1[1]],ngrams2[m1[2]],ngrams3[m2[1]],ngrams4[m[4]])
        }}) 
    output$desc<-renderText({predict()[1]})
    output$desc1<-renderText({predict()[2]})
    output$desc2<-renderText({predict()[3]})
    output$desc3<-renderText({predict()[4]})
}

# Run the application 
shinyApp(ui = ui, server = server)
