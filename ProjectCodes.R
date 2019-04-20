#read data
pr <- read.csv("NYPD.csv")
 
#display columns
colnames(pr)

#head display and tail display
head(pr)
tail(pr)

#count missing values
sum(is.na(pr))

#ommiting missing values
pr <- na.omit(pr)
sum(is.na(pr))

head(pr)
tail(pr)

#plotly loading
library(plotly)

#finding the types of offence and their frequency, plotting
off <- table(pr$Offense)
off
off1 <- as.data.frame(off)
off1
plot_ly(x=off1$Var1, y = off1$Freq, type = "bar")

#finding offense rates based on borough
#first find the count of crimes based on boroughs
table(pr$Borough)
#analyse BURGLARY offense
br<- table(pr$Borough[pr$Offense == "BURGLARY"])
br
plot(br[3:7] , cex.axis = 0.7 , col = "blue" , xlab = "Borough" , ylab = "Count")
#use plotly to create a more interactive plot
br1 <- as.data.frame(br[3:7])
br1
plot_ly(x= br1$Var1, y= br1$Freq, type = "bar", color = I("cornflowerblue"))

#Crime counts based on Day of Week
day <- table(pr$Day.of.Week)
day

#plot the crime counts based on day of week
qplot(pr$Day.of.Week, geom = "bar", ylab = "Count of Crimes",
      xlab = "Days of Week" ,
      main = "Crime Rates on the basis of Days of Week", asp = 0.5,
      col = I("grey90"))


#create a more interactive plot using plotly
day1 <- as.data.frame(day)
day1
plot_ly(x = day1$Var1,y = day1$Freq, type = "bar", color = I("red"),
              name = "Crimes per day of week")



#shinyapp plotting days of week crime count & Burglary Offence
library(shiny)
plot1 <- function(){ plot_ly(x= br1$Var1, y= br1$Freq, type = "bar",
                             color = I("cornflowerblue"))
}
plot2 <- function(){ plot_ly(x = day1$Var1,y = day1$Freq, type = "bar",
                             color = I("red"),name = "Crimes per day of week")
}

ui <- fluidPage(
  titlePanel("Days of Weeks Or Burglary Offence Plot "),
  sidebarPanel("Selection",
               checkboxInput("Plotly", "Burglary"),
               verbatimTextOutput("conditionalInput"),
               uiOutput('conditionlInput')
  ),
  mainPanel("Output",
            plotlyOutput("AhtPlot")
  )
)

server <- function(input, output,session){
  
  
  output$AhtPlot<-renderPlotly({
    if(input$Plotly == TRUE)
    {plot1()} else {
      plot2()    
    }
  })
  
  
  output$conditionalInput <- renderText({input$Plotly})
  
}


shinyApp(ui, server)




