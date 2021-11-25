## app.R ##
library(shinydashboard)
library(shiny)
library(plotly)
library(plyr)
library(dplyr)
library(ggplot2)
library(datasets)
library(tidyverse)
library(leaflet)

evs <- read.csv('C:/Users/Administrator/Desktop/ElectricCarData_Clean.csv')
price <- evs[, c(14)]
acc <- evs[,c(3)]

Rapidcharge_price <- evs[, c(8, 14)] 
Rapidcharge_price <- Rapidcharge_price[order(Rapidcharge_price$PriceEuro), ] 
avg_price <- aggregate(Rapidcharge_price$PriceEuro ~ Rapidcharge_price$RapidCharge, FUN = "mean") 
avg_price <- avg_price %>% rename(RapidCharge = `Rapidcharge_price$RapidCharge`, Avg_Price = `Rapidcharge_price$PriceEuro`) 
less_than_avg <- Rapidcharge_price %>% filter(PriceEuro < avg_price$Avg_Price[2]) 
yes_percentage <- round((nrow(less_than_avg %>% filter(RapidCharge == "Yes")) /nrow(less_than_avg)) * 100, 2) 
no_percentage <- round(100 - yes_percentage, 2) 
percentages <- data.frame(rapidcharge = c("Yes", "No"), percentage = rbind("1" = yes_percentage,"2" = no_percentage)) 
less_than_25 <- Rapidcharge_price %>% filter(PriceEuro < 25000) 
yes_percentage1 <- round((nrow(less_than_25 %>% filter(RapidCharge == "Yes")) /nrow(less_than_25)) * 100, 2) 
no_percentage1 <- round(100 - yes_percentage1, 2) 
percentages_2 <- data.frame(rapidcharge = c("Yes", "No"), percentage = rbind("1" = yes_percentage1,"2" = no_percentage1)) 

head(percentages_2) 

colors <- c('rgb(5, 193, 230)', 'rgb(255, 48, 124)')

data <- read.csv("C:/Users/Administrator/Desktop/Dataset.csv")


data$Date <- strptime(as.character(data$Date.yyyy.MM.dd.),format="%m/%d/%Y")
data$Date <- as.POSIXct(data$Date)

data$DateTime <- strptime(as.character(data$DateTime),format="%m/%d/%Y %H:%M")
data$DateTime <- as.POSIXct(data$DateTime)

data$Day <- as.numeric(as.character(strftime(data$DateTime,format="%d")))
data$Hour <- as.numeric(as.character(strftime(data$DateTime,format="%H")))

data <- data %>% filter(BC6!=0)

ui <- dashboardPage(skin="purple",
                    dashboardHeader(title = "Review 3",
                                    tags$li(class="dropdown",tags$a(href="https://github.com/ananyasingh13",icon("github"),"Ananya's Github", target="_blank")),
                                    tags$li(class="dropdown",tags$a(href="https://github.com/Shivamraj-hub",icon("github"),"Shivam's Github", target="_blank"))),
                    dashboardSidebar(sidebarMenu(
                       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                       menuItem("Data 1", tabName = "data1", icon = icon("database")),
                       menuItem("Interactivity", tabName = "Interactivity", icon = icon("bar-chart")),
                       menuItem("Data 2", tabName = "data2", icon = icon("database")),
                       menuItem("Aggregates", tabName = "aggregate", icon = icon("box")),
                       
                       menuItem("Charts", 
                                icon = icon("line-chart"),
                                menuSubItem("graph1",tabName = "chart1", icon = icon('line-chart')),
                                menuSubItem("graph2",tabName = "chart2", icon = icon('line-chart')),
                                menuSubItem("graph2a",tabName = "chart2a", icon = icon('line-chart')),
                                
                                menuSubItem("graph3",tabName = "chart3", icon = icon('line-chart')),
                                menuSubItem("graph4",tabName = "chart4", icon = icon('line-chart')),
                                menuSubItem("graph5",tabName = "chart5", icon = icon('line-chart'))),
                       menuItem("Link to code files", href = "https://github.com/ananyasingh13/Data-visualization-Project", icon = icon("code"))
                    )),
                    dashboardBody(
                       
                       tabItems(
                          
                          tabItem(tabName = "Interactivity",selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                                                                        selected = "Blue",multiple = F),
                                  radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
                                  
                                  selectInput(inputId="channel1",label="Choose Channel",choices = c("BC1"="BC1",
                                                                                                    "BC2"="BC2",
                                                                                                    "BC3"="BC3",
                                                                                                    "BC4"="BC4",
                                                                                                    "BC5"="BC5",
                                                                                                    "BC6"="BC6",
                                                                                                    "BC7"="BC7"),
                                              selected = "BC6",multiple = F),
                                  
                                  sliderInput(inputId = "bins1xz",
                                              label = "Number of bins:",
                                              min = 1,
                                              max = 50,
                                              value = 30),
                                  
                                  sliderInput(inputId = "range1",
                                              label = "Data Range",
                                              min = 1,
                                              max = 31,
                                              value = c(1,31)),
                                  fluidRow(box( width=12,
                                                plotOutput(outputId = "distPlot"),
                                                plotOutput(outputId = "distPlot1"),
                                                plotOutput(outputId = "distPlot2")))),
                          
                          tabItem(tabName = "dashboard",fluidRow(box(title='Map of VIT Vellore Campus',status="success",background = "green")),leafletOutput("map"),
                                  fluidRow(box(title='Dashboard Creation using R ','Made by Shivam Raj and Ananya Singh',status="success",background = "green"))),
                          
                          tabItem(tabName = "data2",dataTableOutput("mydatatable")),
                          tabItem(tabName = "data1",dataTableOutput("datatable")),
                          
                          tabItem(tabName = "aggregate", 
                                  fluidRow(infoBoxOutput("max", width=6), infoBoxOutput("min",width=6)),
                                  fluidRow(infoBoxOutput("max1", width=6), infoBoxOutput("min1",width=6))),
                          
                          
                          tabItem(tabName = "chart1",fluidRow(box(title='Relationship between Power Train and Range of the EV', 
                                                                  plotlyOutput("one"), width=12, height=500,status = "primary")),
                                  fluidRow(box(title="interpretation",status="success",width=10, background = "black", "The above graph shows the relation between Range of the cars in Km vs the Power train. This graph is an interactive scatterplot where we can hover around our cursor to see a particular entry or we can zoom into the plot or select a particular region and also take snapshots. It gives us an idea of how the range of Different EMVs vary with model."))),
                          
                          tabItem(tabName = "chart2",fluidRow(box(title='Relation between price and rapid charging', 
                                                                  plotlyOutput("two"), width=12,status = "primary")),
                                  fluidRow(box(title="interpretation",status="success",width=10, background = "black","This plot shows the relation between Price of the car and weather it has rapid charging or not. It also indicates the average for both the choices. This interactive plot helps user choose wisely as there is an arrow pointer which they can use to select particular price ranged cars. User can also zoom in to the graph for better clarity of number of options and also they can snip or capture the plot
 "))),
                          
                          
                          
                          tabItem(tabName = "chart3",fluidRow(box(title='Quickest 0-100 km/h in an EV', 
                                                                  plotlyOutput("three"), width=12, status = "primary")),
                                  fluidRow(box(title="interpretation",status="success",width=10, background = "black","The above interactive bar plot is mainly for speed enthusiasts as people have this misconception of weather EVs can deliver the power and speed that normal fuel vehicles do. So by this interactive graph users can easily go for cars which have faster accelerations with all interactive functions in it."))),
                          
                          tabItem(tabName = "chart4",fluidRow(box(title='Efficiencies of EVs', 
                                                                  plotlyOutput("four"), width=12, status = "primary")),
                                  fluidRow(box(title="interpretation",status="success",width=10, background = "black","The above interactive bar plot is mainly for people who look for an economic drive as people have this misconception of whether EVs can deliver the same kind of fuel economy fuelled cars do. So by this interactive graph users can easily go for cars which have good economy with all interactive functions in it.
 "))),
                          
                          tabItem(tabName = "chart5",fluidRow(box(title='Relationship between Power Train and Top Speed of the EV', 
                                                                  plotlyOutput("five"), width=12, status = "primary")),
                                  fluidRow(box(title="interpretation",status="success",width=10, background = "black","The above plot shows the top speed of an EV in accordance to its Power Train Technology. We can see that AWD type have the max top speeds. Thus they are better choices. User can hover their cursor to the spots to get the clear idea of which power train will will go upto highest speeds.
 ")))
                          
                          
                       )
                    )
)

server <- function(input, output) {
   set.seed(122)
   output$disPlot <- renderPlot({plot(rnorm(input$n), rnorm(input$n))})
   output$map<- renderLeaflet({leaflet() %>% addTiles() %>% setView(79.1594 , 12.9717, zoom=16)})
   output$one <- renderPlotly({plot_ly(data=evs, x=~Range_Km, y= ~PowerTrain, type="scatter",color=~PowerTrain, mode="markers")})
   output$two <- renderPlotly({plot_ly(data=evs, x= ~PriceEuro, y= ~RapidCharge, color=~RapidCharge ,type="scatter")})
   
   
   output$three <- renderPlotly({plot_ly(evs, x = ~Model, y = ~AccelSec, type ='bar')})
   output$four <- renderPlotly({plot_ly(evs,  x = ~Model, y = ~Efficiency_WhKm, type ='bar')})
   output$five <- renderPlotly({plot_ly(evs, x = ~TopSpeed_KmH, y = ~PowerTrain, type="scatter",color=~PowerTrain, mode="markers")})
   
   output$min <- renderInfoBox({infoBox(title = "Minimum Price",value=min(price),subtitle = "Minimum Price of EMV in Euro", fill=TRUE)})
   output$max <- renderInfoBox({infoBox(title = "Maximum Price",value=max(price),subtitle = "Maximum Price of EMV in Euro", fill=TRUE)})
   output$min1 <- renderInfoBox({infoBox(title = "Minimum Acc/sec",value=min(acc),subtitle = "Minimum acceleration/sec from all EMV in m/s", fill=TRUE, color="yellow")})
   output$max1 <- renderInfoBox({infoBox(title = "Maximum Acc/sec",value=max(acc),subtitle = "Maximum acceleration/sec from all EMV in m/s", fill=TRUE, color="yellow")})
   
   output$mydatatable <- renderDataTable({evs})
   
   output$datatable <- renderDataTable({data})
   
   output$distPlot <- renderPlot({
      
      if(input$color1=="Red"){
         sColor = "#ff3300"
      }else if(input$color1=="Blue"){
         sColor = "#3399ff"
      }else if(input$color1=="Green"){
         sColor = "#66ff33"
      }
      
      p2 <- data %>%  filter(Day >= input$range1[1] & Day <= input$range1[2]) %>% ggplot()
      if(input$channel1 == "BC1"){
         p2 <- p2 + geom_histogram(aes(x=BC1),bins = input$bins1xz,col=input$border1,fill=sColor)
      }else if(input$channel1 == "BC2"){
         p2 <- p2 + geom_histogram(aes(x=BC2),bins = input$bins1xz,col=input$border1,fill=sColor)
      }else if(input$channel1 == "BC3"){
         p2 <- p2 + geom_histogram(aes(x=BC3),bins = input$bins1xz,col=input$border1,fill=sColor)
      }else if(input$channel1 == "BC4"){
         p2 <- p2 + geom_histogram(aes(x=BC4),bins = input$bins1xz,col=input$border1,fill=sColor)
      }else if(input$channel1 == "BC5"){
         p2 <- p2 + geom_histogram(aes(x=BC5),bins = input$bins1xz,col=input$border1,fill=sColor)
      }else if(input$channel1 == "BC6"){
         p2 <- p2 + geom_histogram(aes(x=BC6),bins = input$bins1xz,col=input$border1,fill=sColor)
      }else if(input$channel1 == "BC7"){
         p2 <- p2 + geom_histogram(aes(x=BC7),bins = input$bins1xz,col=input$border1,fill=sColor)
      }
      p2 <- p2 +  theme_bw()+
         theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
               axis.text = element_text(size=14,color="BLACK",face="bold"))+
         labs(x="Black Carbon (ng/m3)",y="Count",title=paste(" Histogram showing Black Carbon Concentration",input$channel1,sep = " "))
      
      p2
      #hist(x, breaks = bins, col = sColor, border = input$border1,
      #     xlab = "Waiting time to next eruption (in mins)",
      #     main = "Histogram of waiting times")
   })
   
   output$distPlot1 <- renderPlot({
      
      p1 <- data  %>%  filter(Day >= input$range1[1] & Day <= input$range1[2]) %>% ggplot(aes(x=DateTime))
      if(input$channel1 == "BC1"){
         p1 <- p1 + geom_line(aes(y=BC1,col="BC1"),size=0.5)
      }else
         if(input$channel1 == "BC2"){
            p1 <- p1 + geom_line(aes(y=BC2,col="BC2"),size=0.5)
         }else
            if(input$channel1 == "BC3"){
               p1 <- p1 + geom_line(aes(y=BC3,col="BC3"),size=0.5)
            }else
               if(input$channel1 == "BC4"){
                  p1 <- p1 + geom_line(aes(y=BC4,col="BC4"),size=0.5)
               }else
                  if(input$channel1 == "BC5"){
                     p1 <- p1 + geom_line(aes(y=BC5,col="BC5"),size=0.5)
                  }else
                     if(input$channel1 == "BC6"){
                        p1 <- p1 + geom_line(aes(y=BC6,col="BC6"),size=0.5)
                     }else
                        if(input$channel1 == "BC7"){
                           p1 <- p1 + geom_line(aes(y=BC7,col="BC7"),size=0.5)
                        }
      p1 <- p1 +  theme_bw()+
         theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
               axis.text = element_text(size=14,color="BLACK",face="bold"))+
         labs(x="Time",y="Black Carbon (ng/m3)",title="Variation in Black Carbon Concentration in Air for year 2017",colour="Channel")
      
      p1
      
   })
   
   output$distPlot2 <- renderPlot({
      d <- data  %>%  filter(Day >= input$range1[1] & Day <= input$range1[2])
      
      d <- ddply(d, .variables = c("Hour"),function(x){
         
         BC1avg <- mean(x$BC1,na.rm = T)
         BC2avg <- mean(x$BC2,na.rm = T)
         BC3avg <- mean(x$BC3,na.rm = T)
         BC4avg <- mean(x$BC4,na.rm = T)
         BC5avg <- mean(x$BC5,na.rm = T)
         BC6avg <- mean(x$BC6,na.rm = T)
         BC7avg <- mean(x$BC7,na.rm = T)
         
         data.frame(BC1avg,BC2avg,BC3avg,BC4avg,BC5avg,BC6avg,BC7avg)
      })
      
      p1 <- d %>% ggplot(aes(x=Hour))
      if(input$channel1 == "BC1"){
         p1 <- p1 + geom_line(aes(y=BC1avg,col="BC1"),size=1)
         p1 <- p1 + geom_point(aes(y=BC1avg))
      }else if(input$channel1 == "BC2"){
         p1 <- p1 + geom_line(aes(y=BC2avg,col="BC2"),size=1)
         p1 <- p1 + geom_point(aes(y=BC2avg))
      }else if(input$channel1 == "BC3"){
         p1 <- p1 + geom_line(aes(y=BC3avg,col="BC3"),size=1)
         p1 <- p1 + geom_point(aes(y=BC3avg))
      }else if(input$channel1 == "BC4"){
         p1 <- p1 + geom_line(aes(y=BC4avg,col="BC4"),size=1)
         p1 <- p1 + geom_point(aes(y=BC4avg))
      }else if(input$channel1 == "BC5"){
         p1 <- p1 + geom_line(aes(y=BC5avg,col="BC5"),size=1)
         p1 <- p1 + geom_point(aes(y=BC5avg))
      }else if(input$channel1 == "BC6"){
         p1 <- p1 + geom_line(aes(y=BC6avg,col="BC6"),size=1)
         p1 <- p1 + geom_point(aes(y=BC6avg))
      }else if(input$channel1 == "BC7"){
         p1 <- p1 + geom_line(aes(y=BC7avg,col="BC7"),size=1)
         p1 <- p1 + geom_point(aes(y=BC7avg))
      }
      p1 <- p1 +  theme_bw()+
         theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
               axis.text = element_text(size=14,color="BLACK",face="bold"))+
         labs(x="Time",y="Black Carbon (ng/m3)",title="Mean of Black Carbon Concentration in Air - Average Diurnal Variation- 2017",colour="Channel")
      
      p1
      
   })
   
   
}

shinyApp(ui, server)


