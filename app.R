library(shiny)
library(dplyr)
library(reshape2)
library(forcats)
library(ggplot2)
library(stringr)
library(data.table)
Sys.setlocale("LC_TIME", "English")

dtRaw <- fread("berries_analysis.csv")
dt <- dtRaw

dt$Value <- str_remove_all(dt$Value,",")
dt$Value <- as.numeric(dt$Value)

dt$measure <- sub('.*MEASURED IN ', '', dt$`Data Item`)


production <- dt[str_detect(dt$`Data Item`,"PRODUCTION")&
                   dt$measure=="$",]
production <- production[!is.na(production$Value)&
                           production$Value!=0,]

procession <- dt[str_detect(dt$`Data Item`,"PROCESSING")&
                   dt$measure == "$ / LB",]
procession <- procession[!is.na(procession$Value),]

processionYM <- procession[,(meanPrice=mean(Value)),by=Year]

gpprd <- dt[str_detect(dt$`Data Item`,"PRODUCTION")&
              dt$measure=="LB",]
gpprdYS <- gpprd[,(sum(Value,na.rm = T)),
                 by=c("Year","State","Commodity")]
gpprdC <- gpprd[,(sum(Value,na.rm = T)),
                by=c("Year","Commodity")]
rasp <- gpprd[gpprd$Commodity=="RASPBERRIES"][,(sum(Value,na.rm = T)),
                                              by=c("Year","State")]
rasp <- dcast(rasp,Year~State,value.var = "V1")
rasp
# Define UI ----
ui <- fluidPage(
  titlePanel("Analysis of Berries Dataset"),
  verticalLayout(
    mainPanel(
      h3("1. Price of Production"),
      h4("1.2 Affect of Commodity"),
      h5("Confirm whether price of production were affect by commodity or not."),
      plotOutput("p1"),
      plotOutput("p2"),
      h5("The figures above indicates that strawberries are more expensive than the other two kinds of berries."),
      h4("1.3 Affect of State"),
      h5("Confirm whether price of production were affect by State or not."),
      plotOutput("p3"),
      h5("The figures above indicates that berries in California are more expensive than berries in other states."),
      h4("1.4 Affect of Year"),
      h5("Confirm whether price of production were affect by year or not."),
      plotOutput("p4"),
      h5("The figures above indicates that there are no different between price of berries in different years."),
      h3("2. Price of procession"),
      h4("2.1 Affect of Year"),
      plotOutput("p5"),
      h5("The figure and ANOVA model show that price of procession were affect by year."),
      h4("2.2 Trend"),
      plotOutput("p6"),
      h5("The price of berries procession are decreasing."),
      h3("3. Gross Product pf Production"),
      h4("3.1 Change of Gross Product of Procession"),
      h5("The decrease trend of gross product of raspberries procession is interesting."),
      plotOutput("p7"),
      h4("3.2 Change of Raspberries in Different States."),
      h5("Both of gross product of raspberries procession in California and Washington are decrease."),
      h3("Citation"),
      h5("1.Cookbook for R"),
      h5("2.Market Analysis of Fresh Berries in the United States"),
      tableOutput('t1'))
  )
)

# Define server logic ----
server <- function(input, output) {
  output$p1 <- renderPlot({
    ggplot(production,aes(x=Commodity,y=Value,fill=Commodity))+
      geom_boxplot()+
      labs(y="Price($)")
  })
  output$p2 <- renderPlot({
    ggplot(production,aes(x=Value,group=Commodity,fill=Commodity))+
      geom_density(alpha=0.36)+
      labs(x="Price($)")
  })
  output$p3 <- renderPlot({
    ggplot(production,aes(x=State,y=Value,group=State,fill=State))+
      geom_boxplot()+
      labs(y="Price($)")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  output$p4 <- renderPlot({
    ggplot(production,aes(x=Year,y=Value,group=Year,fill=Year))+
      geom_boxplot()+
      labs(y="Price($)")
  })
  output$p5 <- renderPlot({
    ggplot(procession,aes(x=Year,y=Value,group=Year,fill=Year))+
      geom_boxplot()+
      labs(y="Price($)")
  })
  output$p6 <- renderPlot({
    ggplot(processionYM,aes(x=Year,y=V1))+
      geom_line()+
      geom_point()+
      labs(y="Mean of Price ($/LB)")
  })
  output$p7 <- renderPlot({
    ggplot(gpprdC,aes(x=Year,y=V1,group=Commodity,color=Commodity))+
      geom_line()+
      geom_point()+
      labs(y="Gross Product (LB)")
  })
  output$t1 <- renderTable(rasp)
}

options(encoding = "UTF-8")
Sys.setenv(LANGUAGE = "en")

# Run the app ----
shinyApp(ui = ui, server = server)
