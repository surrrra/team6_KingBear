library(shiny)
library(ggplot2)

# 데이터 불러오기
df <- read.csv("./kbopitchingdata.csv", header=T)

### 데이터 시각화 -------------------------------------
ui <- pageWithSidebar(
  
  headerPanel(h1("곽성화")),
  
  sidebarPanel(
    selectInput("year","몇 년도가 궁금합니까?",
                seq(1982,2021,1)),    
  ), 
  
  mainPanel(
    h3("해당연도의 팀별 승률"),
    h3(textOutput("caption")),
    plotOutput("Plot")
  )
)


server <- function(input, output) {

  output$Plot <- renderPlot({
    ggplot(subset(df, year==input$year), 
           aes(x=as.factor(team), y=win_loss_percentage, fill=team, width=0.5))+
      geom_bar(stat='identity')+
      ggtitle(label='')+xlab("프로야구팀")+ylab("승률")+
      geom_text(aes(label=win_loss_percentage), vjust=0, hjust=1, size=5)+
      theme(legend.position = "none", 
            axis.text = element_text(size=15))+
      coord_flip()   # 가로막대
      # coord_polar()    # 원그래프
    
  })
}

# 웹서버 구동
shinyApp(ui, server)
