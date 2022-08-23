library(shiny)
library(ggplot2)
library(plotly)
library(fmsb)
library(dplyr)
library(hrbrthemes)
library(gganimate)

df<-read.csv('kbopitchingdata.csv')

# 결측치가 있어서 필요 없는 컬럼 제거
df <- subset(df, select=-c(games_started,games_finished,intentional_walks,balks,wild_pitches))



# 연도별 바뀐 팀들을 현대의 이름으로 재정렬
for (i in (1:length(df$team))){
  if(df$team[i] == 'MBC Blue Dragons'){
    df$team[i] = 'LG Twins'
  } else if(df$team[i] == 'OB Bears'){
    df$team[i] = 'Doosan Bears'
  } else if(df$team[i] == 'Nexen Heroes' | df$team[i] == 'Woori Heroes'){
    df$team[i] = 'Kiwoom Heroes'
  } else if(df$team[i] == 'SK Wyverns'){
    df$team[i] = 'SSG Landers'
  } else if(df$team[i] == 'Binggre Eagles'){
    df$team[i] = 'Hanwha Eagles'
  } else if(df$team[i] == 'Haitai Tigers'){
    df$team[i] = 'Kia Tigers'
  } else if(df$team[i] == 'Pacific Dolphins' | df$team[i] == 'Chungbo Pintos' | df$team[i] == 'Sammi Superstars'){
    df$team[i] = 'Hyundai Unicorns'
  }
}


min.max.scale<-function (x) {
  return ((x-min(x))/(max(x)-min(x)))
}




ui<-pageWithSidebar(
  headerPanel(h1('SuA')),

  sidebarPanel(
    selectInput('years', '년도?',
                selected=TRUE,
                choices=df$year),
    
    selectInput('teams1', '팀1', selected=TRUE, choices=unique(df$team)),
    
    selectInput('teams2', '팀2', selected=TRUE, choices=unique(df$team))
  ),

  mainPanel(
    tabsetPanel(
      tabPanel('Stat', plotOutput('teamplot')),
      tabPanel('Wins', plotlyOutput('lollipop')),
      tabPanel('ERA', plotlyOutput('ERA'))
      )
    )
)


colnames(df)
server<-function (input, output) {
  output$teamplot<-renderPlot({
    df$hits_9<-min.max.scale(df$hits_9)
    df$homeruns_9<-min.max.scale(df$homeruns_9)
    df$walks_9<-min.max.scale(df$walks_9)
    df$strikeouts_9<-min.max.scale(df$strikeouts_9)
    df$earned_runs<-min.max.scale(df$earned_runs)
    
    df<-subset(df, year==input$years)
    df<-df[(df$team==input$teams1) | (df$team==input$teams2), c(18, 25:28)]
    df<-rbind(rep(1, 5), rep(0, 5), df)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
    
    radarchart(df, axistype=1,
               pcol=colors_border,
               pfcol=colors_in,
               plwd=4,
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
               vlcex=1.1)
    
    legend(x=0.7, y=1, legend = c(input$teams1, input$teams2), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
    
    title('team stat', cex.main=2)
  })
  
  output$lollipop<-renderPlotly({
    df<-subset(df, year==input$years)
    
    p<-ggplot(df, aes(x=team, y=wins)) +
      geom_segment( aes(x=team, xend=team, y=0, yend=wins ), color=ifelse(df$team %in% c(input$teams1, input$teams2), "orange", "grey"), size=ifelse(df$team %in% c(input$teams1, input$teams2), 1.3, 1) ) +
      geom_point( color=ifelse(df$team %in% c(input$teams1, input$teams2), "orange", "grey"), size=ifelse(df$team %in% c(input$teams1, input$teams2), 5, 3) ) +
      theme_ipsum() +
      coord_flip() +
      theme(
        legend.position="none",
        panel.grid.major.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      xlab("") +
      ylab("") +
      ggtitle("wins")
    
    ggplotly(p)
  })
  
  output$ERA <- renderPlotly({
    df1<-df[df$team==input$teams1, ]
    df2<-df[df$team==input$teams2, ]
    newdf<-merge(df1,df2,by.x='year',by.y='year')
    newdf$dif.ERA<-newdf$ERA.x-newdf$ERA.y
    p<-ggplot(newdf, aes(x=year, y=dif.ERA)) +
      geom_vline(xintercept=input$years, color='red', linetype=2)+
      theme_minimal()+
      geom_area(fill='tomato', alpha=0.4)+
      theme(
        legend.position="none",
        panel.grid.major.x = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
      )+
      ggtitle("ERA")

    
    
    ggplotly(p)
  })
  
  
}



shinyApp(ui, server)


