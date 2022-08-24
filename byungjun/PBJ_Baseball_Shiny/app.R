library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(plotly)
# To install the latest version from Github:
# install.packages("devtools")
# devtools::install_github("tylermorganwall/rayshader")
library(devtools)

library(rayrender)

## 
## Attaching package: 'rayrender' # rayrende 퍼블리쉬 오류..?

## The following object is masked from 'package:rgl':
## 
##     text3d

library(rgl)
library(rayshader)
library(rgdal)
library(raster)


df <- read.csv("kbopitchingdata.csv", encoding = "UTF-8")

df_01 <- subset(df, select=-c(games_started,games_finished,intentional_walks, balks, wild_pitches))

for (i in (1:length(df_01$team))){
  if(df_01$team[i] == 'MBC Blue Dragons'){
    df_01$team[i] = 'LG Twins'
  } else if(df_01$team[i] == 'OB Bears'){
    df_01$team[i] = 'Doosan Bears'
  } else if(df_01$team[i] == 'Nexen Heroes' | df_01$team[i] == 'Woori Heroes'){
    df_01$team[i] = 'Kiwoom Heroes'
  } else if(df_01$team[i] == 'SK Wyverns'){
    df_01$team[i] = 'SSG Landers'
  } else if(df_01$team[i] == 'Binggre Eagles'){
    df_01$team[i] = 'Hanwha Eagles'
  } else if(df_01$team[i] == 'Haitai Tigers'){
    df_01$team[i] = 'Kia Tigers'
  } else if(df_01$team[i] == 'Pacific Dolphins' | df_01$team[i] == 'Chungbo Pintos' | df_01$team[i] == 'Sammi Superstars'){
    df_01$team[i] = 'Hyundai Unicorns'
  }
}




ui<-pageWithSidebar(
  headerPanel(h1('BJ')),
  
  sidebarPanel(
    sliderInput('year', '년도', min=1982, max=2021, value=1)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("first_violin",
               plotOutput('first_vio')),
      tabPanel("second_violin",
               plotlyOutput('second_vio')),
      tabPanel("cumulative",
               plotlyOutput('cum')),
      tabPanel("funnel",
               plotlyOutput('funnel')),
      tabPanel("bubble 3d",
               plotlyOutput('bubble')),
      tabPanel("rayshader",
               dataTableOutput('rayshader'))
    )
  )
)


  
  
server<-function (input, output) {
  output$first_vio<-renderPlot({
    df_01 |>
      ggplot( aes(x=team, y=average_age, fill=team, color=team)) +
      geom_violin(width=1.6, size=0.5) +
      scale_fill_viridis(discrete=TRUE) +
      scale_color_viridis(discrete=TRUE) +
      theme_ipsum() +
      theme(
        legend.position="none"
      ) +
      coord_flip() + # This switch X and Y axis and allows to get the horizontal version
      xlab("") +
      ylab("age")+
      ggtitle("     KBO 리그 선수들의 평균나이(1982~2021)")
  })
  
  output$second_vio<-renderPlotly({
    fig <- df_01 %>%
      plot_ly(
        x = ~team,
        y = ~average_age,
        split = ~team,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) 
    
    fig <- fig %>%
      layout(
        xaxis = list(
          title = "KBO TEAM"
        ),
        yaxis = list(
          title = "AGE",
          zeroline = F
        ),
        title = "KBO 리그 선수들의 평균나이(1982~2021)"
      )
    

    fig
  })
  
  output$cum<-renderPlotly({
    accumulate_by <- function(dat, var) {
      var <- lazyeval::f_eval(var, dat)
      lvls <- plotly:::getLevels(var)
      dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }
    
    fig <- df_01 %>%
      filter(year > 1981, team %in% c("Doosan Bears", "LG Twins", "Samsung Lions", "Hanwha Eagles", "Kia Tigers", "Lotte Giants"))
    fig <- fig %>% accumulate_by(~year)
    
    
    fig <- fig %>%
      plot_ly(
        x = ~year, 
        y = ~average_age,
        split = ~team,
        frame = ~frame,
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F)
      )
    fig <- fig %>% layout(
      xaxis = list(
        title = "Date",
        zeroline = F
      ),
      yaxis = list(
        title = "average_age",
        zeroline = F
      )
    ) 
    fig <- fig %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    )
    fig <- fig %>% animation_slider(
      currentvalue = list(
        prefix = "year"
      )
    )
    
    fig <- fig %>%
      layout(
        title = "KBO 리그 선수들의 나이"
      )
    
    fig
  })
  
  output$funnel <- renderPlotly({
    df_2021 <- subset(df_01, year==input$year) 
    df_2021<-df_2021 |>
      arrange(desc(wins)) # wins 내림차순으로 정렬
    
    fig <- plot_ly(
      type = "funnel",
      name = 'win_loss_percentage',
      y = df_2021$team,
      x = df_2021$win_loss_percentage*100,
      textposition = "inside",
      textinfo = "value+percent total")
    
    fig <- fig %>%
      add_trace(
        type = "funnel",
        name = 'wins',
        y = df_2021$team,
        x = df_2021$wins,
        textposition = "inside",
        textinfo = "value+percent total")
    
    fig <- fig %>%
      layout(yaxis = list(categoryarray = c(1:6)),
             title = "KBO 리그 팀들의 승률")
    
    fig
  })
  
  output$bubble<-renderPlotly({
    
    df_2021 <- subset(df_01, year==input$year)
    df_2021 <- df_2021[order(df_2021$team), ]
    fig <- plot_ly(df_2021, x = ~runs, y = ~hits, z = ~walks, color = ~team, size = ~wins,
                   marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(25, 50),
                   text = ~paste('runs : ', runs, '<br>hits : ', hits, '<br> walks :', walks, '<br> wins : ', wins))
    fig <- fig %>% layout(title = '선수들의 실책',
                          
                          scene = list(xaxis = list(title = 'runs',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    type = 'log',
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    gridwidth = 2),
                                       
                                       yaxis = list(title = 'hits',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    gridwith = 2),
                                       
                                       zaxis = list(title = 'walks',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    type = 'log',
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    gridwith = 2)),
                          
                          
                          plot_bgcolor = 'rgb(243, 243, 243)')
    
    fig
  })
  
  output$rayshader <- renderPlotly({
    montereybay %>%
      sphere_shade(zscale = 10, texture = "imhof1") %>%
      plot_3d(montereybay, zscale = 50, fov = 70, theta = 270, phi = 30,
              windowsize = c(1000, 800), zoom = 0.6,
              water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "#233aa1",
              waterlinecolor = "white", waterlinealpha = 0.5)
    Sys.sleep(0.2)
    render_highquality(lightdirection = c(-45,45), lightaltitude  = 30, clamp_value = 10,
                       samples = 256, camera_lookat= c(0,-50,0),
                       ground_material = diffuse(color="grey50",checkercolor = "grey20", checkerperiod = 100),
                       clear = TRUE)

    })
}




shinyApp(ui, server)

