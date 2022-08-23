library(shiny)
library(esquisse)
library(plotly)
library(ggplot2)
library(gapminder)
library(dplyr)
library(wordcloud2)
library(fmsb)
library("plot3D")
library(rgl)
library(viridis)
library(mapproj)
library(rgl)

# 데이터 불러오기
df <- read.csv('kbodata.csv')

# 결측치가 있어 필요 없는 컬럼 제거
df1 <- subset(df, select=-c(games_started,games_finished,intentional_walks,balks,wild_pitches))

# 연도별 바뀐 팀들을 현대의 이름으로 재정렬
for (i in (1:length(df1$team))){
    if(df1$team[i] == 'MBC Blue Dragons'){
        df1$team[i] = 'LG Twins'
    } else if(df1$team[i] == 'OB Bears'){
        df1$team[i] = 'Doosan Bears'
    } else if(df1$team[i] == 'Nexen Heroes' | df1$team[i] == 'Woori Heroes'){
        df1$team[i] = 'Kiwoom Heroes'
    } else if(df1$team[i] == 'SK Wyverns'){
        df1$team[i] = 'SSG Landers'
    } else if(df1$team[i] == 'Binggre Eagles'){
        df1$team[i] = 'Hanwha Eagles'
    } else if(df1$team[i] == 'Haitai Tigers'){
        df1$team[i] = 'Kia Tigers'
    } else if(df1$team[i] == 'Pacific Dolphins' | df1$team[i] == 'Chungbo Pintos' | df1$team[i] == 'Sammi Superstars'){
        df1$team[i] = 'Hyundai Unicorns'
    }
}

# 스케일링
min.max.scale<-function (x) {
    return ((x-min(x))/(max(x)-min(x)))
}

df2<-df1
df2$wins <- min.max.scale(df$wins)
df2$runs_per_game <- min.max.scale(df$runs_per_game)
df2$ERA <- min.max.scale(df$ERA)
df2$run_average_9 <- min.max.scale(df$run_average_9)
df2$strikeouts <- min.max.scale(df$strikeouts)
df2$hits_9 <- min.max.scale(df$hits_9)

test <- subset(df, year >= 0)
test <- test[, c(3,6)]
test$wins <- as.integer(test$wins)
test <- aggregate(wins ~ team, test, sum)
test


# ## 팀별 시즌 당 총 우승
# ggplot(df1) +
#     aes(x = team, y = wins, las=7) +
#     geom_col(fill = "#112446") +
#     theme_minimal()
# 
# # 팀과 우승별 표시
# ggplot(df1) + geom_point(aes(x = team, y = wins, size = wins))
# 
# # 워드 클라우드
# 
# test <- subset(df, year >= 0)
# test <- test[, c(3,6)]
# test$wins <- as.integer(test$wins)
# test <- aggregate(wins ~ team, test, sum)
# test
# 
# wordcloud2(data = test
#            , color = "random-dark"
#            , shape = "cloud"
#            , size = 0.3
#            , fontFamily = "나눔고딕")
# 
# # 워드클라우드 배트
# wordcloud2(test, size = 0.7, shape = 'circle')
# wordcloud2(test, figPath = "test.png", size = 0.1, color = "skyblue", backgroundColor="black")

# 시도 2

p <- ggplot(df1) +
    aes(x = year, y = wins, colour = team) +
    geom_point(shape = "circle", aes(size = wins)) +
    scale_color_hue(direction = 1) +
    theme_minimal()

fig <- ggplotly(p)

fig <- fig %>% 
    animation_opts(
        1000, easing = "elastic", redraw = FALSE
    )

# fig <- fig %>% 
#     animation_button(
#         x = 1, xanchor = "right", y = 0, yanchor = "bottom"
#     ) %>%
#     animation_slider(
#         currentvalue = list(prefix = "YEAR ", font = list(color="red"))
#     )
# --------------------------------------



# radarchart(df2, axistype=1,
#            
#            #custom polygon
#            pcol=rgb(0.2,0.2,0.2,0.2) , pfcol=rgb(0.2,0.2,0.2,0.2) , plwd=6 ,
#            
#            #custom the grid
#            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,30,5), cglwd=0.8,
#            
#            #custom labels
#            vlcex=0.8
# )

###### 3D 그래프 완성 #####
# library


# This is to output a rgl plot in a rmarkdown document.
# setupKnitr()

# Data: the iris data is provided by R
#data <- iris

# Add a new column with color
mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
#df2$color <- mycolors[as.numeric(df2$wins)]

# Plot
# plot3d(
#     x=df2$runs_per_game, y=df2$hits_9, z=df2$wins, 
#     col = mycolors,           #df2$color
#     type = 's', 
#     radius = .1,
#     xlab="runs per game", ylab="hits9", zlab="wins")





# Define UI for application that draws a histogram
ui <- pageWithSidebar(

    # Application title
    headerPanel(h1("KBO")),

    # Sidebar with a slider input for number of bins 
    sidebarPanel(
        sliderInput("years", "년도",
                    min = 1982,
                    max = 2021,
                    value = 1),
        selectInput("team", "팀", selected = TRUE, choices = unique(df2$team))
        ),

        # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
        tabPanel('test1',
                 plotOutput("win.team")
                 ),
        
        tabPanel('test2',
                 plotOutput("win.team2")
                 ),
        
        tabPanel('test3',
                 wordcloud2Output("wordcloud")
                 ),
        
        tabPanel('test4',
                 imageOutput("wordcloud2"),
                 align = 'center'
                 ),
        
        tabPanel('test5',
                 plotlyOutput("scatterplot")
                 ),
        tabPanel('test6',
                 plotOutput("spiderplot", height = '800px')
                 ),
        tabPanel('test7',
                 imageOutput("D3image")
                 ),
        tabPanel('test8',
                 plotOutput("D3"))
    )



        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$win.team <- renderPlot({
        ggplot(df1) +
            aes(x = team, y = wins, las=7) +
            geom_col(fill = "#112446") +
            theme_minimal()
    })
    output$win.team2 <- renderPlot({
        ggplot(df1) + geom_point(aes(x = team, y = wins, size = wins))
    })
    output$wordcloud <- renderWordcloud2({
        wordcloud2(data = test
                   , color = "random-dark"
                   , shape = "cloud"
                   , size = 0.3
                   , fontFamily = "나눔고딕")
    })
    output$wordcloud2 <- renderImage(deleteFile = F,{
        list(src = "test.png",
             width = 600,
             height = 600)
    })
    output$scatterplot <- renderPlotly({
        fig
    })
    output$spiderplot <- renderPlot({
        df2 <- df2[(df2$year == input$years) & (df2$team == input$team), c(5, 6, 9, 10, 21, 25)]
        df2 <- rbind(rep(1,6), rep(0,6), df2)
        
        radarchart(df2, axistype=1,
                   
                   #custom polygon
                   pcol=rgb(0.2,0.2,0.2,0.2) , pfcol=rgb(0.2,0.2,0.2,0.2) , plwd=6 ,
                   
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,30,5), cglwd=0.8,
                   
                   #custom labels
                   vlcex=0.8
        )
    })
    output$D3image <- renderImage(deleteFile = F,{
        list(src = ".\\3D.png",
             width = 600,
             height = 600)
    })   
    output$D3 <- renderPlot({
        plot3d(
            x=df2$runs_per_game, y=df2$hits_9, z=df2$wins, 
            col = mycolors,           #df2$color
            type = 's', 
            radius = .1,
            xlab="runs per game", ylab="hits9", zlab="wins")
    })
    
}

# Run the application 
shinyApp(ui, server)
