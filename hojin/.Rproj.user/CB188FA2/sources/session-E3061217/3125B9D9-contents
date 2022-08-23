# 데이터 불러오기
df <- read.csv('kbodata.csv')

library(esquisse)
library(plotly)
library(ggplot2)
library(gapminder)
library(dplyr)
library(wordcloud2)
library(shiny)

# 결측치가 있어 필요 없는 컬럼 제거
df1 <- subset(df, select=-c(games_started,games_finished,intentional_walks,balks,wild_pitches))

str(df1)

df1$team

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

## 팀별 시즌 당 총 우승
ggplot(df1) +
    aes(x = team, y = wins, las=7) +
    geom_col(fill = "#112446") +
    theme_minimal()
# ----------------------------------------------------------------
# 워드 클라우드

test <- subset(df, year >= 0)
test <- test[, c(3,6)]
test$wins <- as.integer(test$wins)
test <- aggregate(wins ~ team, test, sum)
test

wordcloud2(data = test
           , color = "random-dark"
           , shape = "cloud"
           , size = 0.3
           , fontFamily = "나눔고딕")

# ----------------------------------------------------------------

# 워드클라우드 배트
wordcloud2(test, size = 0.7, shape = 'circle')
wordcloud2(test, figPath = "test.png", size = 0.1, color = "skyblue", backgroundColor="black")

# -------------------------------------------------------------

# 팀과 우승별 표시
ggplot(df1) + geom_point(aes(x = team, y = wins, size = wins))


# ----------------------------------------------------------------
## 추후 결정
ggplot(df1) +
    aes(x = year, y = wins, colour = team, size = wins) +
    geom_line() +
    scale_color_hue(direction = 1) +
    theme_minimal()
# ---------------------------
### 산점도
ggplot(df1) +
    aes(x = year, y = wins, colour = team) +
    geom_point(shape = "circle", size = 4L) +
    scale_color_hue(direction = 1) +
    theme_minimal()
#----------------------------
# 시도 1

p <- ggplot(df1) +
    aes(x = year, y = wins, colour = team) +
    geom_point(shape = "circle", size = 4L) +
    scale_color_hue(direction = 1) +
    theme_minimal()

fig <- ggplotly(p)

fig <- fig %>% 
    animation_opts(
        1000, easing = "elastic", redraw = FALSE
    )

fig <- fig %>% 
    animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )

fig

# -----------------------------------------------------------------
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

fig <- fig %>% 
    animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    ) %>%
    animation_slider(
        currentvalue = list(prefix = "YEAR ", font = list(color="red"))
    )

fig
# ----------------------------------------------------------------

library(esquisse)
library(plotly)
library(ggplot2)
library(gapminder)

p <- ggplot(gapminder, aes(team, wins, color = continent)) +
    geom_point(aes(size = pop, frame = year, ids = team)) +
    scale_x_log10()

fig <- ggplotly(p) %>% 
    animation_opts(
        1000, easing = "elastic", redraw = FALSE
    ) %>% 
    animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    ) %>%
    animation_slider(
        currentvalue = list(prefix = "YEAR ", font = list(color="red"))
    )

fig


# -------------------------------------------------------

# df1 %>% ggplot(aes(x=2,perc, fill=mfr)) + 
#     geom_bar(stat="identity",color="white", alpha =.5) + 
#     coord_polar(theta = "y", start=0)+
#     geom_text(aes(y = y_pos, label = paste0(perc,"%")), color = "black")+
#     scale_fill_manual(values = rainbow(7)) +
#     theme_void() +
#     xlim(0.5, 2.5)


# ----------------------------------------------------------------------------
library(fmsb)
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

summary(df2)
str(df2)
#c("wins" , "ERA" , "hits_9" , "runs_per_game" , "R-run_average_9")
df2 <- df2[(df2$year == 2021) & (df2$team == 'Samsung Lions'), c(5, 6, 9, 10, 21, 25)]
df2
df2 <- rbind(rep(1,6), rep(0,6), df2)
df2
# df2 <- df2[c(1), ]
# test_1 <- t(df2)
radarchart(df2, axistype=1,

            #custom polygon
            pcol=rgb(0.2,0.2,0.2,0.2) , pfcol=rgb(0.2,0.2,0.2,0.2) , plwd=6 ,

            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,30,5), cglwd=0.8,

            #custom labels
            vlcex=0.8
           )

# -------------------------------------------------------------------------------


### 시도

# df2 <- df2[(df2$year == 2021) & (df2$team == 'Samsung Lions'), c(5, 6, 9, 10, 21, 25)]
# df2
# df2 <- rbind(rep(1,6), rep(0,6), df2)
# 
# 
# x = df2[df2$year == 2021]
# x
# y = df2[df2$wins]
# y
# z = rbind(rep(1,0), rep(0,1), df2)
# z
# 
# library(plotly)
# fig <- plot_ly(
#     type = 'surface',
#     contours = list(
#         x = list(show = TRUE, start = 1.5, end = 2, size = 0.04, color = 'white'),
#         z = list(show = TRUE, start = 0.5, end = 0.8, size = 0.05)),
#     x = ~x,
#     y = ~y,
#     z = ~z)
# fig <- fig %>% layout(
#     scene = list(
#         xaxis = list(nticks = 20),
#         zaxis = list(nticks = 4),
#         camera = list(eye = list(x = 0, y = -1, z = 0.5)),
#         aspectratio = list(x = .9, y = .8, z = 0.2)))
# 
# fig

# ---------------------------------------------------------------------------------

#data(df1)
library("plot3D") #colvar = UrbanPop
with(df2, text3D(hits_9, runs_per_game, wins, 
                       labels = rownames(df2), colvar = df2$wins,
                       col = gg.col(100), theta = 60, phi = 20,
                       xlab = "hits_9", ylab = "runs_per_game", zlab = "wins", 
                       main = "baseball", cex = 0.6, 
                       bty = "g", ticktype = "detailed", d = 2,
                       clab = c("Urban","Pop"), adj = 0.5, font = 2))
                       with(df2, scatter3D(hits_9, runs_per_game, wins - 1,      # Add points 
                       colvar = wins, col = gg.col(100), 
                       type = "h", pch = ".", add = TRUE))



# -----------------------------------------------------------------------------

###### 3D 그래프 완성 #####
# library
library(rgl)

# This is to output a rgl plot in a rmarkdown document.
# setupKnitr()
                       
# Data: the iris data is provided by R
#data <- iris
                       
# Add a new column with color
mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
#df2$color <- mycolors[as.numeric(df2$wins)]
                       
# Plot
plot3d(
    x=df2$runs_per_game, y=df2$hits_9, z=df2$wins, 
    col = mycolors,           #df2$color
    type = 's', 
    radius = .1,
    xlab="runs per game", ylab="hits9", zlab="wins")
                       
    # To display in an R Markdown document:
    # rglwidget()
                       
    # To save to a file:
    #htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
                            #file = "HtmlWidget/3dscatter.html",
                            #libdir = "libs",
                            #selfcontained = FALSE
                            #)


# --------------------------------------------------------------------------
str(df2)
#part_2007$rno <- c(1:10)
#4510e3','#e310c3','#f03630'

lb <- df2
lb
angle <-  90-(360*(lb$hits_9)/nrow(df2))
lb$wins<-ifelse(angle < -90, 1, 0)
lb$runs_per_game<-ifelse(angle < -90, angle+180, angle)
color_1 <- c(
    '#c93318','#e36810','#e3ce10','#84e310','#10e36b',
    '#10e3d8','#1084e3'                                       
)
color_1 <- rev(color_1)
ggplot(lb, aes(x=as.factor(hits_9), y=(runs_per_game)*200))+
    geom_bar(stat='identity', fill=color_1)+
    theme_void()+
    coord_polar(start=0)+
    geom_text(data=lb, 
              aes(x=hits_9, y=(runs_per_game)*200, label='안타'),   #hjust=hjust
              color='black', size=4,
              angle=lb$angle, inherit.aes=FALSE,
              family="HUIncludemyungjo140",
    )


# ------------------------------------------------------------------------------

library(viridis)
library(mapproj)
ggplot() +
    geom_polygon(data = df1, aes(fill = wins, x = team, y = year, group = team) , size=0, alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="wins (rank)", breaks=c(1,50,100, 140), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) +
    labs( title = "KBO" ) +
    ylim(1982,2021) +
    theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f4", color = NA), 
        panel.background = element_rect(fill = "#f5f5f4", color = NA), 
        legend.background = element_rect(fill = "#f5f5f4", color = NA),
        plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.position = c(0.2, 0.26)
    ) +
    coord_map()



