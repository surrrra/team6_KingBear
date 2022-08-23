library(shiny)
library(ggplot2)
library(tidyverse)

# 1. 데이터 정렬

# 데이터 불러오기
df_01 <- read.csv('pit.csv')
                
df_hit <- read.csv('hit.csv')
df_pit <- read.csv('pit.csv')

# 연도별 바뀐 팀들을 현대의 이름으로 재정렬
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

year <- unique(df_01$year)

# 연도 패배 횟수가 가장 많은 팀 데이터 형성
for (i in 1:length(year)){
  if (i == 1){
    df_temp_01 <- df_01[df_01$year == year[i],]
    df_temp_01 <- df_temp_01[order(df_temp_01$losses, decreasing = T),]
    d_temp_01 <-  df_temp_01[1,] 
  } else if (i ==2){
    df_temp_02 <- df_01[df_01$year == year[i],]
    df_temp_02 <- df_temp_02[order(df_temp_02$losses, decreasing = T),]
    d_temp_02 <-  df_temp_02[1,] 
    df_lost_1st <- rbind(d_temp_01, d_temp_02)
  } else {
    df_temp_02 <- df_01[df_01$year == year[i],]
    df_temp_02 <- df_temp_02[order(df_temp_02$losses, decreasing = T),]
    d_temp_02 <-  df_temp_02[1,] 
    df_lost_1st <- rbind(df_lost_1st, d_temp_02) 
  }
}

word <-  df_lost_1st$team
dt <- table(word)
df <- data.frame(dt)
df <- df[order(df$Freq, decreasing = F),]

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- df

label_data$id <- c(1:11)

# calculate the ANGLE of the labels
number_of_bar <- nrow(df)

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse(angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

label_data

# 2. 데이터 가공
#> Loading required package: viridisLite
# \dontshow{
options("cores"=2)
# }

df_hit_year <- df_hit[df_hit$year == 2021,]
df_pit_year <- df_pit[df_pit$year == 2021,]
df_year <- merge(df_hit_year,df_pit_year, by='team')
df_year$Invert_ERA <- 10 - df_year$ERA

# 3. 데이터 가공

for (i in (1:length(df_hit$team))){
  if(df_hit$team[i] == 'MBC Blue Dragons'){
    df_hit$team[i] = 'LG Twins'
  } else if(df_hit$team[i] == 'OB Bears'){
    df_hit$team[i] = 'Doosan Bears'
  } else if(df_hit$team[i] == 'Nexen Heroes' | df_hit$team[i] == 'Woori Heroes'){
    df_hit$team[i] = 'Kiwoom Heroes'
  } else if(df_hit$team[i] == 'SK Wyverns'){
    df_hit$team[i] = 'SSG Landers'
  } else if(df_hit$team[i] == 'Binggre Eagles'){
    df_hit$team[i] = 'Hanwha Eagles'
  } else if(df_hit$team[i] == 'Haitai Tigers'){
    df_hit$team[i] = 'Kia Tigers'
  } else if(df_hit$team[i] == 'Pacific Dolphins' | df_hit$team[i] == 'Chungbo Pintos' | df_hit$team[i] == 'Sammi Superstars'){
    df_hit$team[i] = 'Hyundai Unicorns'
  }
}

for (i in (1:length(df_pit$team))){
  if(df_pit$team[i] == 'MBC Blue Dragons'){
    df_pit$team[i] = 'LG Twins'
  } else if(df_pit$team[i] == 'OB Bears'){
    df_pit$team[i] = 'Doosan Bears'
  } else if(df_pit$team[i] == 'Nexen Heroes' | df_pit$team[i] == 'Woori Heroes'){
    df_pit$team[i] = 'Kiwoom Heroes'
  } else if(df_pit$team[i] == 'SK Wyverns'){
    df_pit$team[i] = 'SSG Landers'
  } else if(df_pit$team[i] == 'Binggre Eagles'){
    df_pit$team[i] = 'Hanwha Eagles'
  } else if(df_pit$team[i] == 'Haitai Tigers'){
    df_pit$team[i] = 'Kia Tigers'
  } else if(df_pit$team[i] == 'Pacific Dolphins' | df_pit$team[i] == 'Chungbo Pintos' | df_pit$team[i] == 'Sammi Superstars'){
    df_pit$team[i] = 'Hyundai Unicorns'
  }
}

df_hit_HW <- df_hit[df_hit$team == 'Hanwha Eagles',]
df_pit_HW <- df_pit[df_pit$team == 'Hanwha Eagles',]
df_HW <- merge(df_hit_HW,df_pit_HW, by='year')
df_HW$Invert_ERA <- 10 - df_HW$ERA

df_HW_01 <- df_HW[,c('year', 'OPS', 'Invert_ERA', 'wins')]

df_HW_01$wins <-  (df_HW_01$wins - min(df_HW_01$wins)) / (max(df_HW_01$wins) + min(df_HW_01$wins)) 
df_HW_01$Invert_ERA <-  (df_HW_01$Invert_ERA - min(df_HW_01$Invert_ERA)) / (max(df_HW_01$Invert_ERA) + min(df_HW_01$Invert_ERA)) 
df_HW_01$OPS <-  (df_HW_01$OPS - min(df_HW_01$OPS)) / (min(df_HW_01$OPS) + min(df_HW_01$OPS))

year <- df_HW_01$year
df_HW_02 = data.frame()
d = 1
for (d in 1:length(year)){
  e = 1
  for (e in 1:3){
    if (e == 1){
      df_HW_03 <- c(year[d], '01.WIN', (df_HW_01$wins [d]))
    } else if (e == 2){
      df_HW_03 <- c(year[d], '02.Inverted_ERA', (df_HW_01$Invert_ERA[d]))
    } else {
      df_HW_03 <- c(year[d], '03.OPS', (df_HW_01$OPS [d]))
    }
    df_HW_02 <- rbind(df_HW_02, df_HW_03)
  }
}
names(df_HW_02) <- c('year', 'type', 'n')
df_HW_02


library(ggplot2)
library(dplyr)
library(tidyverse)
library(tsibble)
library(rayshader)


####################################################################



ui <- pageWithSidebar(
  headerPanel(h1("KBO")),
  
  sidebarPanel(
    sliderInput("range", "year:", min = 1982, max = 2021, value = c(1982, 2021), sep='', ),
    checkboxGroupInput("variable", "Variables to show:",
                       c("Inverted_ERA" = "02.Inverted_ERA",
                         "OPS" = "03.OPS"),
                       selected=c("02.Inverted_ERA","03.OPS")),
    helpText("오른쪽에서 탭을 선택하면 다른 데이터도 볼 수 있음.")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("메인",
               img(src='https://img4.daumcdn.net/thumb/R658x0.q70/?fname=https://t1.daumcdn.net/news/202103/31/kbs/20210331163258304mbll.png'),
               h1('KBO에서 가장 못한 팀은 무엇이고 그 원인은 무엇일까?'),
               h3('"야구는 투수놀음"이라는 말은 사실일까?')
               ),
      tabPanel("시즌 꼴등을 제일 많이 한 팀",
               h3('1982년에서 2021년까지 시즌 꼴등을 제일 많이 한 팀'),
               plotOutput('myplot1')),
      tabPanel("타자와 투수",
               plotOutput('myplot2')),
      tabPanel("연도별 한화 데이터",
               plotOutput('myplot3')),
    )
  )
)


server <- function(input, output) {
  output$myplot1 <- renderPlot({
    ggplot(label_data, aes(x=id, y=Freq, fill = word)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      # This add the bars with a blue color
      geom_bar(stat="identity", alpha = 0.6) +
      
      # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
      ylim(-3,10) +
      
      # Custom the theme: no axis title and no cartesian grid
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text  = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
      ) +
      
      # This makes the coordinate polar instead of cartesian.
      coord_polar(start = 0) +
      
      # Add the labels, using the label_data dataframe that we have created before
      geom_text(data=label_data, 
                aes(x=id, y=5, label=word, hjust=hjust),
                color='black', size=4,
                angle=label_data$angle, inherit.aes=FALSE) +
      geom_text(
        x = 125, y = -3,
        label = "MOST WORST TEAM",
        size = 4,
        lineheight = 0.87,
        color = "red"
      ) 
  }, height = 900, width = 900)
  output$myplot2 <- renderPlot({
    dfPlot = ggplot(df_year) + 
      geom_point(aes(x=OPS,y=Invert_ERA,color=losses)) +
      ggtitle("타자, 투수 그리고 승률") 
    
    plot_gg(dfPlot, multicore = T, phi = 30, theta = 45)
  },)
  output$myplot3 <- renderPlot({
    df_HW_02_temp <- subset(df_HW_02, df_HW_02$year >= input$range[1] & input$range[2] >= df_HW_02$year)
    df_HW_02_temp <- subset(df_HW_02_temp, df_HW_02_temp$type == input$variable[1] | df_HW_02_temp$type == input$variable[2]  | df_HW_02_temp$type == '01.WIN')
    ggplot(df_HW_02_temp, aes(x=year, y=n, group=type, color = type)) +
    scale_color_manual(values = c('magenta', 'cyan3', 'tomato')) +   
    geom_line()+
    geom_point()+
    theme_minimal()+
    theme(legend.position='bottom') + 
    scale_y_discrete(breaks = 0.1)
  })
}
shinyApp(ui, server)


