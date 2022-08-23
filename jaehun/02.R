library(rayshader)
library(ggplot2)
#> Loading required package: viridisLite
# \dontshow{
options("cores"=2)
# }

df_hit <- read.csv('hit.csv')
df_pit <- read.csv('pit.csv')

df_hit_year <- df_hit[df_hit$year == 2021,]
df_pit_year <- df_pit[df_pit$year == 2021,]
df_year <- merge(df_hit_year,df_pit_year, by='team')
df_year$Invert_ERA <- 10 - df_year$ERA

dfPlot = ggplot(df_year) + 
  geom_point(aes(x=OPS,y=Invert_ERA,color=losses)) +
  ggtitle("타자, 투수 그리고 승률") 

plot_gg(dfPlot, multicore = T, phi = 30, theta = 45)

