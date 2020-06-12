load("Concatenated_Data/climate_data/merged_climate_data.RData")




# libraries to install
# lubridate
# plotly
# ggmap
# maps
# mapdata
# svn export https://github.com/cdcepi/zika/tree/master/Ecuador/GACETA-ZIKA/data jquery-upvote-lib


rm(list=ls()) #remove previous variable assignments

# load data
load("Concatenated_Data/model_v_data.RData")

# subset data
merged_data2 <- subset(merged_data, Date > "2014-01-01")

library(plotly)

# dengue -----------------------------------------------------------------------------
plotfun_I <- function(dat){  
  plot_ly() %>%
    add_trace(data=dat, x = ~Date, y = ~I, type = 'scatter', mode = 'lines', name = 'Infected class', line = list(color = 'gray', width = 2))
}

p <- merged_data2 %>%
  group_by(Site) %>%
  do(map = plotfun_I(.)) %>%
  subplot(nrows = 4) %>%
  layout(
    showlegend = FALSE,
    width = 1000,
    height = 900,
    hovermode = FALSE
  )

p

plotfun_I <- function(dat){  
  plot_ly() %>%
    add_trace(data=dat, x = ~Date, y = ~I, type = 'scatter', mode = 'lines', name = 'Infected class', line = list(color = 'gray', width = 2))%>%
    # add_trace(data=dat, x = ~Date, y = ~denv_positive, type = 'scatter', mode='line', color=I('darkviolet'), yaxis='y2')%>%
    # add_trace(data=dat, x = ~Date, y = ~denv_positive, type = 'scatter', mode='line+markers', color=I('darkviolet'), marker = list(size = 10, color = 'pink', line = list(color = 'black', width = 2)), yaxis='y2')%>%
    layout(showlegend = FALSE,
           yaxis = list(rangemode = "tozero", showline=T, linewidth=3, title = 'Denuge (modeled)'),
           xaxis = list(rangemode = "tozero", showline=T, linewidth=2, showgrid=F))#,
  # yaxis2 = list(rangemode = "tozero", overlaying = "y", side='right', title = 'Dengue (observed)', showline=F, showgrid=F))
}

p <- merged_data2 %>%
  group_by(Site) %>%
  do(map = plotfun(.)) %>%
  subplot(nrows = 4) %>%
  layout(
    showlegend = FALSE,
    width = 1000,
    height = 900,
    hovermode = FALSE, titleY = T
  )

p




# kpa v RH
K_tr_expDecr <- function(temp, rain, Rmax, N){
  if (rain < 1){
    rain <- 1
  } 
  max(carrying_capacity_t(temp,29.0,0.05, N)*(1/rain)*6, 1000)
}


# K_tr_briere <- function(temp, rain, Rmax, N){
#   R0 <- 1
#   if((rain < R0) | (rain > Rmax)){
#     max(0.01*carrying_capacity_t(temp, 29.0, 0.05, N), 1000)
#   }
#   else {
#     c <- 7.86e-05
#     max(carrying_capacity_t(temp,29.0,0.05, N)*c*rain*(rain-R0)*sqrt(Rmax-rain)*0.1, 1000)
#   }
# }

r <- seq(0,400,1)
rain <- c()

for (i in 1:length(r)){
  rain <- c(rain, K_tr_expDecr(29, r[i], 370, 20000))
  # rain <- c(rain, K_tr_briere(29, r[i], 370, 20000))  
}

plot(r, rain, type='l', xlim=c(0,400), lwd=2, col='blue')


K_tr_poly <- function(temp, rain, Rmax, N){
  max(carrying_capacity_t(temp, 29.0, 0.05, N)*(1.130 + 2.282e-03*rain + -3.706e-05*rain^2 + 9.383e-08*rain^3 + -7.585e-11*rain^4)*5, 1000)
}

r <- seq(0,400,1)
rain <- c()

for (i in 1:length(r)){
  rain <- c(rain, K_tr_poly(29, r[i], 370, 20000))  
}


plot(r, rain, type='l', lwd=2, col='blue')

# this would use mean average rainfall in 2 weeks
K_tr_quadratic <- function(temp, rain, Rmax, N){
  R0 <- 0
  if((rain < R0) | (rain > Rmax)){
    max(0.01*carrying_capacity_t(temp, 29.0, 0.05, N), 1000)
  }
  else {
    c <- -5.99e-03
    max(carrying_capacity_t(temp, 29.0, 0.05, N)*(c*(rain-R0)*(rain-Rmax))*sqrt((c*(Rmax/2-R0)*(Rmax/2-Rmax)))*10 + 0.001, 1000)
  }
}

r <- seq(0,400,1)
rain <- c()

for (i in 1:length(r)){
  rain <- c(rain, K_tr_quadratic(29, r[i], 400, 20000))  
}


plot(r, rain, type='l')


# monotonic decreasing function? How to capture curve above from Benedum - PLUM model
# add exponential decreasing function based on Vidya
# probably need to keep everything with monthly rainfall because of gapfilling (although can try two weeks if we want)

# should similarly adapt humidity functions to match Shmidt and wesolowski 
mu_th_wes <- function(temp, hum){
  if (hum < 30){
    inverted_quadratic(temp,-1.48e-01,9.16,37.73)*1.2-(0.2*hum)
  } else {
    inverted_quadratic(temp,-1.48e-01,9.16,37.73)*(0.5)
  }
}

# keep sigmoidal (move back to 40 based on malaria (or 42)) & test Shmidt relationship ---
# need to digitize data to get slopes and need to figure out how to relate kPA to RH
# but should check SI materials first as coefficients might be provided
H <- seq(0,100,1)
muhum <- c()

for (i in 1:length(H)){
  muhum <- c(muhum, mu_th_wes(29, H[i]))  
}


plot(H, muhum, type='l')
