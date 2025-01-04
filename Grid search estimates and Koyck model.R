library(readxl)
library(rgl)
data <- read_xlsx('Trail4_Grid search.xlsx')
data$xt_alpha <- 0
obj <- c()
par <- c()
data$`y(t)`
#One dimensional grid search on TV
U <- length(data$GRPS)-1
for (alpha in seq(0.0,1, by = 0.05)){
  if( alpha < 1) data$xt_alpha[1] = data$GRPS[1]/(1-alpha)
  else data$xt_alpha[1] = data$GRPS[1]
  
  for (i in 1:U){
  data$xt_alpha[i+1] <- data$GRPS[i+1] + (alpha)*data$xt_alpha[i]
  }
  par = c(par,alpha)
  model <- lm(`y(t)`~xt_alpha, data = data)
  obj <- c(obj,AIC(model))
}
obj
par
plot(par, obj, type = "l",  # "l" stands for a line plot
     col = "blue", lwd = 2,  # Line color and thickness
     xlab = expression(alpha[xt]),  # Greek alpha on X-axis
     ylab = expression(AIC),  
     main = "Objective function")
Combined <- data.frame(alpha_xt = par, AIC = obj)
a = Combined$alpha_xt[which(Combined$AIC == min(Combined$AIC))]
a
data$xt_alpha[1] = data$GRPS[1]
for (i in 1:U){
  data$xt_alpha[i+1] <- data$GRPS[i+1] + a*data$xt_alpha[i]
}
M <- lm(`y(t)`~xt_alpha, data = data)
summary(M)
a##Proper initialization of the series affects how its estimated

data <- read_xlsx('Trail2_Grid search.xlsx')
data$xt_alpha <- 0
obj <- c()
par <- c()
data$`y(t)`
#One dimensional grid search on TV
U <- length(data$GRPS)-1
for (alpha in seq(0.0,1, by = 0.05)){
  data$xt_alpha[1] = data$GRPS[1]
  for (i in 1:U){
    data$xt_alpha[i+1] <- data$GRPS[i+1] + (alpha)*data$xt_alpha[i]
  }
  par = c(par,alpha)
  model <- lm(`y(t)`~xt_alpha, data = data)
  obj <- c(obj,AIC(model))
}
obj
par
plot(par, obj, type = "l",  # "l" stands for a line plot
     col = "blue", lwd = 2,  # Line color and thickness
     xlab = expression(alpha[xt]),  # Greek alpha on X-axis
     ylab = expression(AIC),  
     main = "Objective function")
min(obj)

data$xt_alpha[1] = data$GRPS[1]
for (i in 1:U){
  data$xt_alpha[i+1] <- data$GRPS[i+1] + (alpha)*data$xt_alpha[i]
}

Combined <- data.frame(alpha_xt = par, AIC = obj)
a = Combined$alpha_xt[which(Combined$AIC == min(Combined$AIC))]

data$xt_alpha[1] = data$GRPS[1]
for (i in 1:U){
  data$xt_alpha[i+1] <- data$GRPS[i+1] + a*data$xt_alpha[i]
}
M <- lm(`y(t)`~xt_alpha, data = data)
summary(M)
###Try different noise

data <- read_xlsx('Trail3_Grid search.xlsx')
data$xt_alpha <- 0
obj <- c()
par <- c()
data$`y(t)`
#One dimensional grid search on TV
U <- length(data$GRPS)-1
for (alpha in seq(0.0,1, by = 0.05)){
  data$xt_alpha[1] = data$GRPS[1]
  for (i in 1:U){
    data$xt_alpha[i+1] <- data$GRPS[i+1] + (alpha)*data$xt_alpha[i]
  }
  par = c(par,alpha)
  model <- lm(`y(t)`~xt_alpha, data = data)
  obj <- c(obj,AIC(model))
}
obj
par
plot(par, obj, type = "l",  # "l" stands for a line plot
     col = "blue", lwd = 2,  # Line color and thickness
     xlab = expression(alpha[xt]),  # Greek alpha on X-axis
     ylab = expression(AIC),  
     main = "Objective function")
min(obj)

Combined <- data.frame(alpha_xt = par, AIC = obj)
a = Combined$alpha_xt[which(Combined$AIC == min(Combined$AIC))]
a
data$xt_alpha[1] = data$GRPS[1]
for (i in 1:U){
  data$xt_alpha[i+1] <- data$GRPS[i+1] + a*data$xt_alpha[i]
}
M <- lm(`y(t)`~xt_alpha, data = data)
summary(M)







for (alpha in seq(0,1, by = 0.05)){
  data$yt_alpha[1] = data$Market[1]
  for (i in 1:Tv){
    data$yt_alpha[i+1] <- alpha*data$Market[i+1] + (1-alpha)*data$yt_alpha[i]
  }
  
  par = c(par,alpha)
  model <- lm(log(Output)~log(yt_alpha), data = data)
  obj <- c(obj,AIC(model))
}
data$yt_alpha[1] = data$Market[1]
for (i in 1:Tv){
  data$yt_alpha[i+1] <- 0.3*data$Market[i+1] + 0.7*data$yt_alpha[i]
}

par = c(par,alpha)
model <- lm(log(Output)~log(yt_alpha), data = data)
obj <- c(obj,AIC(model))
plot(data$time,data$Market , type = "l",  
     col = "blue", lwd = 2,  # Line color and thickness
     xlab = 'Time',  # Greek alpha on X-axis
     ylab = 'Market search',  
     main = "Original and smoothed series")
lines(data$time, data$yt_alpha, col = "red", lwd = 2)
model <- lm(log(Output)~log(yt_alpha), data = data)
AIC(model)
obj
par
min(obj)
data$yt_alpha[1] = data$TV[1]
for (i in 1:Tv){
  data$yt_alpha[i+1] <- 0.3*data$TV[i+1] + 0.7*data$yt_alpha[i]
}
plot(data$yt_alpha, data$Output, type = "p",  
     col = "blue", lwd = 2,  # Line color and thickness
     xlab = expression(TV_t),  # Greek alpha on X-axis
     ylab = expression(S_t),  
     main = "Sales and TV")
plot(log(data$yt_alpha), log(data$Output), type = "p",  
     col = "blue", lwd = 2,  # Line color and thickness
     xlab = expression(log(TV_t)),  # Greek alpha on X-axis
     ylab = expression(log(S_t)),  
     main = "Log transformed")

plot(par, obj, type = "l",  # "l" stands for a line plot
     col = "blue", lwd = 2,  # Line color and thickness
     xlab = expression(alpha[TV]),  # Greek alpha on X-axis
     ylab = expression(AIC),  
     main = "Objective function")
#Repeating the same for the brand clicks
for (alpha in seq(0.05,0.95, by = 0.05)){
  data$yt_alpha[1] = data$Brand[1]
  for (i in 1:Tv){
    data$yt_alpha[i+1] <- alpha*data$Brand[i+1] + (1-alpha)*data$yt_alpha[i]
  }
  par = c(par,alpha)
  model <- lm(log(Output)~log(yt_alpha), data = data)
  obj <- c(obj,AIC(model))
}

#Two dimensional grid search:
data$yt_Brand <-0
data$yt_tv <- 0
par_x <- c()
par_y <- c()
obj2 <- c()
for (alphax in seq(0.05,0.95, by = 0.05)){
  data$yt_tv[1] = data$TV[1]
  
  for (i in 1:Tv){
    data$yt_tv[i+1] <- alphax*data$TV[i+1] + (1-alphax)*data$yt_tv[i]
  }
  for (alphay in seq(0.05,0.95, by = 0.05)){
    data$yt_Brand[1] = data$Brand[1]
    for (i in 1:Tv){
      data$yt_Brand[i+1] <- alphay*data$Brand[i+1] + (1-alphay)*data$yt_Brand[i]
    }
    par_x = c(par_x,alphax)
    par_y = c(par_y,alphay)
    model <- lm(log(Output)~log(yt_tv) + log(yt_Brand), data = data)
    obj2 <- c(obj2,AIC(model))
  }
}
Combined <- data.frame(alpha_tv = par_x, alpha_Brand = par_y, AIC = obj2)
Combined$alpha_tv[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Brand[which(Combined$AIC == min(Combined$AIC))]
x= seq(0.05,0.95, by = 0.05)
y = seq(0.05,0.95, by = 0.05)
# Create a grid for X and Y using `expand.grid`
grid <- expand.grid(X = x, Y = y)

# Compute Z as a function of X and Y (e.g., Z = sin(sqrt(X^2 + Y^2)))
grid$Z <- with(grid, Combined$AIC)

# Reshape data into a matrix format for surface plotting
z_matrix <- matrix(grid$Z, nrow = length(x), ncol = length(y))


# Open a 3D plotting window and draw the surface
x= seq(0.05,0.95, by = 0.05)
y = seq(0.05,0.95, by = 0.05)
persp3d(x, y, z_matrix, 
        col = "lightblue", 
        xlab = expression(alpha[TV]), ylab = expression(alpha[Brand_clicks]), zlab = "AIC",
        main = "", alpha = 0.9)

##Three dimensional grid search:
data$yt_Brand <-0
data$yt_tv <- 0
data$yt_Market <- 0
par_x <- c()
par_y <- c()
par_z <- c()
obj2 <- c()
for (alphax in seq(0.05,0.95, by = 0.05)){
  data$yt_tv[1] = data$TV[1]
  
  for (i in 1:Tv){
    data$yt_tv[i+1] <- alphax*data$TV[i+1] + (1-alphax)*data$yt_tv[i]
  }
  for (alphay in seq(0.05,0.95, by = 0.05)){
    data$yt_Brand[1] = data$Brand[1]
    for (i in 1:Tv){
      data$yt_Brand[i+1] <- alphay*data$Brand[i+1] + (1-alphay)*data$yt_Brand[i]
    }
    for (alphaz in seq(0.05,0.95, by = 0.05)){
      data$yt_Market[1] = data$Market[1]
      for (i in 1:Tv){
        data$yt_Market[i+1] <- alphaz*data$Market[i+1] + (1-alphaz)*data$yt_Market[i]
      }
    
    par_x = c(par_x,alphax)
    par_y = c(par_y,alphay)
    par_z = c(par_z,alphaz)
    model <- lm(log(Output)~log(yt_tv) + log(yt_Brand) + log(yt_Market), data = data)
    obj2 <- c(obj2,AIC(model))
    }
  }
}
Combined <- data.frame(alpha_tv = par_x, alpha_Brand = par_y,
                       alpha_Market = par_z, AIC = obj2)
Combined$alpha_tv[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Brand[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Market[which(Combined$AIC == min(Combined$AIC))]

##Four dimensional grid search:(130321 points)
data$yt_Brand <-0
data$yt_tv <- 0
data$yt_Market <- 0
data$yt_DAw <- 0
par_x <- c()
par_y <- c()
par_z <- c()
par_w <- c()
obj3 <- c()
for (alphax in seq(0.05,0.95, by = 0.05)){
  data$yt_tv[1] = data$TV[1]
  
  for (i in 1:Tv){
    data$yt_tv[i+1] <- alphax*data$TV[i+1] + (1-alphax)*data$yt_tv[i]
  }
  for (alphay in seq(0.05,0.95, by = 0.05)){
    data$yt_Brand[1] = data$Brand[1]
    for (i in 1:Tv){
      data$yt_Brand[i+1] <- alphay*data$Brand[i+1] + (1-alphay)*data$yt_Brand[i]
    }
    for (alphaz in seq(0.05,0.95, by = 0.05)){
      data$yt_Market[1] = data$Market[1]
      for (i in 1:Tv){
        data$yt_Market[i+1] <- alphaz*data$Market[i+1] + (1-alphaz)*data$yt_Market[i]
      }
      for (alphaw in seq(0.05,0.95, by = 0.05)){
        data$yt_DAw[1] = 100
        for (i in 1:Tv){
          data$yt_DAw[i+1] <- alphaw*data$Dis.Awarn[i+1] + (1-alphaw)*data$yt_DAw[i]
        }
        par_x = c(par_x,alphax)
        par_y = c(par_y,alphay)
        par_z = c(par_z,alphaz)
        par_w = c(par_w,alphaw)
        model <- lm(log(Output)~log(yt_tv) + log(yt_Brand) + log(yt_Market)+
                      log(yt_DAw), data = data)
        obj3 <- c(obj3,AIC(model))
      }
    }
  }
}
Combined <- data.frame(alpha_tv = par_x, alpha_Brand = par_y,
                       alpha_Market = par_z,alpaha_DAC = par_w, AIC = obj3)
Combined$alpha_tv[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Brand[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Market[which(Combined$AIC == min(Combined$AIC))]
Combined$alpaha_DAC[which(Combined$AIC == min(Combined$AIC))]

##five dimensional grid search:( 59049 points)
data$yt_Brand <-0
data$yt_tv <- 0
data$yt_Market <- 0
data$yt_DAw <- 0
data$yt_DR <- 0
par_1 <- c()
par_2 <- c()
par_3 <- c()
par_4 <- c()
par_5 <- c()
obj5 <- c()
for (alpha1 in seq(0.1,0.95, by = 0.1)){
  data$yt_tv[1] = data$TV[1]
  
  for (i in 1:Tv){
    data$yt_tv[i+1] <- alpha1*data$TV[i+1] + (1-alpha1)*data$yt_tv[i]
  }
  for (alpha2 in seq(0.1,0.95, by = 0.1)){
    data$yt_Brand[1] = data$Brand[1]
    for (i in 1:Tv){
      data$yt_Brand[i+1] <- alpha2*data$Brand[i+1] + (1-alpha2)*data$yt_Brand[i]
    }
    for (alpha3 in seq(0.1,0.95, by = 0.1)){
      data$yt_Market[1] = data$Market[1]
      for (i in 1:Tv){
        data$yt_Market[i+1] <- alpha3*data$Market[i+1] + (1-alpha3)*data$yt_Market[i]
      }
      for (alpha4 in seq(0.1,0.95, by = 0.1)){
        data$yt_DAw[1] = 100
        for (i in 1:Tv){
          data$yt_DAw[i+1] <- alpha4*data$Dis.Awarn[i+1] + (1-alpha4)*data$yt_DAw[i]
        }
        for (alpha5 in seq(0.1,0.95, by = 0.1)){
          data$yt_DR[1] = 10
          for (i in 1:Tv){
            data$yt_kwd[i+1] <- alpha5*data$Kwd[i+1] + (1-alpha5)*data$yt_kwd[i]
          }
            par_1 = c(par_1,alpha1)
            par_2 = c(par_2,alpha2)
            par_3 = c(par_3,alpha3)
            par_4 = c(par_4,alpha4)
            par_5 = c(par_5,alpha5)
            model <- lm(log(Output)~log(yt_tv) + log(yt_Brand) + log(yt_Market)+
                          log(yt_DAw) + log(yt_kwd), data = data)
            obj5 <- c(obj5,AIC(model))
          
        }
      }
    }
  }
}
Combined <- data.frame(alpha_tv = par_1, alpha_Brand = par_2,
                       alpha_Market = par_3,alpha_DAC = par_4,
                       alpha_kwd = par_5, AIC = obj5)
Combined$alpha_tv[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Brand[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Market[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_DAC[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_kwd[which(Combined$AIC == min(Combined$AIC))]



##six dimensional grid search:( 531441 points)
data$yt_Brand <-0
data$yt_tv <- 0
data$yt_Market <- 0
data$yt_DAw <- 0
data$yt_kwd <- 0
data$yt_DR <- 0
par_1 <- c()
par_2 <- c()
par_3 <- c()
par_4 <- c()
par_5 <- c()
par_6 <- c()
obj6 <- c()
for (alpha1 in seq(0.1,0.95, by = 0.1)){
  data$yt_tv[1] = data$TV[1]
  
  for (i in 1:Tv){
    data$yt_tv[i+1] <- alpha1*data$TV[i+1] + (1-alpha1)*data$yt_tv[i]
  }
  for (alpha2 in seq(0.1,0.95, by = 0.1)){
    data$yt_Brand[1] = data$Brand[1]
    for (i in 1:Tv){
      data$yt_Brand[i+1] <- alpha2*data$Brand[i+1] + (1-alpha2)*data$yt_Brand[i]
    }
    for (alpha3 in seq(0.1,0.95, by = 0.1)){
      data$yt_Market[1] = data$Market[1]
      for (i in 1:Tv){
        data$yt_Market[i+1] <- alpha3*data$Market[i+1] + (1-alpha3)*data$yt_Market[i]
      }
      for (alpha4 in seq(0.1,0.95, by = 0.1)){
        data$yt_DAw[1] = 100
        for (i in 1:Tv){
          data$yt_DAw[i+1] <- alpha4*data$Dis.Awarn[i+1] + (1-alpha4)*data$yt_DAw[i]
        }
        for (alpha5 in seq(0.1,0.95, by = 0.1)){
          data$yt_DR[1] = 10
          for (i in 1:Tv){
            data$yt_DR[i+1] <- alpha5*data$Dis.retar[i+1] + (1-alpha5)*data$yt_DR[i]
          }
          for (alpha6 in seq(0.1,0.95, by = 0.1)){
            data$yt_kwd[1] = data$Kwd[1]
            for (i in 1:Tv){
              data$yt_kwd[i+1] <- alpha6*data$Kwd[i+1] + (1-alpha6)*data$yt_kwd[i]
            }
            par_1 = c(par_1,alpha1)
            par_2 = c(par_2,alpha2)
            par_3 = c(par_3,alpha3)
            par_4 = c(par_4,alpha4)
            par_5 = c(par_5,alpha5)
            par_6 = c(par_6,alpha6)
            model <- lm(log(Output)~log(yt_tv) + log(yt_Brand) + log(yt_Market)+
                      log(yt_DAw) + log(yt_kwd) + log(yt_DR), data = data)
            obj6 <- c(obj6,AIC(model))
          }
        }
      }
    }
  }
}
Combined <- data.frame(alpha_tv = par_1, alpha_Brand = par_2,
                       alpha_Market = par_3,alpha_DAC = par_4,
                       alpha_DR =par_5,
                       alpha_kwd = par_6, AIC = obj6)
Combined$alpha_tv[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Brand[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_Market[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_DAC[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_DR[which(Combined$AIC == min(Combined$AIC))]
Combined$alpha_kwd[which(Combined$AIC == min(Combined$AIC))]
#write.csv(Combined, 'SixDhalf.csv')
