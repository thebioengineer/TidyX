## TidyX Episode 165 - The Power of Plotting Compels You - 1

##### Base R Plot Tutorial

## get data

library(Lahman)

dat <- Batting[
  Batting$yearID >= 2010 &
    Batting$AB >= 100, 
  c("lgID", "R","H")
  ]
dat <- droplevels(dat)
head(dat)



### Scatter Plot -----------------------------------------

## basic plot
plot(x = dat$H, y = dat$R)

with(dat, plot(x=H, y=R))




## change the point types
plot(x = dat$H, 
     y = dat$R, 
     pch = 19)


## available shapes
plot(x = rep(1:10, times = 10),
     y = rep(1:10, each = 10),
     pch = 1:100)




## change the color
plot(x = dat$H, 
     y = dat$R, 
     pch = 19,
     col = "blue")




## change the size
plot(x = dat$H, 
     y = dat$R, 
     pch = 19,
     cex = 4)




## Add a trend line
fit <- lm(R ~ H, data = dat)

plot(x = dat$H, 
     y = dat$R, 
     pch = 19,
     col = 'light grey')
abline(fit,
       col = 'red',
       lwd = 5,
       lty = 2)




# alternative approach
plot(x = dat$H, 
     y = dat$R, 
     pch = 19,
     col = 'light grey')
abline(a = fit$coef[1],
       b = fit$coef[2],
       col = 'red',
       lwd = 5,
       lty = 5)





## Color dots by group
plot(x = dat$H, 
     y = dat$R, 
     pch = 19,
     col = dat$lgID)
abline(fit,
       col = 'blue',
       lwd = 5,
       lty = 2)




# bespoke colors
clrs <- c("#FFCCCC", "#CCFF00")

plot(x = dat$H, 
     y = dat$R, 
     pch = 19,
     col = clrs[dat$lgID])
abline(fit,
       col = 'black',
       lwd = 5,
       lty = 2)




# separate regression lines
fit_al <- lm(R ~ H, data = dat, subset = lgID == 'AL')
fit_nl <- lm(R ~ H, data = dat, subset = lgID == 'NL')

clrs <- c("#FFCCCC", "#CCFF00")

plot(x = dat$H, 
     y = dat$R, 
     pch = 19,
     col = clrs[dat$lgID])
abline(fit_al,
       col = 'black',
       lwd = 5,
       lty = 2)
abline(fit_nl,
       col = 'black',
       lwd = 5,
       lty = 2)




# create facets
par(mfrow = c(1,2))

plot(x = dat$H[dat$lgID == 'AL'], 
     y = dat$R[dat$lgID == 'AL'], 
     pch = 19,
     col = "#FFCCCC")
abline(fit_al,
       col = 'black',
       lwd = 5,
       lty = 2)

plot(x = dat$H[dat$lgID == 'NL'], 
     y = dat$R[dat$lgID == 'NL'], 
     pch = 19,
     col = "#CCFF00")
abline(fit_nl,
       col = 'black',
       lwd = 5,
       lty = 2)




## Add a legend
par(mfrow = c(1,2))

plot(x = dat$H[dat$lgID == 'AL'], 
     y = dat$R[dat$lgID == 'AL'], 
     pch = 19,
     col = "#FFCCCC")
abline(fit_al,
       col = 'black',
       lwd = 5,
       lty = 2)
legend('topleft', ## or x-y coordinates
       legend = c('AL', 'NL'),
       pch = c(19, 12),
       col = clrs)

plot(x = dat$H[dat$lgID == 'NL'], 
     y = dat$R[dat$lgID == 'NL'], 
     pch = 12,
     col = "#CCFF00")
abline(fit_nl,
       col = 'black',
       lwd = 5,
       lty = 2)





# Add labels
par(mfrow = c(1,1)) # or dev.off()

shapes <- c(19, 3)

plot(x = dat$H, 
     y = dat$R, 
     pch = shapes[dat$lgID],
     cex = 2,
     col = clrs[dat$lgID],
     xlab = 'Hits',
     ylab = 'Runs',
     main = 'Runs ~ Hits\nAs players get more hits they increase their runs',
     sub = 'Data Courtesy of {Lahman} Package')

abline(a = fit$coef[1],
       b = fit$coef[2],
       col = 'black',
       lwd = 5,
       lty = 2)
legend("topleft",
       legend = c('AL', 'NL'),
       cex = 1.2,
       pch = shapes,
       col = clrs)



### Line Plots -----------------------------------------
### time series

### regression lines with confidence and prediction intervals

### Boxplot -----------------------------------------


### Bar Plot, Histogram & Density Plot -----------------



