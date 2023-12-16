
##### Base R Plot Tutorial
## get data
library(Lahman)

dat <- Batting[Batting$yearID >= 2010 & Batting$AB >= 100, c("lgID", "R","H")]
dat <- droplevels(dat)
head(dat)

### Scatter Plot -----------------------------------------
## basic plot
plot(x = dat$H, 
     y = dat$R)

## change the point types
plot(x = dat$H, 
     y = dat$R, 
     pch = 19)

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
       lty = 2)


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
legend('topleft',
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

## get data
library(Lahman)

dat <- Batting[
  Batting$yearID >= 2000 & Batting$yearID < 2020 & Batting$AB >= 100,
  c("yearID", "teamID", "lgID", "R", "H")
  ]
dat <- droplevels(dat)
head(dat)

dat_yr <- aggregate(cbind(R, H) ~ yearID,
                  data = dat,
                  FUN = mean)

head(dat_yr)

### Basic plot
plot(x = dat_yr$yearID, y = dat_yr$H)

### Add line
plot(x = dat_yr$yearID, y = dat_yr$H, type = "l")

### lines and dots
plot(x = dat_yr$yearID, y = dat_yr$H, type = "b")

### change point type
plot(x = dat_yr$yearID, 
     y = dat_yr$H, 
     type = "b",
     pch = 19)

### change line type
plot(x = dat_yr$yearID, 
     y = dat_yr$H, 
     type = "b",
     lty = 2,
     pch = 19)

### dress up the plot
plot(x = dat_yr$yearID, 
     y = dat_yr$H, 
     type = "b",
     lty = 2,
     pch = 19,
     cex = 1.4,
     lwd = 2,
     ylim = c(80, 105))

### regression lines
fit <- lm(H ~ yearID, data = dat_yr)
summary(fit)

plot(x = dat_yr$yearID, 
     y = dat_yr$H, 
     type = "b",
     pch = 19,
     cex = 1.4,
     lwd = 2,
     main = 'Average hits per season has gone down since 2000',
     xlab = 'year',
     ylab = 'Hits',
     ylim = c(80, 105))
abline(fit,
       col = 'red',
       lty = 2,
       lwd = 3)

## add confidence intervals and prediction intervals
dat_yr$pred <- predict(fit, newdata = dat_yr)

conf_intervals <- predict(fit, newdata = dat_yr, interval = "confidence")
conf_intervals <- subset(conf_intervals, select = -fit)
colnames(conf_intervals) <- c("ci_low", "ci_high")

pred_intervals <- predict(fit, newdata = dat_yr, interval = "prediction")
pred_intervals <- subset(pred_intervals, select = -fit)
colnames(pred_intervals) <- c("pi_low", "pi_high")

dat_yr <- cbind(dat_yr, conf_intervals, pred_intervals)
head(dat_yr)

plot(x = dat_yr$yearID, 
     y = dat_yr$H, 
     type = "b",
     pch = 19,
     cex = 1.4,
     lwd = 2,
     main = 'Average hits per season has gone down since 2000',
     xlab = 'year',
     ylab = 'Hits',
     ylim = c(80, 105))
abline(fit,
       col = 'red',
       lty = 2,
       lwd = 3)
lines(x = dat_yr$yearID, y = dat_yr$ci_low,
       col = 'blue',
       lty = 2,
       lwd = 3)
lines(x = dat_yr$yearID, y = dat_yr$ci_high,
       col = 'blue',
       lty = 2,
       lwd = 3)
lines(x = dat_yr$yearID, y = dat_yr$pi_low,
      col = 'green',
      lty = 2,
      lwd = 3)
lines(x = dat_yr$yearID, y = dat_yr$pi_high,
      col = 'green',
      lty = 2,
      lwd = 3)

## to save the plot
temp_pdf <- tempfile(fileext = ".pdf")

## without width/height params, it uses rstudio canvas size
pdf(temp_pdf, width = 5, height = 5 )

plot(x = dat_yr$yearID, 
     y = dat_yr$H, 
     type = "b",
     pch = 19,
     cex = 1.4,
     lwd = 2,
     main = 'Average hits per season has gone down since 2000',
     xlab = 'year',
     ylab = 'Hits',
     ylim = c(80, 105))
abline(fit,
       col = 'red',
       lty = 2,
       lwd = 3)
lines(x = dat_yr$yearID, y = dat_yr$ci_low,
      col = 'blue',
      lty = 2,
      lwd = 3)
lines(x = dat_yr$yearID, y = dat_yr$ci_high,
      col = 'blue',
      lty = 2,
      lwd = 3)
lines(x = dat_yr$yearID, y = dat_yr$pi_low,
      col = 'green',
      lty = 2,
      lwd = 3)
lines(x = dat_yr$yearID, y = dat_yr$pi_high,
      col = 'green',
      lty = 2,
      lwd = 3)

dev.off()

## open file in windows
shell.exec(temp_pdf)


### Boxplot -----------------------------------------


### Bar Plot, Histogram & Density Plot -----------------



