library(boot)
library(MASS)
data(Cars93)

hist(Cars93$MPG.city, freq= FALSE, main = 'Histogram of MPG (City)')

boot_mean <- function(mydata, i)
    return(mean(mydata[i]))

mymean <- mean(Cars93$MPG.city)
mysd = sd(Cars93$MPG.city)
c(mymean - qt(0.975, 93 - 1 ) * mysd / sqrt(93),
    mymean + qt(0.975, 93 - 1 ) * mysd / sqrt(93))

set.seed(1008)
boot.mpgcity <- boot(Cars93$MPG.city, boot_mean, R = 120)
boot.mpgcity
boot.ci(boot.mpgcity)

boot_ratio <- function(mydata, i)
    return(exp(mean(log(mydata$MPG.highway[i] / mydata$MPG.city[i]))))

rat <- exp(mean(log(Cars93$MPG.highway / Cars93$MPG.city)))

set.seed(1008)
boot.rat <- boot(Cars93, boot_ratio, R = 120)
boot.rat
boot.ci(boot.rat)

wtmean <- mean(Cars93$Weight)
wtsd = sd(Cars93$Weight)
c(wtmean - qt(0.975, 93 - 1 ) * wtsd / sqrt(93),
    wtmean + qt(0.975, 93 - 1 ) * wtsd / sqrt(93))

set.seed(1008)
boot.wt <- boot(Cars93$Weight, boot_mean, R = 120)
boot.wt
boot.ci(boot.wt)

plot(density(boot.wt$t), ylim = c(0, 0.008))
curve(dnorm(x, mean(boot.wt$t), sd(boot.wt$t)), add = TRUE, lty = 'dashed')
