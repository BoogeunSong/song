################################
############½Ã°¢È­##############
################################

##ggplot
getwd()
setwd("C:/rproject/ubion")
#install.packages("ggplot2", dependencies = T, type = "binary")
library(ggplot2)
mpg
mpg <- mpg

colnames(mpg)
??ggplot
help(ggplot)
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ, y=hwy)) + geom_smooth()
ggplot(mpg, aes(x=cty, y=hwy)) + geom_smooth() + geom_point()
ggplot(mpg, aes(x=displ, y=hwy,color = drv, shape = class,
                size = displ)) + geom_smooth() + geom_point()

ggplot(mpg, aes(x=displ, y=hwy, alpha =cyl)) + geom_point()


b <- ggplot(seals, aes(x= long, y=lat))
b + geom_curve(aes(yend = lat +10, 
                   xend = long +10),
               curvature = 10)

a <- ggplot(economics, aes(date, unemploy))
a + geom_ribbon(aes(ymin = unemploy -900,
                    ymax = unemploy +900)) 











