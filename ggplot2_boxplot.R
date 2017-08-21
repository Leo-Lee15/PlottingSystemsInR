##----------------------------------------
## ggplot2包绘制箱线图 --------------------
##----------------------------------------

## load the necessary packages
library(ggplot2)
library(dplyr)

## data
data("ToothGrowth")
as_tibble(ToothGrowth)
sapply(ToothGrowth, function(x) length(unique(x)))

## base ggplot
e <- ggplot(ToothGrowth, aes(x = supp, y = len)); e
e +
  geom_boxplot()

## # Rotate the box plot
e + 
  geom_boxplot() + 
  coord_flip()

## Notched box plot
e + 
  geom_boxplot(notch = TRUE)
## Box plot with mean points
e + geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point",
               shape = 18, size = 4, color = "blue")

## Choose which items to display: group  "OJ", "VC"
unique(ToothGrowth$supp)
e + 
  geom_boxplot() +
  scale_x_discrete(limits=c("VC"))  ## only show the "VC" group
## 这里只绘制supp = "VC"类型的箱线图

# Change outline colors by dose (groups)
e +
  geom_boxplot(aes(color = factor(dose))) +
  tidyquant::theme_tq() +
  scale_color_discrete(name = "dose") # 将图例的名称修改为"dose"
# Change fill color by dose (groups)
e + 
  geom_boxplot(aes(fill = factor(dose))) +
  tidyquant::theme_tq() +
  scale_color_discrete(name = "dose")







