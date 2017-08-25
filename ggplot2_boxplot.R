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
      

##------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggthemes)
data("ToothGrowth")
tooth_growth <- ToothGrowth %>% 
  mutate(dose = as.factor(dose)) %>% 
  as_tibble()
tooth_growth


# len ~ supp
ggplot(tooth_growth, aes(x = supp, y = len)) +
  geom_boxplot(aes(fill = supp), notch = TRUE, width = 0.4) + # width = 0.4 调整箱体的宽度
  scale_fill_manual(values = c("#F26B68", "#0991DB")) +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = "#FCF0E1"),  # 图片背景填充颜色
        panel.background = element_rect(fill = "#FCF0E1"), # 图片中格子填充颜色
        legend.background = element_blank()) +             # 去掉图例背景
  labs(title = "Boxplot of the length", 
       subtitle = "Made in 2017-08-25",
       caption = "Source: ToothGrowth data") 


# 仅绘制supp中`VC`种类的箱线图
ggplot(tooth_growth, aes(x = supp, y = len)) +
  geom_boxplot(notch = TRUE, width = 0.2, fill =  "#0991DB") + # width = 0.4 调整箱体的宽度
  scale_x_discrete(limits = c("VC")) +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = "#FCF0E1"),
        panel.background = element_rect(fill = "#FCF0E1"),
        legend.background = element_blank()) +
  labs(title = "Boxplot of the length", 
       subtitle = "Made in 2017-08-25",
       caption = "Source: ToothGrowth data") 

## len ~ dose
ggplot(tooth_growth, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose), notch = TRUE, width = 0.4) + # width = 0.4 调整箱体的宽度
  scale_fill_manual(values = c("#F26B68", "#0991DB", "lightblue")) +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = "#FCF0E1"),
        panel.background = element_rect(fill = "#FCF0E1"),
        legend.background = element_blank()) +
  scale_x_discrete(limits = c("2", "1", "0.5")) +  # 调整3个箱子的位置
    labs(title = "Boxplot of the length", 
       subtitle = "Made in 2017-08-25",
       caption = "Source: ToothGrowth data")
