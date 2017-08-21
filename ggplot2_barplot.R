##------------------------------------
## ggplot2绘制 `柱状图` ---------------
##------------------------------------

# Basic bar plot
set.seed(1234)
df1 <- data_frame(x1 = sample(letters[1:5], 200, replace = TRUE), y1 = round(rpois(200, 20)))
df1

## 基础柱状图
f <- ggplot(data = df1, aes(x = x1, y = y1))
f + geom_bar(stat = "identity")

## `有序`条形图
f <- df1 %>% 
  group_by(x1) %>% 
  summarize(y1_count = sum(y1)) %>% 
  arrange(desc(y1_count)) %>% 
  print() %>% 
  ggplot(aes(x = reorder(x1, X = y1_count), y = y1_count)) # 这里`reorder(x1, X = y1_count)`绘制`有序`柱状图
  
# Change fill color and add labels at the top (vjust = -0.3)
f + geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = y1_count), vjust = -0.3, size = 3.5) + # 这里vjust = -0.3表示向上平移0.3个单位
  theme_minimal()
  
# Label inside bars, vjust = 1.6
f + geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=y1_count), vjust=1.6, color="white", size = 3.5)+  # 这里vjust = 1.6表示向下平移1.6个单位
  theme_minimal()

# Change bar plot line colors by groups
f + 
  geom_bar(aes(color = x1), stat="identity", fill="white")
# Change bar plot fill colors by groups
f + 
  geom_bar(aes(fill = x1), stat="identity")
# Change outline color manually: custom color
f + 
  geom_bar(aes(color = x1), stat="identity", fill="white") +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "tomato2", "skyblue"))
# Change fill color manually: custom color
f + 
  geom_bar(aes(fill = x1), stat="identity") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "tomato2", "skyblue"))



























