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


##---------------------------------------------------------------------------------------

###-----------------------------------------------
### ggplot2绘制美观的柱状图 ------------------------
###-----------------------------------------------

## 绘制柱状图
library(janeaustenr)
library(tidyverse)
library(tidytext)

austen_books() %>%
  select(2, 1) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  print() %>% 
  # A token is a meaningful unit of text, most often a word, 
  # that we are interested in using for further analysis, 
  # and tokenization is the process of splitting text into tokens.
  count(word, sort = TRUE) %>% 
  top_n(10) ->
  top_10_austen_bigrams

library(ggthemes)
win.graph()

## 一元映射
top_10_austen_bigrams %>% 
  print() %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "#E7962A") +  # `柱子`填充色为“橘黄色”
  coord_flip() +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "#FCF0E1"), # 图片背景填充色为“土黄色”
        plot.background = element_rect(fill = "#FCF0E1")) +
  geom_text(aes(label = n), hjust = -0.3) + # hjust/vjust调整`标签`的位置
  # 标题
  labs(title = "Words Frequency Count",
       subtitle = "Made in 2017-08-25",
       caption = "Source: janeaustenr package",
       xlab = "", ylab = "")

## 二元映射
austen_books() %>%
  select(2, 1) %>%
  filter(book %in% c("Sense & Sensibility", "Pride & Prejudice")) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 1) %>%
  anti_join(stop_words, by = "word") %>% 
  print() %>% 
  group_by(book) %>% 
  count(book, word, sort = TRUE) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(n2 = if_else(book == "Sense & Sensibility", -n, n)) %>% 
  print(n = Inf) %>% 
  ggplot(aes(x = reorder(word, n2), y = n2, fill = book)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "#FCF0E1"), # 图片背景填充色为“土黄色”
        plot.background = element_rect(fill = "#FCF0E1"),
        legend.background = element_blank()) +
  # 标题
  labs(title = "Words Frequency Count 2",
       subtitle = "Made in 2017-08-25",
       caption = "Source: janeaustenr package",
       xlab = "", ylab = "",
       fill = "") + # fill = ""表示删掉`图例`的标题
  scale_fill_manual(values = c("#F26B68", "#0991DB")) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) # hjust/vjust调整`标签`的位置，这里用hjust = -0.3存在问题，因为`标签都会向右偏`

# 因此采用另一种方法，分别给`Sense & Sensibility`和`Pride & Prejudice`绘制标签
austen_books() %>%
  select(2, 1) %>%
  filter(book %in% c("Sense & Sensibility", "Pride & Prejudice")) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 1) %>%
  anti_join(stop_words, by = "word") %>% 
  print() %>% 
  group_by(book) %>% 
  count(book, word, sort = TRUE) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(n2 = if_else(book == "Sense & Sensibility", -n, n)) ->
  austen_2_books
print(austen_2_books)  

austen_2_books %>% 
  ggplot(aes(x = reorder(word, n2), y = n2, fill = book)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "#FCF0E1"), # 图片背景填充色为“土黄色”
        plot.background = element_rect(fill = "#FCF0E1"),
        legend.background = element_blank()) +
  # 标题
  labs(title = "Words Frequency Count 2",
       subtitle = "Made in 2017-08-25",
       caption = "Source: janeaustenr package",
       xlab = "", ylab = "",
       fill = "") +
  scale_fill_manual(values = c("#F26B68", "#0991DB")) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  geom_text(data = filter(austen_2_books, book == "Sense & Sensibility"), 
            aes(x = , y = , label = n), hjust = 1.5) +
  geom_text(data = filter(austen_2_books, book != "Sense & Sensibility"), 
            aes(x = , y = , label = n), hjust = -0.5)



























