library(ggplot2)

# 산점도그리기
ggplot(data=mpg, aes(x=displ, y=hwy))
ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point()

ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point() + xlim(3, 6)
ggplot(data=mpg, aes(x=displ, y=hwy)) + 
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)


# 연습
ggplot(data=mpg, aes(x=cty, y=hwy)) + geom_point()

ggplot(data=midwest, aes(x=poptotal, y=popasian)) +
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000)


# 막대그래프
library(dplyr)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_mpg

ggplot(data = df_mpg, aes(x=drv, y=mean_hwy)) + geom_col()
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()


# 빈도 막대 그래프
ggplot(data = mpg, aes(x = drv)) + geom_bar()
ggplot(data=mpg, aes(x=hwy)) + geom_bar()

# 연습
# 1.
View(mpg)
mpg <- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))%>% 
  head(5)

ggplot(data=mpg, aes(x=reorder(manufacturer, -mean_cty), y=mean_cty)) + geom_col()

# 2.
library(dplyr)
mpg <- mpg
ggplot(data=ggplot2::mpg, aes(x=class)) + geom_bar()


# 선 그래프
# 시계열 그래프 : 일정 시간 간격을 두고 나열된 데이터
ggplot(data=economics, aes(x=date, y=unemploy)) + geom_line()

# 연습
ggplot(data=economics, aes(x=date, y=psavert)) + geom_line()


# 상자 그림(Box Plot)
# 데이터의 분포(퍼져 있는 형태)를 직사각형 상자 모양으로 표현한 그래프
ggplot(data=ggplot2::mpg, aes(x=drv, y=hwy)) + geom_boxplot()

# 연습
View(ggplot2::mpg)
mpg <- as.data.frame(ggplot2::mpg)
ex_mpg <- mpg %>% 
  filter(class %in% c("compact", "subcompact", "suv"))

ggplot(data=ex_mpg, aes(x=class, y=cty)) + geom_boxplot()





