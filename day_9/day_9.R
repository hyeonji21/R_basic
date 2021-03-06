# 인터랙티브 그래프

install.packages("plotly")
library(plotly)

library(ggplot2)
p <- ggplot(data = mpg, aes(x=displ, y=hwy, col=drv)) + geom_point()

ggplotly(p)

p <- ggplot(data = diamonds, aes(x=cut, fill=clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)

# dygraphs 패키지 -> 인터랙티브 시계열 그래프 만들기
install.packages("dygraphs")
library(dygraphs)

economics <- ggplot2::economics
head(economics)

library(xts) # 데이터가 시간 순서 속성을 지니는 타입
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)

dygraph(eco)

dygraph(eco) %>% dyRangeSelector()  # 날짜 범위 선택 기능

eco_a <- xts(economics$psavert, order.by = economics$date)
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)


eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c("psavert", "unemploy")
head(eco2)

dygraph(eco2) %>% dyRangeSelector()
