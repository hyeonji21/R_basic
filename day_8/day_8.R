# 단계 구분도 -> 지역별 통계치를 색깔의 차이로 표현한 지도
# 인구나 소득 같은 특성이 지역별로 얼마나 다른지 쉽게 이해 가능.

# 미국 주별 강력 범죄율 단계 구분도 만들기

install.packages("mapproj")
install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)
head(USArrests)

library(tibble)  # rownames_to_column() 사용

crime <- rownames_to_column(USArrests, var = "state")
crime$state <- tolower(crime$state)
str(crime)

install.packages("maps")
library(ggplot2)
states_map <- map_data("state")
str(states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map,
             interactive = T) # 마우스 움직임에 반응하는 인터랙티브 단계 구분도


# 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기
# kormaps2014 패키지 <- 대한민국의 지역 통계 데이터와 지도 데이터를 사용

install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

str(changeCode(korpop1))

library(dplyr)
korpop1 <- rename(korpop1, pop = 총인구_명, name = 행정구역별_읍면동)
korpop1$name <- iconv(korpop1$name, "UTF-8", "CP949")

str(changeCode(kormap1))

library(ggiraphExtra)
ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

# 대한민국 시도별 결핵 환자 수 단계 구분도

str(changeCode(tbc))
tbc$name <- iconv(tbc$name, "UTF-8", "CP949")
ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name,),
             map = kormap1, 
             interactive = T)
