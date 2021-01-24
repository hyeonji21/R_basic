# 데이터 전처리
# 데이터 전처리 작업에 가장 많이 사용되는 패키지 : dplyr

"""
filter() : 행 추출
select() : 열 추출
arrange() : 정렬
mutate() : 변수 추가
summarise() : 통계치 산출
group_by() : 집단별로 나누기
left_join() : 데이터 합치기(열)
bind_rows() : 데이터 합치기(행)
"""

library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

# 1반 학생들의 데이터만 추출
exam %>% filter(class == 1)
# %>% : 파이프 연산자

exam %>% filter(class == 2)
exam %>% filter(class != 1)
exam %>% filter(class != 3)

exam %>% filter(math > 50)
exam %>% filter(math < 50)
exam %>% filter(english >= 80)
exam %>% filter(english <= 80)

exam %>% filter(class == 1 & math >= 50)
exam %>% filter(class == 2 & english >= 80)

exam %>% filter(math >= 90 | english >= 90)
exam %>% filter(english < 90 | science < 50)

exam %>% filter(class == 1 | class == 3 | class == 5)
exam %>% filter(class %in% c(1, 3, 5))
# %in% 매치 연산자

class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2)

mean(class1$math)
mean(class2$math)

# 연습

# 1. displ 4 이하인 자동차의 hwy가 평균적으로 더 높다.
mpg <- as.data.frame(ggplot2::mpg)
mpg

displ1 <- mpg %>% filter(displ <= 4)
displ2 <- mpg %>% filter(displ >= 5)

mean(displ1$hwy)
mean(displ2$hwy)

# 2. toyota의 cty가 평균적으로 더 높다.
View(mpg)
audi = mpg %>% filter(manufacturer == "audi")
toyota = mpg %>% filter(manufacturer == "toyota")

mean(audi$cty)
mean(toyota$cty)

# 3. "chevrolet", "ford", "honda" 회사의 전체 평균
hwy <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(hwy$hwy)

# select() : 일부 변수만 추출해 활용할때

exam %>% select(math)
exam %>% select(english)
exam %>% select(class, math, english)

exam %>% select(-math) # math 빼고 
exam %>% select(-math, -english)

# dplyr 함수 조합
exam %>% filter(class == 1) %>% select(english)

exam %>% 
  filter(class == 1) %>% 
  select(english)

exam %>% 
  select(id, math) %>% 
  head

exam %>% 
  select(id, math) %>% 
  head(10)

# 연습
# class, cty 변수만으로 구성된 데이터
new <- mpg %>% select(class, cty)
new %>% head
head(new)

# suv와 compact 중에서 cty 평균이 높은 것은 compact.
suv <- mpg %>% filter(class=="suv")
compact <- mpg %>% filter(class == "compact")
mean(suv$cty)
mean(compact$cty)


# 오름차순
exam %>% arrange(math)

# 내림차순
exam %>% arrange(desc(math))

exam %>% arrange(class, math)

# 연습
audi <- mpg %>% filter(manufacturer == "audi")
arrange_audi <- audi %>% arrange(audi$hwy)
head(arrange_audi, 5)


# 파생변수 추가하기

exam %>% 
  mutate(total = math + english + science) %>% 
  head
exam %>% 
  mutate(total = math + english + science, 
         mean = (math + english + science)/3) %>% 
  head
exam %>% 
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>% 
  head

exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(total) %>% 
  head

# 연습

mpg2 <- mpg %>% mutate("합산 연비 변수" = cty + hwy)
head(mpg2, 5)

mpg2 <- mpg2 %>% mutate("평균 연비 변수" = (cty + hwy) /2)
head(mpg2, 5)

mpg2 %>% arrange("평균 연비 변수")
head(mpg2$"평균 연비 변수", 3)

mpg %>% 
  mutate("합산 연비 변수" = cty + hwy, "평균 연비 변수" = (cty + hwy) / 2) %>% 
  arrange("평균 연비 변수") %>% 
  head(3)


# 집단별로 요약 : group_by(), summarise()

exam %>% summarise(mean_math = mean(math))
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n()) # 데이터가 몇 행으로 되어 있는지 '빈도'를 구하는 기능.

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(tot = (cty+hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

# 연습
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(5)

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

mpg %>% 
  group_by(class) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

mpg %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))


# 가로로 합치기

test1 <- data.frame(id=c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
test2 <- data.frame(id=c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))
test1
test2

total <- left_join(test1, test2, by="id")
total

# 다른 데이터를 활용해 변수 추가

name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

exam_new <- left_join(exam, name, by="class")
exam_new

# 세로로 합치기
group_a <- data.frame(id=c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
group_b <- data.frame(id=c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_a
group_b

group_all <- bind_rows(group_a, group_b)
group_all

# 연습
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl= c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel

mpg <- as.data.frame(ggplot2::mpg)
mpg <- left_join(mpg, fuel, by="fl")

mpg %>% 
  select(model, fl, price_fl) %>% 
  head(5)

# 도전

midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>% mutate(add = (popadults-poptotal)/poptotal * 100)
midwest %>% 
  arrange(desc(add)) %>% 
  select(county, add) %>% 
  head(5)

midwest <- midwest %>% 
  mutate(add2 = ifelse(add >= 40, "large", 
                       ifelse(add >= 30, "middle", "small")))
table(midwest$add2)

midwest %>% 
  mutate(asian = (popasian/poptotal) * 100) %>% 
  arrange(asian) %>% 
  select(state, county, asian) %>% 
  head(10)

