# 결측치
df <- data.frame(sex=c("M", "F", NA, "M", "F"),
                 score=c(5, 4, 3, 4, NA))
df

# 결측치 확인하기
is.na(df)

table(is.na(df))

table(is.na(df$sex))
table(is.na(df$score))

mean(df$score)
sum(df$score)

# 결측치 제거하기
library(dplyr)
df %>% filter(is.na(score))
df %>% filter(!is.na(score))

df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)

sum(df_nomiss$score)

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

# 위처럼 일일이 변수를 지정해 결측치가 있는 행을 제거하지 않아도 x
# na.omit() 사용
# 분석에 사용될 수 있는 데이터까지 제거될 수 있으므로 filter() 권장
df_nomiss2 <- na.omit(df)
df_nomiss2

# 함수의 결측치 제외 기능 사용
# 결측치 제외하고 평균, 합계 산출
mean(df$score, na.rm=T)
sum(df$score, na.rm=T)

exam <- read.csv("csv_exam.csv")
exam[c(3, 8, 15), "math"] <- NA
exam

exam %>% summarise(mean_math = mean(math))
exam %>% summarise(mean_math = mean(math, na.rm=T))
exam %>% summarise(mean_math = mean(math, na.rm=T),
                   sum_math = sum(math, na.rm=T),
                   median_math = median(math, na.rm=T))

# 평균값으로 결측치 대체하기
mean(exam$math, na.rm=T)
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))
exam
mean(exam$math)

# 연습
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA
table(is.na(mpg$drv))
table(is.na(mpg$hwy))
mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))


# 이상치 제거
outlier <- data.frame(sex=c(1, 2, 1, 3, 2, 1),
                      score=c(5, 4, 3, 4, 2, 6))
outlier

table(outlier$sex)
table(outlier$score)

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats  # 상자 그림 통계치 출력

mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm=T))

# 연습
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10, 14, 58, 93), "drv"] <- "k"
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)

head(mpg$drv)
table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA)

boxplot(mpg$cty)
boxplot(mpg$cty)$stats

mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)

mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))
