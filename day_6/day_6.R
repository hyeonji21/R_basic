# '한국복지패널데이터' 분석 준비하기

# foreign 패키지를 이용하면 SPSS,SAS,STATA 등 다양한 통계분석 소프트웨어의 파일을 
# 불러올 수 있음.
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file="Koweps_hpc10_2015_beta1.sav", to.data.frame=T)
welfare <- raw_welfare

head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 1. 성별에 따른 월급 차이

# 성별 변수의 전처리 작업
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

class(welfare$sex)
table(welfare$sex)
welfare$sex <- ifelse(welfare$sex==9, NA, welfare$sex)
table(is.na(welfare$sex))
welfare$sex <- ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# 월급 변수 검토 및 전처리
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)

summary(welfare$income)
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income) # 이상치 결측 처리
table(is.na(welfare$income))

# 성별에 따른 월급 차이 분석
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data=sex_income, aes(x=sex, y=mean_income)) + geom_col()


# 2. 나이와 월급의 관계

# 나이 변수 검토 및 전처리
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

summary(welfare$birth)  # 이상치 확인
table(is.na(welfare$birth))  # 결측치 확인
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1    # 파생변수 만들기
summary(welfare$age)
qplot(welfare$age)

# 나이와 월급의 관계 분석하기
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
head(age_income)

ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_line()


# 3. 연령대에 따른 월급 차이

# 연령대 변수 검토 및 전처리하기
welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

# (월급 변수 전처리는 앞서 실행함.)
# 연령대에 따른 월급 차이 분석
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
ageg_income

ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + geom_col()

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))


# 4. 연령대 및 성별 월급 차이

# 연령대 및 성별 월급 평균표 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data = sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

ggplot(data = sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col(position = "dodge") + 
  scale_x_discrete(limits = c("young", "middle", "old"))

# 나이 및 성별 월급 차이 분석하기
# (연령대 구분은 x)
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))
head(sex_age)

ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex)) + geom_line()


# 5. 직업별 월급 차이

# 직업 변수 검토 및 전처리하기

library(dplyr)
class(welfare$code_job)
table(welfare$code_job)

library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names=T, sheet=2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, id="code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

# 직업별 월급 차이 분석
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

library(ggplot2)
ggplot(data = top10, aes(x=reorder(job, mean_income), y=mean_income)) +
  geom_col() +
  coord_flip()  # 막대를 오른쪽으로 90도 회전

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)

ggplot(data = bottom10, aes(x=reorder(job, -mean_income),
                            y=mean_income))+
  geom_col() +
  coord_flip() +
  ylim(0, 850)


# 6. 성별 직업 빈도

# 성별 직업 분도 분석

# 남성 직업 빈도 상위 10개 추출
job_male <- welfare %>% 
  filter(!is.na(job)&sex == "male") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male

# 여성 직업 빈도 상위 10개 추출
job_female <- welfare %>% 
  filter(!is.na(job)&sex == "female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female

ggplot(data = job_male, aes(x=reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

ggplot(data = job_female, aes(x=reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()


# 참고
# geom_bar() : 원자료를 이용해 막대 그래프를 만들 때 사용
# geom_col() : 요약표를 이용해 막대 그래프를 만들 때 사용


# 7. 종교 유무에 따른 이혼율

class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)


library(ggplot2)
qplot(welfare$religion)

# 변수 검토
class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                          ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

library(dplyr)
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))  # 소수 첫번째 자리까지
religion_marriage

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

divorce <- religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)
divorce

ggplot(data = divorce, aes(x=religion, y=pct)) + geom_col()

# 연령대 및 종교 유무에 다른 이혼율 분석

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
ageg_marriage

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

# 초년 제외, 이혼 추출
ageg_divorce <- ageg_marriage %>% 
  filter(ageg != "young" & group_marriage == "divorce") %>% 
  select(ageg, pct)
ageg_divorce

ggplot(data = ageg_divorce, aes(x=ageg, y=pct)) + geom_col()

# 연령대, 종교 유무, 결혼 상태별 비율표 만들기
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
ageg_religion_marriage

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

# 연령대 및 종교 유무별 이혼율 표 만들기
df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(ageg, religion, pct)
df_divorce

ggplot(data = df_divorce, aes(x=ageg, y=pct, fill=religion)) +
  geom_col(position = "dodge")


# 8. 지역별 연령대 비율

# 변수 검토
class(welfare$code_region)
table(welfare$code_region)

list_region <- data.frame(code_region=c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region

welfare <- left_join(welfare, list_region, id="code_region")

welfare %>% 
  select(code_region, region) %>% 
  head

# 지역별 연령대 비율 분석

region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))

head(region_ageg)

region_ageg <- welfare %>% 
  count(region, ageg) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100, 2))

ggplot(data = region_ageg, aes(x=region, y=pct, fill=ageg)) +
  geom_col() +
  coord_flip()  # 그래프 오른쪽으로 회전

# 노년층 비율 내림차순 정렬
list_order_old <- region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)
list_order_old

order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x=region, y=pct, fill=ageg)) + 
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits=order)

# 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)
levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c("old", "middle", "young"))
class(region_ageg$ageg)

levels(region_ageg$ageg)

ggplot(data = region_ageg, aes(x=region , y=pct, fill=ageg)) + 
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)
