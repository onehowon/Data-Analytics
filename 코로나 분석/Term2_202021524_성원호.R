---
  title: "Term2"
author: "성원호"
date: "12/7/2020"
output: html_document
---

#COVID-19와 포스트 코로나 이전 이후로 알아보는 현황과 소비 생활

##2020년 이래로 COVID-19이 심각한 전염병으로 분류되면서 단합 문화, 조직 문화 등과는 반대로 사회적 거리두기 운동이 펼쳐지고 있습니다. 따라서 국내 COVID-19의 시작 기점부터 포스트 코로나 시대까지의 다양한 분석을 통해 그 심각성을 다시 한 번 제고하고자합니다.

#목차

##1. 코로나 유행기 2020.02 ~ 2020.06

##1.1 성별 및 연령대에 따른 코로나 확진자 수
##1.2 코로나 감염이 잦은 지역의 분석과 감염경로
##2.3 확진자 방문장소 별 감염자 수

##2. 포스트 코로나 2020.06 ~

##2.1 포스트 코로나 이후 배달업 현황
##2.2 포스트 코로나 이후의 소비변화
##2.3 업종별 코로나로 인해 미치는 피해의 분석

```{r}
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(extrafont)
library(readr)
library(chron)
library(gridExtra)
library(lubridate)
library(stringr)
library(ggplotify)
#필요한 라이브러리 할당

#1 성별에 따른 코로나 감염자
sex <- read.csv("TimeGender.csv")
sex_2 <- sex %>% mutate(sex, death_rate = deceased/confirmed)
#sex 변수 내 death_rate라는 새로운 열을 추가(사망률)

sex_2$date <- as.Date(sex_2$date)
#as.date는 문자형 데이터를 날짜 형식으로 변환해주는 데이터
#날짜에 따라 분류하기 위해 sex_2 내의 date를 날짜 형식으로 변환

sex_2 %>% ggplot(aes(x = date, y = confirmed, fill = sex)) +
  theme_set(theme_gray(base_family='AppleMyungjo'))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_date(breaks = "month") +
  xlab(label = "날짜") +
  ylab(label = "확진자 수") +
  ggtitle(label = "성별군 누적 확진자") +  
  theme(text=element_text(color="darkblue")) +
  theme(axis.title=element_text(size=15)) +
  theme(plot.title=element_text(hjust = 0.4, size=15, color="darkgrey"))
#성별, 날짜에 따른 확진자 수 분류의 시각화
#성별에 따른 확진자 수를 살펴본 결과 남성보다 여성 확진자가 월등하게 많다는 점을 알 수 있었다.
```

```{r}
#연령대에 따른 코로나 확진자 수
age <- read.csv("Timeage.csv")
age_disease <- aggregate(cbind(confirmed,deceased)~age, age, sum) %>%
  mutate(age, death_rate = deceased/confirmed)
# cbind를 통해 age 내 confirmed,deceased 컬럼과 결합하고, 컬럼 기준으로 합계를 구해준 후 death_rate(사망률)이라는 변수를 새로 추가해줌

age_disease %>% ggplot() +
  theme_set(theme_gray(base_family='AppleMyungjo'))+
  geom_bar(mapping = aes(x = age, y = confirmed/1000000), #y는 사망률 표기
           stat = "identity", width=0.4, fill = "darkblue") +
  geom_point(mapping = aes(x = age, y = death_rate), 
             size = 2, shape = 24, fill = "red") + xlab(label = "연령대") +
  scale_y_continuous(name = expression("사망률"), 
                     sec.axis = sec_axis(~ . *1000000, name = "확진자 수")) +
  ggtitle("연령대별 확진자 수 및 사망률") +
  theme(text=element_text(color="darkgrey")) +
  theme(axis.title=element_text(size=16)) +
  theme(plot.title=element_text(hjust = 0.4, size=15, color="darkgrey"))
#연령대별 확진자 및 사망률에 대한 시각화
#분석 결과 10~20까지는 사망률이 0에 수렴하나, 60대, 80대에서 사망률이 가장 높은 것으로 나타났다. 또한 가장 전염이 많은 연령층은 20대라는 점 역시 파악할 수 있었다.
```

```{r}
#연령, 성별군에 따라 한 눈에 볼 수 있는 데이터 시각화
patient_total <- read.csv("Patientinfo.csv")
patient_total %>% filter(sex != '') %>% filter(age != '') %>%
  group_by(age, sex) %>% summarise(N=n()) %>%
  #환자 전체 정보에서 성별과 연령만 분류하여 한 번에 출력하고자 함
  ggplot(aes(x=age, y=N, fill=sex)) + 
  theme_set(theme_gray(base_family='AppleMyungjo')) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("연령, 성별군 확진자 수") +
  xlab(label = "연령군") +
  ylab(label = "확진자") + #x축과 y축의 라벨링 처리
  theme(text=element_text(color="darkgrey")) +
  theme(axis.title=element_text(size=16)) +
  theme(plot.title=element_text(hjust = 0.4, size=15, color="darkgrey"))

ggplot(sex_2, aes(x=date, y=death_rate, color=sex)) + 
  theme_set(theme_gray(base_family='AppleMyungjo'))+
  geom_point() + geom_line() +
  scale_x_date(breaks = "month") +
  xlab(label = "날짜") +
  ylab(label = "사망률") +
  ggtitle(label = "성별군 사망률") +  
  theme(text=element_text(color="darkgrey")) +
  theme(axis.title=element_text(size=15)) +
  theme(plot.title=element_text(hjust = 0.4, size=15, color="darkblue"))
#연령, 성별군에 따라 분류하여 전체적인 정보를 시각화한 결과 전체적인 전염률은 여성이 높으나, 그에 비해 사망률은 남성이 월등히 높다는 결론을 도출할 수 있었다. 또한 20대 확진자가 압도적으로 많으며, 사망률은 고령의 나이일수록 점차 증가하는 것을 볼 수 있었다.
```

```{r}
#2. 코로나 감염이 잦은 지역에 대한 분석
province <- read.csv("timeprovince.csv")
province$date <- as.Date(province$date)
#날짜형 데이터로 변환
ggplot(province, aes(date,confirmed)) + geom_line() +
  theme_set(theme_gray(base_family='AppleMyungjo'))+
  scale_x_date(breaks = "month") + #한 달마다의 감염 수치를 보여주게끔 함
  facet_wrap(~province, scales="free_y") +
  xlab(label = "날짜") + 
  ylab(label = "확진자") + #x,y축 라벨링
  ggtitle(label = "지역별 누적 확진자") +  
  theme(text=element_text(color="blue")) +
  theme(axis.title=element_text(size=14)) +
  theme(axis.text.x=element_text(angle = 45)) +
  theme(plot.title=element_text(hjust = 3, size=1, color="darkblue"))
#지역별 누적 확진자 수의 시각화 
```

```{r}
#2. 코로나 감염이 잦은 지역에 대한 분석
case <- read.csv("case.csv")
groupcase <- aggregate(confirmed~province+group, case, sum)
ggplot(case, aes(x=province, y=confirmed, fill=group)) + #x축은 지역, y축은 확진자 수
  theme_set(theme_gray(base_family='AppleMyungjo')) +
  geom_bar(stat="identity", position=position_dodge()) +
  xlab(label = "지역") +
  ylab(label = "확진자 수") + #x,y 축 라벨링
  ggtitle(label = "지역별 집단 감염자 수") +  
  theme(text=element_text(color="blue")) +
  theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(angle = 45)) +
  theme(plot.title=element_text(hjust = 0.5, size=20, color="blue"))
#지역별 집단 감염자 수의 비율은 2020년 7월까지의 정보를 토대로 대구가 가장 많이 발생했음을 알 수 있다.
```

```{r}
#2. 감염경로 분석
info <- read.csv("patientinfo.csv")
info$infection_case <- 
  recode(info$infection_case, "Bonghwa Pureun Nursing Home" = "요양원",
         "Changnyeong Coin Karaoke" = "코인 노래방", "Dongan Church" =  "교회",
         "contact with patient" = "확진자 접촉", "etc" = "기타",
         "Eunpyeong St. Mary's Hospital" = "병원", "Geochang Church" = "교회",
         "Guro-gu Call Center" = "콜센터", "Cheongdo Daenam Hospital" = "병원",
         "Gyeongsan Cham Joeun Community Center" = "요양원",
         "Gyeongsan Jeil Silver Town" = "실버타운",
         "Coupang Logistics Center" = "쿠팡 센터", "Itaewon Clubs" = "이태원 클럽",
         "Gyeongsan Seorin Nursing Home" = "요양원",
         "gym facility in Cheonan" = "체육관", "gym facility in Sejong" = "체육관",
         "Milal Shelter" = "장애인 시설", 
         "Ministry of Oceans and Fisheries" = "해양수산부",
         "Onchun Church" = "교회", "overseas inflow" = "해외 입국", 
         "Pilgrimage to Israel" = "이스라엘 성지순례",
         "River of Grace Community Church" = "교회", 
         "Seongdong-gu APT" = "아파트", "Shincheonji Church" = "교회", 
         "Suyeong-gu Kindergarten" = "유치원") #가시성을 위해 영어로 된 원본 데이터를 한글로 바꾸어줌.

x <- info %>% filter(sex != '') %>% filter(age != '') %>% 
  filter(infection_case != '') %>%
  group_by(infection_case, age, sex) %>% summarise(N = n())
#감염 경로, 연령, 성별을 기준으로 데이터 정렬

y <- aggregate(N~infection_case, x, sum)
# 변수에 감염 경로에 따라 환자 수의 합계(빈도) 할당

y %>% ggplot(aes(x = reorder(infection_case, y$N), N)) +
  theme_set(theme_gray(base_family='AppleMyungjo')) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab(label = "접촉 장소 유형") +
  ylab(label = "접촉자 수") +
  ggtitle("감염경로") +
  theme(text=element_text(color="blue")) +
  theme(axis.title=element_text(size=15)) +
  theme(plot.title=element_text(hjust = 0.5, size=20, color="darkblue"))
#확진자 접촉을 제외한 감염경로로는 해외입국과 교회에서 가장 많은 감염이 발생했음을 알 수 있었다.
```

```{r}
#3. 확진자 방문장소 분석
patient_route <- read.csv("patientroute.csv")
select_route <- patient_route %>% select(patient_id, type)
#환자 신원과 종류만 추출한 데이터를 새로운 변수에 할당
a <- inner_join(info, select_route, by = "patient_id")
#데이터 info와 select_route 변수에서 환자 아이디를 기준으로 데이터 병합
b <- a %>% filter(sex != '') %>% filter(age != '') %>% 
  group_by(type, age) %>% summarise(N = n())
#종류, 연령군에 따라 데이터 정렬

b %>% ggplot(aes(x=reorder(type, b$N), y=N, fill = age)) + 
  theme_set(theme_gray(base_family='AppleMyungjo')) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab(label = "방문장소") +
  ylab(label = "확진자") +
  ggtitle(label = "확진자 방문장소") +  
  theme(text=element_text(color="darkblue")) +
  theme(axis.title=element_text(size=15)) +
  theme(plot.title=element_text(hjust = 0.5, size=20, color="darkblue"))
#데이터 시각화
#그래프 분석 결과 병원, 상점, 레스토랑 등 대중들이 빈번하게 방문하는 장소들에 확진자들이 가장 많이 방문했음을 알 수 있었다.
```

```{r}
#1 단순 확진자 수와 배달 수 간의 관계
# 수도권 환자 수
patientinfo <- read.csv("patientinfo.csv")
seoul_patient = patientinfo %>%
  filter(province == "서울" | province == "인천" | province == "경기도") %>%
  #수도권 지역만 추출하여 새로운 변수에 할당
  mutate(district = paste(province, city)) %>%
  #mutate 함수로 지역에 province, city 컬럼 추가
  select(district, confirmed_date) %>%
  #district, confirmed_date 항목만 추출
  arrange(confirmed_date)
#감염일에 따라 정렬
seoul_patient_result = seoul_patient %>%
  group_by(confirmed_date) %>%
  summarise(
    confirmed_num = n()
  )
```

```{r}
# 코로나에 따른 수도권 배달 수
#결측치가 많고, 배달 취소 건수 등의 변수 등을 감안하고자 데이터 양의 피해를 최소화하고자 DLVR_RCEPT_TIME 컬럼을 사용하였음.
delivery <- read.csv("delivery.csv")
seoul_delivery = delivery %>%
  filter(DLVR_DSTN_SIDO == "경기도" | DLVR_DSTN_SIDO == "서울특별시" | DLVR_DSTN_SIDO == "인천광역시") %>%
  #수도권 배달량만 추출
  mutate(district = paste(DLVR_DSTN_SIDO, DLVR_DSTN_SIGUNGU)) %>%
  #mutate 함수로 시도, 시군구 컬럼 추가
  select(district, DLVR_RCEPT_TIME) %>%
  #지역별, 수취 시간 별 컬럼만 추출
  arrange(DLVR_RCEPT_TIME)
#수취 시간을 기준으로 정렬
seoul_delivery$DLVR_RCEPT_TIME = as.Date(seoul_delivery$DLVR_RCEPT_TIME)
#날짜형 데이터로 변환
seoul_delivery_result = seoul_delivery %>%
  group_by(DLVR_RCEPT_TIME) %>%
  summarise(
    delivery_num = n()
  )
```

```{r}
#2. 일일 확진자에 따른 배달업 현황
seoul_patient_result = merge(seoul_patient_result, seoul_delivery_result, by.x = "confirmed_date", by.y = "DLVR_RCEPT_TIME", all.x=T, all.y=T)

seoul_patient_result$confirmed_num[is.na(seoul_patient_result$confirmed_num)] = 0
# 확진자가 발생하지 않은 날은 일일 확진자 수 표기가 0임.

seoul_patient_result = seoul_patient_result %>%
  mutate(cum_confirmed_num = cumsum(confirmed_num))
# 누적확진자수 열 추가

seoul_patient_result$groupinfection = ifelse(seoul_patient_result$confirmed_date >= "2020-05-07" , T, F)
seoul_patient_result = as.data.frame(seoul_patient_result)
head(seoul_patient_result)
# 이태원 집단감염 시작일 2020-05-07(오피셜)

seoul_patient_result_a = na.omit(seoul_patient_result) %>%
  group_by(confirmed_num) %>% #확진자 컬럼 기준으로 정렬
  summarise(mean_delivery_num = mean(delivery_num, trim=0.05))
head(seoul_patient_result_a)
# 일일 확진자에 따른 평균 배달량 (outlier 제거를 위해 Trimmed mean 사용)

g1 = ggplot(seoul_patient_result) +
  geom_point(aes(x=confirmed_date, y=confirmed_num, col=groupinfection)) +
  geom_smooth(aes(x=confirmed_date, y=confirmed_num), method="loess") +
  ggtitle("수도권, 일일 확진자")
#수도권 지역 일일 확진자


g2 = ggplot(seoul_patient_result) +
  geom_point(aes(x=confirmed_date, y=cum_confirmed_num, col=groupinfection)) +
  ggtitle("수도권, 일일 누적 확진자")
#수도권 일일 누적 확진자

grid.arrange(g1, g2 , nrow=2, ncol=1)
#데이터 시각화
```

```{r}
#2. 이태원 집단감염 이후 배달업 현황 
ggplot(seoul_patient_result) +
  geom_point(aes(x=confirmed_date, y=delivery_num)) +
  geom_smooth(aes(x=confirmed_date, y=delivery_num), method="loess") +
  ggtitle("수도권 지역 일일 배달량")
#수도권 지역 일일 배달량

#이태원 집단감염 발발 이후 인근지역의 배달 상황
# 사용한 컬럼 : 마포구, 중구, 성동구, 강남구, 서초구, 동작구, 영등포구
patient <- read.csv("patientinfo.csv")
yongsan = patient %>% 
  filter(province == "서울") %>%
  filter(city == "용산구" | city == "마포구" | city == "중구" | city == "성동구" | city == "강남구" | city == "서초구" | city == "동작구" | city == "영등포구") %>% #서울 내에서 사용하기로 한 컬럼만 걸러냄.
  select(province, city, age, confirmed_date)
#지역, 도시, 연령대, 확진일 컬럼만 추출
yongsan_result = yongsan %>%
  group_by(confirmed_date) %>%
  summarise(
    confirmed_num = n()
  )

yongsan_delivery = delivery %>%
  filter(DLVR_DSTN_SIDO == "서울특별시") %>%
  filter(DLVR_DSTN_SIGUNGU == "용산구" | DLVR_DSTN_SIGUNGU == "마포구" | DLVR_DSTN_SIGUNGU == "중구" | DLVR_DSTN_SIGUNGU == "성동구" |
           DLVR_DSTN_SIGUNGU == "강남구" | DLVR_DSTN_SIGUNGU == "서초구" | DLVR_DSTN_SIGUNGU == "동작구" | DLVR_DSTN_SIGUNGU == "영등포구") %>%
  select(DLVR_DSTN_SIDO, DLVR_DSTN_SIGUNGU, DLVR_RCEPT_TIME)
yongsan_delivery$DLVR_RCEPT_TIME = as.Date(yongsan_delivery$DLVR_RCEPT_TIME)
yongsan_delivery_result = yongsan_delivery %>%
  #지역별 배달을 알아보기 위해 데이터 정제
  group_by(DLVR_RCEPT_TIME) %>%
  #수취시간에 따라 분류
  summarise(
    delivery_num = n()
  )

yongsan_delivery_result = merge(yongsan_result, yongsan_delivery_result, by.x = "confirmed_date", by.y = "DLVR_RCEPT_TIME", all.x=T, all.y =T)
#지역별 환자, 배달량 데이터 병합
yongsan_delivery_result$confirmed_num[is.na(yongsan_delivery_result$confirmed_num)] = 0
yongsan_delivery_result = yongsan_delivery_result %>%
  mutate(cum_confirmed_num = cumsum(confirmed_num))
yongsan_delivery_result$groupinfection = ifelse(yongsan_delivery_result$confirmed_date >= "2020-05-07" , T, F)
head(yongsan_delivery_result)


part_yongsan_delivery_result = yongsan_delivery_result %>%
  filter(confirmed_date <= "2020-06-14")
# 용산구 인접지역, 이태원 집단감염 발생 3주 동안에 대한 데이터
g3 = ggplot(part_yongsan_delivery_result ) +
  geom_point(aes(x=confirmed_date, y=delivery_num, color=groupinfection)) +
  geom_smooth(aes(x=confirmed_date, y=delivery_num), method="loess") +
  ggtitle("용산구 인근 지역, 이태원 집단감염 3주 동안의 배달량")


g4 = ggplot(yongsan_delivery_result) +
  geom_point(aes(x=confirmed_date, y=delivery_num, col=groupinfection)) +
  geom_smooth(method="loess",  aes(x=confirmed_date, y=delivery_num)) +
  ggtitle("용산구 인근지역, 배달량 추이")
# 용산구 인근지역 전체기간에 대한 분석

grid.arrange(g3, g4, nrow=2, ncol=1)

# 이태원 집담감염은 이후 인근 지역 배달량에 아무런 상관관계가 없다는 것을 알 수 있었음.
```

```{r}
#3. 포스트 코로나 국민 소비 생활에 대한 분석
card <- read.csv("card_20200717.csv", encoding = 'UTF-8')
str(card)

health = card %>%
  filter(mrhst_induty_cl_nm == "건강식품(회원제형태)" | mrhst_induty_cl_nm == "인삼 제품" | mrhst_induty_cl_nm == "홍삼 제품" |
           mrhst_induty_cl_nm == "기타건강식")
rest = card %>%
  filter(mrhst_induty_cl_nm == "서양음식" | mrhst_induty_cl_nm == "일반한식" | mrhst_induty_cl_nm == "중국음식" | mrhst_induty_cl_nm == "한정식" | 
           mrhst_induty_cl_nm == "일식회집" | mrhst_induty_cl_nm == "갈비전문점")
market = card %>%
  filter(mrhst_induty_cl_nm == "슈퍼 마켓" | mrhst_induty_cl_nm == "제과점" | mrhst_induty_cl_nm == "정 육 점" | mrhst_induty_cl_nm == "편 의 점" | 
           mrhst_induty_cl_nm == "스넥" | mrhst_induty_cl_nm == "농축수산가공품" | mrhst_induty_cl_nm == "농축수산품")
drink = card %>% 
  filter(mrhst_induty_cl_nm == "유흥주점" | mrhst_induty_cl_nm == "주류판매점" | mrhst_induty_cl_nm == "주점" | mrhst_induty_cl_nm == "칵테일바" | 
           mrhst_induty_cl_nm == "단란주점")
kitc = card %>% 
  filter(mrhst_induty_cl_nm == "주방용식기" | mrhst_induty_cl_nm == "주방 용구"  | mrhst_induty_cl_nm == "기타주방용구")
#식품, 유흥 등 다양한 소비 칼럼에 대한 변수 설정

health$selng_cascnt = as.numeric(health$selng_cascnt)
health$salamt = as.numeric(health$salamt)

rest$selng_cascnt = as.numeric(rest$selng_cascnt)
rest$salamt = as.numeric(rest$salamt)

market$selng_cascnt = as.numeric(market$selng_cascnt)
market$salamt = as.numeric(market$salamt)

drink$selng_cascnt = as.numeric(drink$selng_cascnt)
drink$salamt = as.numeric(drink$salamt)

kitc$selng_cascnt = as.numeric(kitc$selng_cascnt)
kitc$salamt = as.numeric(kitc$salamt)
#chr형은 연산을 위해 num화

h = health %>%
  group_by(receipt_dttm) %>%
  summarise(
    sell = sum(selng_cascnt),
    med_salamt = median(salamt)
  ) %>%
  mutate(groupinfection = ifelse(receipt_dttm >= "2020-05-07", T, F))
#이태원 집단감염 이후를 기준으로 분석
#데이터 정제
hg = ggplot(h) +
  geom_point(aes(x=receipt_dttm, y=sell, col=groupinfection)) +
  geom_smooth(aes(x=receipt_dttm, y=sell), method ="loess") +
  labs(
    title = "건강식품 판매건수",
    caption = "건강식품, 인삼제품, 홍삼제품, 기타건강식"
  )
#데이터 시각화

rt = rest %>%
  group_by(receipt_dttm) %>%
  summarise(
    sell = sum(selng_cascnt),
    med_salamt = median(salamt)
  ) %>%
  mutate(groupinfection = ifelse(receipt_dttm >= "2020-05-07", T, F))
#이태원 집단감염 이후를 기준으로 분석
#데이터 정제
rtg = ggplot(rt) +
  geom_point(aes(x=receipt_dttm, y=sell, col=groupinfection)) +
  geom_smooth(aes(x=receipt_dttm, y=sell), method ="loess") +
  labs(
    title = "식당 판매건수",
    caption = "서양음식, 일반한식, 중국음식, 한정식, 일식회집, 갈비전문점"
  )
#데이터 시각화

mt = market %>%
  group_by(receipt_dttm) %>%
  summarise(
    sell = sum(selng_cascnt),
    med_salamt = median(salamt)
  ) %>%
  mutate(groupinfection = ifelse(receipt_dttm >= "2020-05-07", T, F))
#이태원 집단감염 이후를 기준으로 분석
#데이터 정제
mtg = ggplot(mt) +
  geom_point(aes(x=receipt_dttm, y=sell, col=groupinfection)) +
  geom_smooth(aes(x=receipt_dttm, y=sell), method ="loess") +
  labs(
    title = "식료품점 판매건수",
    caption = "슈퍼마켓, 제과점, 정육점, 편의점, 스넥, 농축수산가공품, 농축수산품"
  )
#데이터 시각화

dr = drink %>%
  group_by(receipt_dttm) %>%
  summarise(
    sell = sum(selng_cascnt),
    med_salamt = median(salamt)
  ) %>%
  mutate(groupinfection = ifelse(receipt_dttm >= "2020-05-07", T, F))
#이태원 집단감염 이후를 기준으로 분석
#데이터 정제
drg = ggplot(dr) +
  geom_point(aes(x=receipt_dttm, y=sell, col=groupinfection)) +
  geom_smooth(aes(x=receipt_dttm, y=sell), method ="loess") +
  labs(
    title = "주류점 판매건수",
    caption = "유흥주점, 주류판매점, 주점, 칵테일바, 단란주점"
  )
#데이터 시각화 

kc = kitc %>%
  group_by(receipt_dttm) %>%
  summarise(
    sell = sum(selng_cascnt),
    med_salamt = median(salamt)
  ) %>%
  mutate(groupinfection = ifelse(receipt_dttm >= "2020-05-07", T, F))
#이태원 집단감염 이후를 기준으로 분석
#데이터 정제
kcg = ggplot(kc) +
  geom_point(aes(x=receipt_dttm, y=sell, col=groupinfection)) +
  geom_smooth(aes(x=receipt_dttm, y=sell), method ="loess") +
  labs(
    title = "주방용품 판매건수",
    caption = "주방용식기, 주방용구, 기타주방용구"
  )
#데이터 시각화
grid.arrange(hg, rtg, ncol=1, nrow=2)
grid.arrange(mtg, drg, ncol=1, nrow=2)
kcg
#추출해야 할 시각 자료가 5개이므로 kcg는 따로 출력
```

```{r}
colnames(card) <- c("date", "adstrd_code", "adstrd_nm", "shop_code", "category_m", "salary_count", "salary_amount")
#변수명 지정

card$date <- ymd(card$date)
#일은 날짜형 데이터로 처리
card$category_m <- as.factor(card$category_m)
#카테고리는 팩터형 데이터로 처리
card <- card %>%
  filter(!str_detect(salary_count, regex("[가-힣]")))
#문자열 처리
card$salary_amount <- as.numeric(card$salary_amount)
card$salary_count <- as.numeric(card$salary_count)
#계산해야할 컬럼 수치화

card2 <- card %>% 
  separate(col = date, into = c("year", "month", "day"), sep = "-") %>%  
  # date 변수를 "-"를 기준으로 year, month, day로 나누고
  unite(year, month, col = "year_month", sep = "-") %>%  
  # year와 month를 다시 "-"로 결합해서 year_month라는 변수로 만듦. 
  mutate(category_l = str_sub(shop_code, start = 1, end = 1)) %>%  
  # 그리고 shop_code에서 첫번째 숫자만 뽑아서 category_l라는 변수 생성함.
  select(year_month:shop_code, category_l, category_m:salary_amount)  
# 변수 보기 좋게 재배열

card2$category_l[card2$category_l=="1"]='관광'
card2$category_l[card2$category_l=="2"]='취미'
card2$category_l[card2$category_l=="3"]='생활'
card2$category_l[card2$category_l=="4"]='쇼핑'
card2$category_l[card2$category_l=="5"]='교육'
card2$category_l[card2$category_l=="6"]='차량관련'
card2$category_l[card2$category_l=="7"]='의료'
card2$category_l[card2$category_l=="8"]='음식'
card2$category_l[card2$category_l=="9"]='기타'
#컬럼이름 설정

head(card2)
#정상출력 확인
```

```{r}
# 월별, 대분류 업종별 매출건수의 합과 평균, 매출금액의 합과 평균, 1월 대비 증감비율

month_total <- card2 %>%
  group_by(year_month, category_l) %>%
  summarize(count_sum = sum(salary_count), count_mean = mean(salary_count), count_median = median(salary_count),
            amount_sum = sum(salary_amount), amount_mean = mean(salary_amount), amount_median = median(salary_amount))
# month_total : 대분류 업종별 매출건수 합계, 평균, 중앙값, 매출금 합계, 평균

get_ratio <- function(var){
  per <- length(var) / 6
  out <- c()
  for(i in 0:5){
    out <- c(out, var[(1+i*per):(per+i*per)] / var[1:per])
  }
  return(out)
}
#카테고리별로 연산을 위해 function을 사용해 자동 분산 함수 만듬.

ratio <- ungroup(month_total) %>%
  mutate_at(vars(count_sum:amount_median), get_ratio) %>%
  rename(count_sum_ratio = count_sum, count_mean_ratio = count_mean, count_median_ratio = count_median,
         amount_sum_ratio = amount_sum, amount_mean_ratio = amount_mean, amount_median_ratio = amount_median)
month_total_ratio <- month_total %>%
  inner_join(ratio, by = c("year_month", "category_l"))
#정제한 데이터에서 연월, 카테고리 별로 분류하여 추출

options(repr.plot.width=10, repr.plot.height=6)
ggplot(month_total_ratio, aes(x = year_month, y = amount_median_ratio, group = category_l, color = category_l, shape = category_l)) +
  geom_point(size = 4) +
  geom_line(size = 1.5) +
  scale_shape_manual(values = 10:18) +
  ggtitle("1월 대비 월 중앙값 매출금액의 변화") +
  xlab("월") +
  ylab("매출 비율") +
  labs(color = "대분류", shape = "대분류")+
  theme(plot.title = element_text(size = 20))
#그래프를 통해 관광, 쇼핑 등 대면을 필요로 하는 항목들이 차례로 기하급수적으로 감소하는 모습을 보였다.
```

```{r}
# 관광 소분류(14종)
card2$category_m<-as.character(card2$category_m)
category_1<-subset(card2,card2$category_l=="관광")
#관광 산업만 알아보기 위해 관광 카테고리만 추출
unique(category_1$category_m)
#중복행 삭제


travel_month_total <- card2 %>%
  filter(category_l == "관광") %>%
  group_by(year_month, category_m) %>%
  summarize(count_sum = sum(salary_count), count_mean = mean(salary_count), count_median = median(salary_count),
            amount_sum = sum(salary_amount), amount_mean = mean(salary_amount), amount_median = median(salary_amount))
travel_month_total <- travel_month_total %>%
  # travel_month_total : 여행 관련 산업 월, 업종별 매출 합계, 평균, 중앙값, 매출금 합계, 평균
  filter(category_m != "택시회사")
travel_ratio <- ungroup(travel_month_total) %>%
  mutate_at(vars(count_sum:amount_median), get_ratio) %>%
  rename(count_sum_ratio = count_sum, count_mean_ratio = count_mean, count_median_ratio = count_median,
         amount_sum_ratio = amount_sum, amount_mean_ratio = amount_mean, amount_median_ratio = amount_median)
# travel_month_m_ratio : travel_month_total에 1월 대비 증감비율

travel_month_m_ratio <- travel_month_total %>%
  inner_join(travel_ratio, by = c("year_month", "category_m"))
#매출금액의 중앙값 1월 대비 증감비율이 가장 낮은 3개의 업종

x <- travel_month_m_ratio %>%
  group_by(year_month) %>%
  top_n(n = -3, amount_median_ratio)

options(repr.plot.width=8, repr.plot.height=5)
ggplot(filter(x, year_month != "2020-01"), aes(x = year_month, y = amount_median_ratio, fill = category_m)) +
  geom_col(position = "dodge") +
  ylim(c(0, 1)) +
  ggtitle("1월 대비 매출금액의 비율 감소한 하위 3개 업종") +
  xlab("월") +
  ylab("매출금액의 중앙값 비율") +
  labs(fill = "중분류")+
  theme(plot.title = element_text(size = 15))
#1월 이후로 여객선, 관광여행 등 소비자들이 점차 여행을 기피하는 성향을 보이기 시작하면서 항공사 역시 매출금이 점차 감소했음을 확인할 수 있었다.
```