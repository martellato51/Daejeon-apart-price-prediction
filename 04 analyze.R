##########################
# import
##########################
library(regbook)

df=read.csv('data.csv',header = T, encoding = 'UTF-8')
df_numeric=read.csv('data_numeric.csv',header = T, encoding = 'UTF-8')

##########################
# 시각화
##########################

# 범주형 변수
sigungu=df %>% 
    count(시군구) %>% 
    ggplot(aes(x=reorder(시군구,+n),y=n))+
    geom_col(width = 0.6, fill='orange') +
    geom_text(aes(x = 시군구, y = n, label = n), vjust=-0.7, size=5)+
    scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 850))+
    labs(title='지역별 거래')+
    theme_classic()+
    theme(plot.title=element_text(size=25, hjust=0.5, face='bold'),
          axis.text.x=element_text(size=18, face='bold'),
          axis.text.y=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
    
tsac=df %>% 
    count(거래유형) %>% 
    ggplot(aes(x=reorder(거래유형,+n),y=n))+
    geom_col(width = 0.6, fill='blue') +
    geom_text(aes(x = 거래유형, y = n, label = n), vjust=-0.7, size=5)+
    scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 2100))+
    labs(title='거래유형')+
    theme_classic()+
    theme(plot.title=element_text(size=25, hjust=0.5, face='bold'),
          axis.text.x=element_text(size=18, face='bold'),
          axis.text.y=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

heat=df %>% 
    count(난방방식) %>% 
    ggplot(aes(x=reorder(난방방식,+n),y=n))+
    geom_col(width = 0.6, fill='red') +
    geom_text(aes(x = 난방방식, y = n, label = n), vjust=-0.7, size=5)+
    scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 1400))+
    labs(title='난방방식')+
    theme_classic()+
    theme(plot.title=element_text(size=25, hjust=0.5, face='bold'),
          axis.text.x=element_text(size=18, face='bold'),
          axis.text.y=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

etype=df %>% 
    count(현관구조) %>% 
    ggplot(aes(x=reorder(현관구조,+n),y=n))+
    geom_col(width = 0.6, fill='green') +
    geom_text(aes(x = 현관구조, y = n, label = n), vjust=-0.7, size=5)+
    scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 1900))+
    labs(title='현관구조')+
    theme_classic()+
    theme(plot.title=element_text(size=25, hjust=0.5, face='bold'),
          axis.text.x=element_text(size=18, face='bold'),
          axis.text.y=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

x11()
grid.arrange(sigungu, tsac, heat, etype, ncol=4)

# 연속형 변수
tidy_apart=df_numeric %>% 
    select(거래금액:세대당주차대수) %>% 
    pivot_longer(거래금액:세대당주차대수,
                 names_to = '시설',
                 values_to = '개수')
# 욕실개수 방개수
x11()
ggplot(tidy_apart, aes(x = 개수, fill = 시설)) +
    geom_histogram(color = "black") +
    facet_wrap(~시설, scales = "free") +
    theme_minimal()+
    theme(plot.title=element_text(size=30, hjust=0.5, face='bold'))

room=df %>% 
    count(방개수) %>% 
    ggplot(aes(x=방개수,y=n))+
    geom_col(width = 0.6) +
    geom_text(aes(x = 방개수, y = n, label = n), vjust=-0.7, size=5)+
    scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 1900))+
    labs(title='방개수')+
    theme_classic()+
    theme(plot.title=element_text(size=25, hjust=0.5, face='bold'),
          axis.text.x=element_text(size=18, face='bold'),
          axis.text.y=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

bath=df %>% 
    count(욕실개수) %>% 
    ggplot(aes(x=욕실개수,y=n))+
    geom_col() +
    geom_text(aes(x = 욕실개수, y = n, label = n), vjust=-0.7, size=5)+
    scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 1900))+
    labs(title='욕실개수')+
    theme_classic()+
    theme(plot.title=element_text(size=25, hjust=0.5, face='bold'),
          axis.text.x=element_text(size=18, face='bold'),
          axis.text.y=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

grid.arrange(room,bath, ncol=2)

# 시군구별 거래금액
grp_region=df %>% 
    group_by(시군구) %>% 
    summarise(mean_price=mean(거래금액)) %>% 
    ggplot(aes(x=시군구, y=mean_price))+
    geom_col()


##########################
# 분석
##########################
# 0. 더미변수 (서구, 유성구, 직거래, 개별난방, 계단식)
df_numeric=df_numeric %>% 
    mutate(건축년도=2023-건축년도) %>% 
    rename(연식=건축년도) %>% 
    dplyr::select(-c(시군구대덕구,시군구중구,시군구동구,
                     거래유형직거래,
                     난방방식중앙난방,
                     현관구조복도식,현관구조복합식))


# 1. 층화 샘플링
set.seed(1234)
library(sampling)
size=df %>% 
    count(시군구) %>% 
    pull()*0.8 
train_idx=strata(df_numeric,stratanames = c('시군구'), size=round(size))
train=getdata(df_numeric, train_idx) %>% 
    dplyr::select(-c(시군구:Stratum))
test=df_numeric[-as.vector(train_idx$ID_unit),]


# 2. boxcox 변환 -> 로그변환
library(MASS)
first_model=lm(거래금액~ ., train)
bc=boxcox(first_model)
bc

# par(mfrow=c(2,1))
# hist(train$거래금액,freq = F)
# lines(density(train$거래금액))
# plot(first_model,2)

lambda=bc$x[which.max(bc$y)]# 0.1010101
train$거래금액=(train$거래금액^lambda-1)/lambda
test$거래금액=(test$거래금액^lambda-1)/lambda

train$거래금액=log(train$거래금액)
test$거래금액=log(test$거래금액)


# 3. 표준화(standardization)
train_stand=as.data.frame(scale(train %>% dplyr::select(-c(시군구서구:현관구조계단식)))) %>% 
    cbind(train %>% dplyr::select(시군구서구:현관구조계단식))
test_stand=as.data.frame(scale(test %>% dplyr::select(-c(시군구서구:현관구조계단식)))) %>% 
    cbind(test %>% dplyr::select(시군구서구:현관구조계단식))


# 3. 정규화(normalization)
# min_max_scaling=function(x) {
#     (x - min(x)) / (max(x) - min(x))
# }
# train_minmax=as.data.frame(lapply(train, min_max_scaling))
# test_minmax=as.data.frame(lapply(test, min_max_scaling))


# 4. 상관계수 행렬 
library(corrplot)
x11()
corrplot(cor(train_stand),
         method="square",
         diag=F,
         addCoef.col="black",
         number.font = 0.5,
         number.digits = 1)


# 5. 예비모형 -> 회귀계수 0에 가까운것, 유의하지 않은 것
model=lm(거래금액~., train) # full model
summary(model) # R-adj 0.8701

# 4,5번 제거후보
# 개별 t검정시 유의하지 않은 것 : 중개거래, 계단식, 층, 카페, 병원
# 회귀계수 0에 가까운 것 : 학교, 공공기관, 최고층, 층, 카페, 병원, 중개거래, 계단식
# 상관관계 플롯 : 


# 6. 잔차분석 
x11()
#par(mfrow=c(1,1))
par(mfrow=c(2,2))
plot(model)

# 7. 영향력관측치
df[c(390, 392, 1246, 1861, 1965),] %>% View
im=influence.measures(model)$infmat %>% 
    as.data.frame()
# 지렛값
which(im$hat>2*23/1584)
# dffit
which(im$dffit>2/sqrt(23/1584))
# 쿡의거리
which(im$cook.d>qf(0.5,23,1561))
plot(model, which = 4)


# 8. 다중공선성 -> 앞서 유의하지않거나, 공선성 의심되는 변수 제거후 vif 값확인.
library(regbook)
R=cor(train_stand)
diag(solve(R))
eigen(R)$values

summary(vif(model)) # 카페 vif 12


# 공선성 의심 제거후 사용된 변수 확인
# (학교, 공공기관, 최고층, 층, 카페, 병원, 중개거래, 계단식) 제거

model1=lm(거래금액~연식+전용면적+학원+술집+문화시설+역최단거리+
              세대수+세대당주차대수+욕실개수+방개수+시군구서구+시군구유성구+
              난방방식개별난방,train) # 임의로 제거

summary(vif(model1)) # vif 10이상 없음.
summary(model1) # 0.8635




# 9. 변수선택 (모든가능한 회귀, stepwise, backward, forward)

# 모든가능한 회귀 : 
# BIC 기준 : 병원, 중개거래, 계단식 제거
# Cp 기준 : 병원, 카페, 중개거래, 계단식 제거
all_result=regsubsets(거래금액~., train_stand, nbest = 1, nvmax = 21)
summaryf(all_result)



# stepwise : 중개거래만 뺌
step_result=step(model,
            scope = ~연식+전용면적+층+학교+학원+병원+술집+공공기관+문화시설+카페+
                역최단거리+세대수+최고층+세대당주차대수+방개수+욕실개수+
                시군구서구+시군구유성구+거래유형중개거래+난방방식개별난방+현관구조계단식, direction = 'both')
summary(step_result)
summary(model) # R-adj 0.8701



# forward : 중개거래만 뺌
model0=lm(거래금액~1, train_stand) # 상수항만 있는 모형
fw_result=step(model0, scope = ~연식+전용면적+층+학교+학원+병원+술집+공공기관+문화시설+카페+
                   역최단거리+세대수+최고층+세대당주차대수+방개수+욕실개수+
                   시군구서구+시군구유성구+거래유형중개거래+난방방식개별난방+현관구조계단식, direction = 'forward')
summary(fw_result)


# backward : 중개거래만 뺌
bw_result=step(model, direction = 'backward')
summary(bw_result)




# 11. 예측성능 평가
# 학습 세트에 대한 loocv
press(step_result) # pred.r.squared=0.8660809

# 평가세트에 대한 예측성능
rmse=function(y1, y2) {
    sqrt(mean((y1 - y2) ^ 2))
}

mae=function(y1, y2) {
    mean(abs(y1 - y2))
}

rsquare=function(y1, y2) {
    sum((y1 - mean(y1)) * (y2 - mean(y2))) ^ 2 / (sum((y1 - mean(y1)) ^ 2) * sum((y2 - mean(y2)) ^ 2))
}

rsquare_adj=function(y1, y2, p) {
    n <- length(y1)
    rsq <- rsquare(y1, y2)
    1 - (n - 1) * (1 - rsq) / (n - p - 1)
}

# 
model2=lm(거래금액~연식+전용면적+층+학교+학원+술집+공공기관+문화시설+
              역최단거리+세대수+최고층+세대당주차대수+방개수+욕실개수+
              시군구서구+시군구유성구+난방방식개별난방,train_stand) # Cp기준 모형

model3=lm(거래금액~연식+전용면적+학원+층+술집+문화시설+역최단거리+
              세대수+세대당주차대수+욕실개수+방개수+시군구서구+시군구유성구+
              난방방식지역난방,train) # 임의로 제거 


predict_value=predict(model2, newdata = test)
real_value=test %>% dplyr::select(거래금액) %>% pull()


rmse(real_value,predict_value)
mae(real_value,predict_value)
rsquare(real_value,predict_value) # 0.869
rsquare_adj(real_value,predict_value,14) #0.864

exp(predict_value)
exp(test$거래금액) %>% View



# 12. 11월 데이터에 대한 성능 평가
data_11=read.csv('./data_november/data_11.csv', header = T, encoding = 'UTF-8')
data_11=data_11 %>% 
    mutate(건축년도=2023-건축년도) %>% 
    rename(연식=건축년도) %>%
    dplyr::select(-c('시군구','도로명주소','월','지번주소','아파트','일','거래유형','경도','위도','가장.가까운.역',
              '현관구조','난방방식')) %>% 
    dplyr::select(-c(시군구대덕구,시군구중구,시군구동구,
                     거래유형직거래,
                     난방방식중앙난방,난방방식지역난방,
                     현관구조복도식,현관구조복합식))
data_11$거래금액=log(data_11$거래금액)

predict_11=predict(model2, newdata = data_11)
real_11=data_11 %>% dplyr::select(거래금액) %>% pull()

rmse(real_11,predict_11)
mae(real_11,predict_11)
rsquare(real_11,predict_11)
rsquare_adj(real_11,predict_11,14)






