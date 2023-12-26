main=read.csv('./data/step0_apart_main_2022.csv', encoding = 'UTF-8')
station=read.csv('./data/subway.csv', header = T, colClasses = c('character','character','character'))

################
# 패키지 임포트
################

library(XML)
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(geosphere)
library(caret)
library(gridExtra)

##############
# 전처리
##############

mapping=c("30230"="대덕구",
          "30110"="동구",
          "30170"="서구",
          "30200"="유성구",
          "30140"="중구")

# 거래금액 수치화/ 불필요 공백 제거
main$거래금액=as.numeric(gsub("[ ,]", '', final$거래금액)) 
main$법정동=gsub("^\\s+",'',main$법정동)

# 시군구코드 -> 시군구 매핑 / 해제여부 'O' 아닌경우 필터링 / 중복 행 제거 (불필요한 중개사소재지 제거)
main=main %>% 
    mutate(시군구=as.character(mapping[as.character(법정동시군구코드)])) %>% 
    filter(해제여부!='O') %>% 
    dplyr::select(-중개사소재지) %>% 
    distinct()

# 열선택
data=main %>% 
    mutate(지번주소=paste('대전시', 시군구, 법정동, 지번)) %>% 
    mutate(도로명주소=paste(시군구,도로명, 도로명건물본번호코드)) %>% 
    dplyr::select(거래금액, 거래유형 ,건축년도, 전용면적, 층,
           거래유형, 시군구, 도로명주소, 지번주소 ,아파트, 월, 일)



#######################
# 아파트 좌표 불러오기
#######################
coord=data.frame()
for (addr in data[,'지번주소']){
    result=addr_lon_lat_fun(addr)
    
    if (identical(result, character(0))){
        cat(addr,'\n')
        result=c(0,0)
    }
    
    coord=rbind(coord, addr_lon_lat_fun(addr))
}

colnames(coord)=c('경도','위도')
data=cbind(data,coord)


# 초중고 / 학원 / 병원 / 술집 / 역 / 아파트단지별 주차장 수

##########################
# 아파트 반경 1 km 내 학교
##########################
n_sch=tibble()
for (i in 1:nrow(data)){
    long=data[i,'경도']
    lat=data[i,'위도']
    # n=nrow(as.data.frame(category_search('SC4',long,lat,1000)))
    n=category_search('SC4',long,lat,1000)
    n_sch=rbind(n_sch,n)
}
colnames(n_sch)='학교'
data=cbind(data,n_sch)


##########################
# 아파트 반경 1 km 내 학원
##########################
n_aca=tibble()
for (i in 1:nrow(data)){
    long=data[i,'경도']
    lat=data[i,'위도']
    #n=nrow(as.data.frame(category_search('AC5',long,lat,1000)))
    n=category_search('AC5',long,lat,1000)
    n_aca=rbind(n_aca,n)
}
colnames(n_aca)='학원'
data=cbind(data,n_aca)


##########################
# 아파트 반경 1 km 내 병원
##########################
n_hos=tibble()
for (i in 1:nrow(data)){
    long=data[i,'경도']
    lat=data[i,'위도']
    #n=nrow(as.data.frame(category_search('HP8',long,lat,1000)))
    n=category_search('HP8',long,lat,1000)
    n_hos=rbind(n_hos,n)
}
colnames(n_hos)='병원'
data=cbind(data,n_hos)


##########################
# 아파트 반경 1 km 내 술집
##########################
n_hof=tibble()
for (i in 1:nrow(data)){
    long=data[i,'경도']
    lat=data[i,'위도']
    # n=nrow(as.data.frame(keyword_search('술집',long,lat,1000)))
    n=keyword_search('술집',long,lat,1000)
    n_hof=rbind(n_hof,n)
}
colnames(n_hof)='술집'
data=cbind(data,n_hof)

#############################
# 아파트 반경 1km 내 공공기관
#############################
n_pi=tibble()
for (i in 1:nrow(data)){
    long=data[i,'경도']
    lat=data[i,'위도']
    # n=nrow(as.data.frame(category_search('PO3',long,lat,1000)))
    n=category_search('PO3',long,lat,1000)
    n_pi=rbind(n_pi,n)
}
colnames(n_pi)='공공기관'
data=cbind(data,n_pi)

#############################
# 아파트 반경 1km 내 문화시설
#############################
n_ct=tibble()
for (i in 1:nrow(data)){
    long=data[i,'경도']
    lat=data[i,'위도']
    # n=nrow(as.data.frame(category_search('CT1',long,lat,1000)))
    n=category_search('CT1',long,lat,1000)
    n_ct=rbind(n_ct,n)
}
colnames(n_ct)='문화시설'
data=cbind(data,n_ct)

#############################
# 아파트 반경 1km 내 카페
#############################
n_cf=tibble()
for (i in 1:nrow(data)){
    long=data[i,'경도']
    lat=data[i,'위도']
    # n=nrow(as.data.frame(category_search('CE7',long,lat,1000)))
    n=category_search('CE7',long,lat,1000)
    n_cf=rbind(n_cf,n)
}
colnames(n_cf)='카페'
data=cbind(data,n_cf)


##########################
# 역까지의 최단 직선 거리
##########################

distance_sta=tibble()
for (i in 1:nrow(data)){
    long=data[i,'경도']
    lat=data[i,'위도']
    n=get_nearest(long, lat)
    distance_sta=rbind(distance_sta,n)
}


colnames(distance_sta)=c('가장 가까운 역', '역최단거리')
data=cbind(data, distance_sta)
data$역최단거리=as.numeric(data$역최단거리)


##########################
# 중복제거
##########################
data=distinct(data)

# step1_kakaoApi_2022(2023).csv
write.csv(data,"./data/step1_kakaoApi_2023.csv",row.names = F, fileEncoding = "UTF-8")

########################
# 아파트 정보 병합 -> preprocessed=data+aparts
########################

data$exclusiveArea=floor(data$전용면적)
colnames(aparts)[5]='지번주소'

preprocessed=left_join(data, aparts, by=c('지번주소','exclusiveArea')) %>% 
    distinct(거래금액, 거래유형, 건축년도, 전용면적, 층, 아파트, 월, 일, .keep_all = T) %>% 
    rename(세대수=totalHouseholdCount,
           최고층=highFloor,
           세대당주차대수=parkingCountByHousehold,
           난방방식=heatMethodTypeCode,
           현관구조=entranceType,
           방개수=roomCnt,
           욕실개수=bathroomCnt) %>% 
    select(거래금액, 시군구:아파트, 거래유형, 건축년도:층, 월:위도, 학교:카페, 역최단거리, '가장 가까운 역',
           세대수:최고층, 세대당주차대수:욕실개수)

# step2_crawling_2022(2023).csv
write.csv(preprocessed,"./data/step2_crawling_2023.csv",row.names = F, fileEncoding = "UTF-8")



########################
# 가변수화, 결측제거, 형변환
########################

dummy1 = dummyVars(" ~ 시군구+거래유형+난방방식+현관구조", data=preprocessed)
dummy1= data.frame(predict(dummy1, newdata=preprocessed))
preprocessed_dummy=cbind(preprocessed,dummy1) 

# 행에 결측이 하나라도 있으면 제거
preprocessed_dummy=na.omit(preprocessed_dummy)
# 주차대수가 0으로 기록안된 행 제거
preprocessed_dummy=preprocessed_dummy[-which(preprocessed_dummy$세대당주차대수==0),]

# 형변환
preprocessed_dummy$방개수=as.numeric(preprocessed_dummy$방개수)
preprocessed_dummy$욕실개수=as.numeric(preprocessed_dummy$욕실개수)

# step3_dummy_2022(2023).csv
write.csv(preprocessed_dummy,"./data/step3_dummy_2023.csv",row.names = F, fileEncoding = "UTF-8")

######################
# 분석용이하게 변수들만
######################
original=preprocessed_dummy %>% 
    filter(월<11, 월>8) %>% 
    select(-c('건폐율','용적률','주차대수'))

output=preprocessed_dummy %>% 
    select(-c('시군구','도로명주소','지번주소','아파트','일','거래유형','경도','위도','가장 가까운 역',
              '현관구조','난방방식','용적률','건폐율','주차대수')) %>% 
    filter(월<11, 월>8) %>% 
    select(-월)

write.csv(original,"./data.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(output,"./data_numeric.csv",row.names = F, fileEncoding = "UTF-8")














