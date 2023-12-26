#######################
# 구 별 코드
#######################
gu=data.frame(gu=c('대덕구','동구','서구','유성구','중구'),
                   code=c(3023000000, 3011000000, 3017000000, 3020000000, 3014000000 ))

########################
# 구 별 동 코드 
########################
get_dong_code=function(gu_code){
    data_list =
        GET(url = paste0('https://new.land.naver.com/api/regions/list?cortarNo=', gu_code)) %>% 
        content(as = 'text') %>% 
        fromJSON() %>% 
        as.data.frame() %>% 
        select(regionList.cortarNo, regionList.cortarName)
    
    return(data_list)
} 

########################
# 동 별 아파트 코드
########################
get_apart_code=function(dong_code){
    data_list =
        GET(url = paste0('https://new.land.naver.com/api/regions/complexes?cortarNo=',dong_code,'&realEstateType=APT%3AABYG%3AJGC%3APRE&order=')) %>% 
        content(as = 'text') %>% 
        fromJSON() %>% 
        as.data.frame() %>% 
        select(complexList.cortarAddress, complexList.complexNo, complexList.complexName)
    return(data_list)
}

#######################
# 아파트 세부정보
#######################
get_apart_detail=function(apart_code){
    data_list =
        GET(url = paste0('https://new.land.naver.com/api/complexes/',apart_code,'?sameAddressGroup=false'),
            add_headers('Authorization'='Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6IlJFQUxFU1RBVEUiLCJpYXQiOjE2OTk2Njc3NjksImV4cCI6MTY5OTY3ODU2OX0.OLcNsxW_xk-IYLtQvARnLIejzhyk5xKEqz-VAKBacZw',
                        'Accept-Encoding'='gzip',
                        'Host'='new.land.naver.com',
                        'Referer'='https://new.land.naver.com/complexes/106692?ms=36.3732335,127.3412276,17&a=APT:ABYG:JGC:PRE&e=RETAIL',
                        'Sec-Fetch-Dest'='empty',
                        'Sec-Fetch-Mode'='cors',
                        'Sec-Fetch-Site'='same-origin',
                        'User-Agent'='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36')) %>% 
        content(as = 'text') %>% 
        fromJSON() 
    
    df1=data_list %>% 
        pluck('complexDetail') %>%  # nested data에서 complexDetail 항목 가져와 저장
        as.data.frame() %>% 
        select(-realEstateTypeCode)
    
    df2=data_list %>% 
        pluck('complexPyeongDetailList') %>% # nested data에서 complexPyeongDetailList 항목 가져와 저장
        as.data.frame() %>% 
        select(exclusiveArea,entranceType,roomCnt, bathroomCnt)
    
    df1=bind_cols(df1,df2)

    
    
    return(df1)
}

########################
# 대전시 모든 아파트코드
########################
# 대전시 구-동 별 아파트 코드 불러오기
code=data.frame()
for (i in 1:5){ # 구
    dong=get_dong_code(gu[i,2])
    apart=data.frame()
    
    for (i in 1:nrow(dong)){ # 동
        try(eval(parse(text='apart=rbind(apart,get_apart_code(dong[i,1]))')), silent = T) # 특정 동에 아파트 존재하지않는다면 스킵.
        
    }
    code=rbind(code,apart)
}

########################
# 아파트 정보 불러오기
########################
apart_detail=data.frame()
for (i in 1:nrow(code)){
    apart_detail=bind_rows(apart_detail, get_apart_detail(code[i,1])) # bind_rows, 열 자체 결측시, NA
}

heatMethod=c("HT001"="개별난방",
             "HT002"="중앙난방",
             "HT005"="지역난방")


aparts=apart_detail%>%
    mutate(roadAddress=paste(roadAddressPrefix, roadAddress), # 주소 전처리
           address=paste(address, detailAddress)) %>% 
    
    select(complexNo,complexName, realEstateTypeName, roadAddress, address, # 열선택
           totalHouseholdCount,highFloor,parkingPossibleCount,parkingCountByHousehold,
           heatMethodTypeCode,exclusiveArea,entranceType,
           roomCnt, bathroomCnt) %>% 
    
    mutate(roadAddress=sub('대전시 ','',roadAddress)) %>%  # 도로명주소 대전시 추가
    mutate(heatMethodTypeCode=heatMethod[heatMethodTypeCode]) %>% # 난방방식 매핑
    mutate(exclusiveArea=floor(as.numeric(exclusiveArea)))


# 대전시 내 모든 아파트 정보 
write.csv(aparts, './data/apart_detail.csv', fileEncoding = 'UTF-8') 




