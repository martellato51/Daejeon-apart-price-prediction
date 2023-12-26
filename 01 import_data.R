########################
# year_month 문자열 생성
########################
ym_generate=function(start, end){ 
    v=c()
    for (year in start:end){
        for (month in 1:12){
            ym=paste0(year,sprintf("%02d", month))
            v=c(v,ym)
        }
    }
    return(v) # 202201 (6자리 연-월 정보)
}

######################################
# 지역별, 기간별 실거래가 데이터 생성
######################################
api_url="http://openapi.molit.go.kr/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTradeDev"
api_key=""

generate_data=function(start, end, region_code){
    
    final_data=data.frame() # 데이터 저장될 메인 df
    
    for (contract_mon in ym_generate(start,end)){ 
        
        # 연-월별로 loop, 각 연-월별 가져와야 할 loop_count 받아오기
        url=paste0(api_url,
                   paste0("?serviceKey=",api_key),
                   paste0("&pageNo=",1),
                   paste0("&numOfRows=",50),
                   paste0("&LAWD_CD=",region_code),
                   paste0("&DEAL_YMD=",contract_mon)
        )
        # xml 파일을 xml_file에 읽어오고, 루트노드에 접근
        xml_file=xmlParse(url)
        root_node=xmlRoot(xml_file)
        # 1페이지 50행을 가져올때, total_count/50을 loopCount에 저장
        total_count=as.numeric(xpathSApply(root_node,"//totalCount",xmlValue))
        loop_count=ceiling(total_count/50)
        
        
        # 실제 데이터 불러오기
        for (i in 1:loop_count) {
            # 호출 URL 생성
            url=paste0(api_url,
                       paste0("?serviceKey=",api_key),
                       paste0("&pageNo=",i),
                       paste0("&numOfRows=",50),
                       paste0("&LAWD_CD=",region_code),
                       paste0("&DEAL_YMD=",contract_mon)
            )
            
            # XML 데이터를 읽어옴
            doc = xmlParse(url)
            
            # XML 데이터의 Root Node에 접근
            root_node = xmlRoot(doc)
            
            # item 노드의 서브노드를 가져와 데이터프레임으로 변환하여 저장
            xml_data = xmlToDataFrame(nodes = getNodeSet(root_node,'//item'))
            
            # 추출한 데이터를 final_data 누적 저장
            final_data = rbind(final_data, xml_data)
        }
        
    } 
    return(final_data)
}
####################
# 대전 소재 초등학교
####################
elementary_sch=function(){
    api_url="http://apis.data.go.kr/6300000/openapi2022/elSchInfo/getelSchInfo"
    api_key_e=''
    
    url=paste0(api_url,
               paste0("?serviceKey=",api_key_e),
               paste0("&pageNo=",1),
               paste0("&numOfRows=",1000))
    json_data = jsonlite::fromJSON(url)
    
    df = as.data.frame(json_data)
    return(df)
}

######################
# 대전 소재 중고등학교
######################
mid_high_sch=function(){
    api_url="http://apis.data.go.kr/6300000/openapi2022/midHighSchInfo/getmidHighSchInfo"
    api_key_h=''
    
    url=paste0(api_url,
               paste0("?serviceKey=",api_key_h),
               paste0("&gu=A"),
               paste0("&pageNo=",1),
               paste0("&numOfRows=",1000))
    json_data = jsonlite::fromJSON(url)
    
    df = as.data.frame(json_data)
    return(df)
}

###############
# 지하철역 정보
###############
subway=data.frame()
station_list=c('반석역','지족역','노은역','월드컵경기장역','현충원역','구암역','유성온천역','갑천역',
               '월평역','갈마역','정부청사역','대전 시청역','탄방역','용문역','오룡역','서대전역','서대전네거리역',
               '중구청역','중앙로역','대전역','대동역','신흥역','판암역','유성시외버스정류소','대전복합터미널')
for (i in 1:NROW(station_list)){
    subway=rbind(subway,
                 c(station_list[i], addr_lon_lat_fun(station_list[i])))
}
colnames(subway)=c('역이름','경도','위도')



############
# 함수 실행
############
# 지역구 (대덕구 : 30230,동구 : 30110,서구 : 30170, 유성구 : 30200, 중구 : 30140) 

df1=generate_data(2023, 2023, 30230) # 대덕구 
df2=generate_data(2023, 2023, 30110) # 동구
df3=generate_data(2023, 2023, 30170) # 서구
df4=generate_data(2023, 2023, 30200) # 유성구
df5=generate_data(2023, 2023, 30140) # 중구

final=rbind(df1,df2,df3,df4,df5)
e_sch=elementary_sch()
mh_sch=mid_high_sch()

write.csv(final,"./data/step0_apart_main_2023.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(e_sch,"./data/ele_sch.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(mh_sch,"./data/mid_hi_sch.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(subway,"./data/subway.csv",row.names = F, fileEncoding = 'UTF-8')


