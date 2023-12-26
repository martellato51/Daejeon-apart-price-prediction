###################
# 주소-> 좌표변환
###################
api_key_c=""


addr_lon_lat_fun = function(addr) {
    data_list =
        GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
            query = list(query = addr, analyze_type= 'similar'),
            add_headers(Authorization = paste0("KakaoAK ", api_key_c))) %>% 
        content(as = 'text') %>% 
        fromJSON()
    
    return(c(as.character(data_list$documents$x[1]),
             as.character(data_list$documents$y[1])))
    
    # return (data_list$documents)
}
#Test
addr_lon_lat_fun('대전시 서구 둔산동 908') # "127.393306371061" "36.3613116133279"



###################
# 키워드 검색
###################
keyword_search= function(addr,long,lat,rad, category=NULL ) {
    data_list =
        GET(url = 'https://dapi.kakao.com/v2/local/search/keyword.json',
            query = list(query = addr, category_group_code=category, x=long, y=lat, radius=rad),
            add_headers(Authorization = paste0("KakaoAK ", api_key_c))) %>% 
        content(as = 'text') %>% 
        fromJSON()
    return (data_list$documents[c(1,4,7,8)]) # 주소, 카테고리, 전화번호, 이름 
    #return (data_list$meta[['total_count']]) # 총개수 반환
}


###################
# 카테고리 검색
###################
category_search= function(category,long,lat,rad) {
    data_list =
        GET(url = 'https://dapi.kakao.com/v2/local/search/category.json',
            query = list(category_group_code = category, x=long, y=lat, radius=rad),
            add_headers(Authorization = paste0("KakaoAK ", api_key_c))) %>% 
        content(as = 'text') %>% 
        fromJSON()
    
    return (data_list$documents[c(1,2,7,8)]) # 주소, 카테고리, 전화번호, 이름 
    #return (data_list$meta[['total_count']]) # 총개수 반환
}

#Test
# 한빛 아파트
category_search('CT1',127.353659081769, 36.3635250963285,1000) %>% View()
keyword_search('술집',127.353659081769, 36.3635250963285,1000) %>% View() 

##########################
# 역까지의 최단 직선 거리
##########################
get_nearest=function(long, lat){
    distances=distGeo(p1=c(long, lat),
                      p2=station[,c('경도','위도')])
    
    nearest_index=which.min(distances)
    nearest=station[nearest_index,1]
    
    return(c(nearest, round(min(distances))))
}

# 역까지의 최단 직선 거리
get_nearest("127.353659081769", "36.3635250963285")


