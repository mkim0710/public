




allsubstr=function(x){
  res=c()
  for(i in 1:nchar(x))  res=c(res,unique(substring(x, i, i:nchar(x))))
  res
}

allsubstr("나는 행복합니다.") 
# > allsubstr("나는 행복합니다.")
# [1] "나"               "나는"             "나는 "            "나는 행"          "나는 행복"        "나는 행복합"      "나는 행복합니"   
# [8] "나는 행복합니다"  "나는 행복합니다." "는"               "는 "              "는 행"            "는 행복"          "는 행복합"       
# [15] "는 행복합니"      "는 행복합니다"    "는 행복합니다."   " "                " 행"              " 행복"            " 행복합"         
# [22] " 행복합니"        " 행복합니다"      " 행복합니다."     "행"               "행복"             "행복합"           "행복합니"        
# [29] "행복합니다"       "행복합니다."      "복"               "복합"             "복합니"           "복합니다"         "복합니다."       
# [36] "합"               "합니"             "합니다"           "합니다."          "니"               "니다"             "니다."           
# [43] "다"               "다."              "."               


x = "나는 행복합니다."
str(x)
# > str(x)
# chr "나는 행복합니다."
# list(x)
# as.list(x)
nchar(x)
# > nchar(x)
# [1] 9




myLCS_sub=function(a,b){
  result=intersect(allsubstr(a),allsubstr(b))
  if(length(result)==0) return(NULL)
  result[which.max(nchar(result))]         
}

myLCS_sub("나는 행복합니다.","너도 행복하니?")

a = "나는 행복합니다."
b = "너도 행복하니?"

# a[1]
length(a)
nchar(a)
# > length(a)
# [1] 1
# > nchar(a)
# [1] 9
substring(a, 1, 1)




#@ ------
myLCS=function(x){
  Reduce(myLCS_sub,x)
}

x=c("나는 행복합니다.","너도 행복하니?","행복한 세상")
myLCS(x)
# > myLCS(x)
# [1] "행복"


allsubstr1=function(x){
  unlist(sapply(seq_len(nchar(x)), function(i) unique(substring(x, i, i:nchar(x)))))
  
}
myLCS_sub1=function(a,b){
  result=intersect(allsubstr1(a),allsubstr1(b))
  if(length(result)==0) return(NULL)
  result[which.max(nchar(result))]         
}
myLCS1=function(x){
  Reduce(myLCS_sub1,x)
}
myLCS1(x)

x=paste0(1:10000,"행복",10000:1)
length(x)
str(x)
# > length(x)
# [1] 10000
# > str(x)
# chr [1:10000] "1행복10000" "2행복9999" "3행복9998" "4행복9997" "5행복9996" "6행복9995" "7행복9994" "8행복9993" ...

system.time(myLCS(x))
system.time(myLCS1(x))
# > system.time(myLCS(x))
# user  system elapsed 
# 2.376   0.014   2.399 
# > system.time(myLCS1(x))
# user  system elapsed 
# 2.761   0.013   2.780 

allsubstr2=function(x,max=NULL){
  res=c()
  if(is.null(max)) max=nchar(x)
  for(i in 1:max)  res=c(res,unique(substring(x, 1:(nchar(x)-i+1),i:nchar(x))))
  res
}

allsubstr2("행복한 세상")
allsubstr2("행복한 세상",2)
# > allsubstr2("행복한 세상")
# [1] "행"          "복"          "한"          " "           "세"          "상"          "행복"        "복한"        "한 "        
# [10] " 세"         "세상"        "행복한"      "복한 "       "한 세"       " 세상"       "행복한 "     "복한 세"     "한 세상"    
# [19] "행복한 세"   "복한 세상"   "행복한 세상"
# > allsubstr2("행복한 세상",2)
# [1] "행"   "복"   "한"   " "    "세"   "상"   "행복" "복한" "한 "  " 세"  "세상"

myLCS_sub2=function(a,b){
  n=min(nchar(a),nchar(b))
  result=intersect(allsubstr2(a,n),allsubstr2(b,n))
  if(length(result)==0) return(NULL)
  result[which.max(nchar(result))]         
}
myLCS2=function(x){
  Reduce(myLCS_sub2,x)
}
system.time(myLCS(x))
system.time(myLCS2(x))
# > system.time(myLCS(x))
# user  system elapsed 
# 2.373   0.035   2.414 
# > system.time(myLCS2(x))
# user  system elapsed 
# 1.011   0.008   1.023 



#@ mh -----
mhLCS_sub=function(a,b){
  if (nchar(a) < nchar(b)) {
    tmp = a
    a = b
    b = tmp
  }
  a.vec = unlist(strsplit(a,""))
  b.vec = unlist(strsplit(b,""))
  res = c()
  for (i in 1:length(a.vec)) {
    for (j in which(b.vec == a.vec[i])) {
      k = kk = 0
      while (kk == 0) {
        kk = 1
        if ( (i+k+1) <= length(a.vec) ) {
          if ( (j+k+1) <= length(b.vec) ) {
            if (identical(a.vec[i:(i+k+1)], b.vec[j:(j+k+1)])) {
              k = k + 1
              kk = 0
            }
          }
        }
      }
      res = c(res, Reduce(paste0, a.vec[i:(i+k)]))
    }
  }
  res[which.max(nchar(res))]  
}
mhLCS_sub("나는 행복합니다.","너도 행복하니?")

mhLCS=function(x){
  Reduce(mhLCS_sub,x)
}

mhLCS(x)
# > mhLCS(x)
# [1] "행복"

system.time(myLCS(x))
system.time(myLCS2(x))
system.time(mhLCS(x))
# > system.time(myLCS(x))
# user  system elapsed 
# 2.223   0.024   2.252 
# > system.time(myLCS2(x))
# user  system elapsed 
# 0.966   0.004   0.971 
# > system.time(mhLCS(x))
# user  system elapsed 
# 0.712   0.009   0.722 


#@ end -----

