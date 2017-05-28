




allsubstr=function(x){
  res=c()
  for(i in 1:nchar(x))  res=c(res,unique(substring(x, i, i:nchar(x))))
  res
}

allsubstr("���� �ູ�մϴ�.") 
# > allsubstr("���� �ູ�մϴ�.")
# [1] "��"               "����"             "���� "            "���� ��"          "���� �ູ"        "���� �ູ��"      "���� �ູ�մ�"   
# [8] "���� �ູ�մϴ�"  "���� �ູ�մϴ�." "��"               "�� "              "�� ��"            "�� �ູ"          "�� �ູ��"       
# [15] "�� �ູ�մ�"      "�� �ູ�մϴ�"    "�� �ູ�մϴ�."   " "                " ��"              " �ູ"            " �ູ��"         
# [22] " �ູ�մ�"        " �ູ�մϴ�"      " �ູ�մϴ�."     "��"               "�ູ"             "�ູ��"           "�ູ�մ�"        
# [29] "�ູ�մϴ�"       "�ູ�մϴ�."      "��"               "����"             "���մ�"           "���մϴ�"         "���մϴ�."       
# [36] "��"               "�մ�"             "�մϴ�"           "�մϴ�."          "��"               "�ϴ�"             "�ϴ�."           
# [43] "��"               "��."              "."               


x = "���� �ູ�մϴ�."
str(x)
# > str(x)
# chr "���� �ູ�մϴ�."
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

myLCS_sub("���� �ູ�մϴ�.","�ʵ� �ູ�ϴ�?")

a = "���� �ູ�մϴ�."
b = "�ʵ� �ູ�ϴ�?"

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

x=c("���� �ູ�մϴ�.","�ʵ� �ູ�ϴ�?","�ູ�� ����")
myLCS(x)
# > myLCS(x)
# [1] "�ູ"


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

x=paste0(1:10000,"�ູ",10000:1)
length(x)
str(x)
# > length(x)
# [1] 10000
# > str(x)
# chr [1:10000] "1�ູ10000" "2�ູ9999" "3�ູ9998" "4�ູ9997" "5�ູ9996" "6�ູ9995" "7�ູ9994" "8�ູ9993" ...

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

allsubstr2("�ູ�� ����")
allsubstr2("�ູ�� ����",2)
# > allsubstr2("�ູ�� ����")
# [1] "��"          "��"          "��"          " "           "��"          "��"          "�ູ"        "����"        "�� "        
# [10] " ��"         "����"        "�ູ��"      "���� "       "�� ��"       " ����"       "�ູ�� "     "���� ��"     "�� ����"    
# [19] "�ູ�� ��"   "���� ����"   "�ູ�� ����"
# > allsubstr2("�ູ�� ����",2)
# [1] "��"   "��"   "��"   " "    "��"   "��"   "�ູ" "����" "�� "  " ��"  "����"

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
mhLCS_sub("���� �ູ�մϴ�.","�ʵ� �ູ�ϴ�?")

mhLCS=function(x){
  Reduce(mhLCS_sub,x)
}

mhLCS(x)
# > mhLCS(x)
# [1] "�ູ"

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
