#相似离度 Degree of Analogue Deviation
#a,b:等长数值向量
degAD<-function(a,b){
    x=BBmisc::normalize(a)-BBmisc::normalize(b) #应用标准化数据
    E=mean(x);
    D=mean(abs(x)); S=mean(abs(x-E));  #D值系数 S形系数
    return((D+S)/2)
}

