#相似离度 Degree of Analogue Deviation
#a,b:等长数值向量
#值系数D比重beta，形系数S比重1-beta
degAD2<-function(a,b,beta=0.5){
    x=BBmisc::normalize(a)-BBmisc::normalize(b)
    E=mean(x);
    D=mean(abs(x));S=mean(abs(x-E));
    return(beta*D+(1-beta)*S)
}