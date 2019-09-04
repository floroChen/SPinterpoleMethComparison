############多元回归 数据格式说明######
# df和Test.df：DataFrame对象，Test.df为待检验点数据，
# dataframe有n+5列元素，前5列为'name','iii','lat','lon','hh'，
# 其后为气象要素值，顺序两者一一对应
#返回值：成员为var.lm, var.pred和var.diff、degAD.df的列表对象，
#第1,2个为n个成员的var的回归模型列表和拟合值的数据框，
#第三个为实测和拟合值的差，第四个为相似离度
##################
lm.result<-function(Test.df, df, n, beta=0.5){
    var.lm<-NULL
    var.pred<-NULL 
    degAD.df<-NULL 
    for(i in 1:n){
        var.lm<-df[,c(i+5,3:5)] %>% 
            lm(DF2formula(.),data=.) %>%   #var(i)~lat+lon+hh
            c(var.lm, list(.))  
        var.lm<-var.lm[-(1:12)]  #删除list(.)产生的模型其他信息
        var.pred<-predict(var.lm[[i]], Test.df) %>% 
            cbind(var.pred, v.pred= . )
        degAD.df<-degAD2(Test.df[,i+5], var.pred[,i], beta) %>%
            cbind(degAD.df, degAD=. )  
    }
    var.diff<-Test.df[,6:(n+5)]-var.pred
    return(list(var.lm=var.lm, var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}
