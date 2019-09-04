############地理加权回归 数据格式说明######
# Sampledf.sp和Testdf.sp：SpatialPointsDataFrame对象，Testdf.sp为待检验点数据，
#  其 @data有n+5列元素，前5列为'name','iii','lat','lon','hh'，
# 其后为气象要素值，顺序两者一一对应
# approachX:评分准则，核函数带宽估算用。 
# kernelX：核函数
#返回值：成员为var.pred和var.diff、degAD.df的列表对象，
#第1个为n个成员的var的拟合值的数据框，
#第2个为实测和拟合值的差，第3个为相似离度
##################
gwr.result<-function(Testdf.sp, Sampledf.sp, n, approachX="AICc", kernelX="gaussian",beta=0.5){
    var.pred<-NULL 
    degAD.df<-NULL 
    for(i in 1:n){
        gwrdfi<-Sampledf.sp[,c(i+5,3:5)] 
        dfi.formula<-DF2formula(gwrdfi@data)
        #核函数gaussian，评分准则AICc，估算带宽dif.bw
        dfi.bw<-bw.gwr(dfi.formula,data=gwrdfi,approach=approachX,
           kernel=kernelX,adaptive=T)
        #地理加权回归
        dfi.model<-gwr.predict(dfi.formula,data=gwrdfi,
           predictdata=Testdf.sp, bw=dfi.bw,
           kernel=kernelX,adaptive=T)
        var.pred<-cbind(var.pred, v.pred=dfi.model$SDF$prediction)
        degAD.df<-degAD2(Testdf.sp@data[,i+5], var.pred[,i], beta) %>%
            cbind(degAD.df, degAD=. )  
    }
    var.diff<-Testdf.sp@data[,6:(n+5)]-var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}

