############数据格式说明######
# df.sp和Test.spdf：Spatial*DataFrame对象，Test.spdf为待检验点数据，
# dataframe有n+5列元素，前5列为'name','iii','lat','lon','hh'，
# 其后为气象要素值，顺序两者一一对应
# k：取邻近的k个点
# beta：相似离度的值系数D比重beta，形系数S比重1-beta
#返回值：成员为var.pred和var.diff、degAD.df的列表对象，
#第一个为n个成员的var拟合值的数据框，第二个为实测-拟合值之差的数据框，
#第三个为相似离度
##################
knn.result<-function(Test.spdf, df.sp, n, k, beta=0.5){
    dist.m<-gDistance(df.sp,Test.spdf, byid=T) 
    m.dim= dim(dist.m)
    var.pred<-NULL
    degAD.df<-NULL
    for(i in 1:n){
        pts.kmean<-NULL  #k个邻近点的均值
        for(j in 1:m.dim[1]){
            pts.kmean<-c(pts.kmean,mean(df.sp@data[which(rank(dist.m[j,])<k),i+5]))
        }
        var.pred<-cbind(var.pred, pts.kmean)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], var.pred[,i], beta) )
    }
    var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}

