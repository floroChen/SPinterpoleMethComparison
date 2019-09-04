############数据格式说明######
# df.sp和Test.spdf：Spatial*DataFrame对象，Test.spdf为待检验点数据，
# dataframe有n+5列元素，前5列为'name','iii','lat','lon','hh'，
# 其后为气象要素值，顺序两者一一对应
# map.raster: 栅格化的地图("RasterLayer"对象），对应df.sp所研究区域
# idpx=5 #距离权重平滑因子
# beta：相似离度的值系数D比重beta，形系数S比重1-beta
#返回值：成员为var.pred和var.diff、degAD.df的列表对象，
#第一个为n个成员的var拟合值的数据框，第二个为实测-拟合值之差的数据框，
#第三个为相似离度
##################
idw2.result<-function(map.raster,Test.spdf, df.sp, n, idpx=5, beta=0.5){
    var.pred<-NULL
    degAD.df<-NULL
    for(i in 1:n) {
        dfx<-df.sp[,i+5]
        colnames(dfx@data)<- "x"
        g.idw<-gstat(formula=x~1, data=dfx, set=list(idp=idpx))%>% #IDW模型
             interpolate(map.raster, . ) %>% #特定范围内插
             mask(. , Test.spdf) %>%  #掩膜提取
             rasterToPoints(., spatial=T) #转成SpatialPointsDataFrame,点位按先y降序后x升序排列
        var.pred<-cbind(var.pred, g.idw@data$var1.pred)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], var.pred[,i], beta) )
     }
     var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
     return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}
