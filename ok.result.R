############数据格式说明######
# df.sp和Test.spdf：Spatial*DataFrame对象，Test.spdf为待检验点数据，
# dataframe有n+5列元素，前5列为'name','iii','lat','lon','hh'，
# 其后为气象要素值，顺序两者一一对应
# nmax, nmin：最多和最少搜索点个数
# map.raster: 栅格化的地图("RasterLayer"对象），对应df.sp所研究区域
# beta：相似离度的值系数D比重beta，形系数S比重1-beta
#返回值：成员为var.pred和var.diff、degAD.df的列表对象，
#第一个为n个成员的var拟合值的数据框，第二个为实测-拟合值之差的数据框，
#第三个为相似离度
##################
ok.result<-function(map.raster, Test.spdf, df.sp, n, 
nmax=15, nmin=10, beta=0.5){
    map.grid<-rasterToPoints(map.raster, spatial=T) %>%
        as(. , "SpatialPixelsDataFrame")
    #map.grid<-as(map.raster, "SpatialGridDataFrame")
    var.pred<-NULL
    degAD.df<-NULL
    var.formula<-formula("x~1")
    for(i in 1:n) {
        dfx<-df.sp[,i+5]
        colnames(dfx@data)<- "x"
        vgmodel<-autofitVariogram(formula=var.formula, input_data=dfx, model = c("Sph", "Exp", "Gau", "Log", "Ste", "Wav"))
        vgmodel<-vgmodel$var_model
        ok<-krige(formula=var.formula, model=vgmodel, locations=dfx, newdata=map.grid, nmax=nmax, nmin=nmin)  #克里格内插
        #提取最邻近点
        ok.pred<-pointDistance(ok, Test.spdf) %>%
           apply(.,2, which.min) %>%  #最邻近的1个点，因为网格经纬度坐标有舍入等差异
           ok[. ,]   #提取对应点的插值
        var.pred<-cbind(var.pred, ok.pred@data$var1.pred)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], var.pred[,i], beta) )
    }
    var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}
