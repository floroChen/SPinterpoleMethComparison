############泛克里格法2  数据格式说明######
# df.sp和Test.spdf：Spatial*DataFrame对象，Test.spdf为待检验点数据，
# dataframe有n+5列元素，前5列为'name','iii','lat','lon','hh'，
# 其后为气象要素值，顺序两者一一对应
# Spatial*对象不允许有重叠的空间点，若存在必须先行去除
# map.sp: DEM地图的SpatialPointsDataFrame对象，对应df.sp所研究区域，
#   @data的列名为'lat','lon','hh'
# beta：相似离度的值系数D比重beta，形系数S比重1-beta
#返回值：成员为var.pred和var.diff、degAD.df的列表对象，
#第一个为n个成员的var拟合值的数据框，第二个为实测-拟合值之差的数据框，
#第三个为相似离度
##################
g_uk.result<-function(map.sp, Test.spdf, df.sp, n, beta=0.5){    
    var.pred<-NULL
    degAD.df<-NULL
    for(i in 1:n) {
        dfx<-df.sp[,c(i+5,3:5)]
        var.formula=DF2formula(dfx@data)  #DF2formula: var(i)~lat+lon+hh
        vgmodel<-autofitVariogram(formula=var.formula, input_data=dfx, model = c("Sph", "Exp", "Gau", "Log", "Ste", "Wav"))
        vgmodel<-vgmodel$var_model
        #克里格内插
        g_uk<-gstat(formula=var.formula, data=dfx, model=vgmodel) %>%
            predict(., newdata=map.sp)  #与map.sp同类型（points）
        g_uk.pred<-pointDistance(g_uk, Test.spdf) %>%
            apply(.,2, which.min) %>%  #最邻近的1个点，因为网格经纬度坐标有舍入等差异
            g_uk[. ,]   #提取对应点的插值
        var.pred<-cbind(var.pred, g_uk.pred@data$var1.pred)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], var.pred[,i], beta) )
    }
    var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}

