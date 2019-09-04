############���ݸ�ʽ˵��######
# df.sp��Test.spdf��Spatial*DataFrame����Test.spdfΪ����������ݣ�
# dataframe��n+5��Ԫ�أ�ǰ5��Ϊ'name','iii','lat','lon','hh'��
# ���Ϊ����Ҫ��ֵ��˳������һһ��Ӧ
# map.raster: դ�񻯵ĵ�ͼ("RasterLayer"���󣩣���Ӧdf.sp���о�����
# idpx=5 #����Ȩ��ƽ������
# beta��������ȵ�ֵϵ��D����beta����ϵ��S����1-beta
#����ֵ����ԱΪvar.pred��var.diff��degAD.df���б�����
#��һ��Ϊn����Ա��var���ֵ�����ݿ򣬵ڶ���Ϊʵ��-���ֵ֮������ݿ�
#������Ϊ�������
##################
idw.result<-function(map.raster,Test.spdf, df.sp, n, idpx=5, beta=0.5){
    map.grid<-rasterToPoints(map.raster, spatial=T) %>%
        as(. , "SpatialPixelsDataFrame")
    var.pred<-NULL
    degAD.df<-NULL
    for(i in 1:n) {
        dfx<-df.sp[,i+5]
        colnames(dfx@data)<- "x"
        idw.x<-idw(x~1, locations=dfx , newdata=map.grid, idp=idpx) #IDW�ڲ�
        idw.pred<-pointDistance(idw.x,Test.spdf) %>%
           apply(.,2, which.min) %>%  #���ڽ���1���㣬��Ϊ������γ������������Ȳ���
           idw.x[. ,]   #��ȡ��Ӧ��Ĳ�ֵ
        var.pred<-cbind(var.pred, idw.pred@data$var1.pred)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], var.pred[,i], beta) )
    }
    var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}