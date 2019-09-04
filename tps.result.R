############���ݸ�ʽ˵��######
# df.sp��Test.spdf��Spatial*DataFrame����Test.spdfΪ����������ݣ�
# dataframe��n+5��Ԫ�أ�ǰ5��Ϊ'name','iii','lat','lon','hh'��
# ���Ϊ����Ҫ��ֵ��˳������һһ��Ӧ
# map.raster: դ�񻯵ĵ�ͼ("RasterLayer"���󣩣���Ӧdf.sp���о�����
# beta��������ȵ�ֵϵ��D����beta����ϵ��S����1-beta
#����ֵ����ԱΪvar.pred��var.diff��degAD.df���б�����
#��һ��Ϊn����Ա��var���ֵ�����ݿ򣬵ڶ���Ϊʵ��-���ֵ֮������ݿ�
#������Ϊ�������
##################
tps.result<-function(map.raster, Test.spdf, df.sp, n, beta=0.5){ 
    var.pred<-NULL
    degAD.df<-NULL
    df.cor<-coordinates(df.sp)
    for(i in 1:n) {
        tps<-Tps(df.cor, df.sp@data[,i+5]) %>%
             interpolate(map.raster, .) %>%
             mask(.,Test.spdf) %>%  #��Ĥ��ȡ
             rasterToPoints(. , spatial=T)    
        var.pred<-cbind(var.pred, tps@data$layer)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], 
            var.pred[,i], beta) )
    }
    var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}
