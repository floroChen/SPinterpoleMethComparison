############��ͨ�����2  ���ݸ�ʽ˵��######
# df.sp��Test.spdf��Spatial*DataFrame����Test.spdfΪ����������ݣ�
# dataframe��n+5��Ԫ�أ�ǰ5��Ϊ'name','iii','lat','lon','hh'��
# ���Ϊ����Ҫ��ֵ��˳������һһ��Ӧ
# Spatial*�����������ص��Ŀռ�㣬�����ڱ�������ȥ��
# map.raster: դ�񻯵ĵ�ͼ("RasterLayer"���󣩣���Ӧdf.sp���о�����
# beta��������ȵ�ֵϵ��D����beta����ϵ��S����1-beta
#����ֵ����ԱΪvar.pred��var.diff��degAD.df���б�����
#��һ��Ϊn����Ա��var���ֵ�����ݿ򣬵ڶ���Ϊʵ��-���ֵ֮������ݿ�
#������Ϊ�������
##################
g_ok.result<-function(map.raster, Test.spdf, df.sp, n, beta=0.5){    
    var.pred<-NULL
    degAD.df<-NULL
    var.formula<-formula("x~1")
    for(i in 1:n) {
        dfx<-df.sp[,i+5]
        colnames(dfx@data)<- "x"
        vgmodel<-autofitVariogram(formula=var.formula, input_data=dfx, model = c("Sph", "Exp", "Gau", "Log", "Ste", "Wav"))
        vgmodel<-vgmodel$var_model
        #������ڲ�
        g_ok<-gstat(formula=var.formula, data=dfx, model=vgmodel) %>%
           interpolate(map.raster, . ) %>%
           mask(. , Test.spdf) %>%  #��Ĥ��ȡ
           rasterToPoints(. , spatial=T)    
        var.pred<-cbind(var.pred, g_ok@data$var1.pred)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], var.pred[,i], beta) )
    }
    var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}
