############�������2  ���ݸ�ʽ˵��######
# df.sp��Test.spdf��Spatial*DataFrame����Test.spdfΪ����������ݣ�
# dataframe��n+5��Ԫ�أ�ǰ5��Ϊ'name','iii','lat','lon','hh'��
# ���Ϊ����Ҫ��ֵ��˳������һһ��Ӧ
# Spatial*�����������ص��Ŀռ�㣬�����ڱ�������ȥ��
# map.sp: DEM��ͼ��SpatialPointsDataFrame���󣬶�Ӧdf.sp���о�����
#   @data������Ϊ'lat','lon','hh'
# beta��������ȵ�ֵϵ��D����beta����ϵ��S����1-beta
#����ֵ����ԱΪvar.pred��var.diff��degAD.df���б�����
#��һ��Ϊn����Ա��var���ֵ�����ݿ򣬵ڶ���Ϊʵ��-���ֵ֮������ݿ�
#������Ϊ�������
##################
g_uk.result<-function(map.sp, Test.spdf, df.sp, n, beta=0.5){    
    var.pred<-NULL
    degAD.df<-NULL
    for(i in 1:n) {
        dfx<-df.sp[,c(i+5,3:5)]
        var.formula=DF2formula(dfx@data)  #DF2formula: var(i)~lat+lon+hh
        vgmodel<-autofitVariogram(formula=var.formula, input_data=dfx, model = c("Sph", "Exp", "Gau", "Log", "Ste", "Wav"))
        vgmodel<-vgmodel$var_model
        #������ڲ�
        g_uk<-gstat(formula=var.formula, data=dfx, model=vgmodel) %>%
            predict(., newdata=map.sp)  #��map.spͬ���ͣ�points��
        g_uk.pred<-pointDistance(g_uk, Test.spdf) %>%
            apply(.,2, which.min) %>%  #���ڽ���1���㣬��Ϊ����γ������������Ȳ���
            g_uk[. ,]   #��ȡ��Ӧ��Ĳ�ֵ
        var.pred<-cbind(var.pred, g_uk.pred@data$var1.pred)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], var.pred[,i], beta) )
    }
    var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}
