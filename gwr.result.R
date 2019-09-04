############������Ȩ�ع� ���ݸ�ʽ˵��######
# Sampledf.sp��Testdf.sp��SpatialPointsDataFrame����Testdf.spΪ����������ݣ�
#  �� @data��n+5��Ԫ�أ�ǰ5��Ϊ'name','iii','lat','lon','hh'��
# ���Ϊ����Ҫ��ֵ��˳������һһ��Ӧ
# approachX:����׼�򣬺˺������������á� 
# kernelX���˺���
#����ֵ����ԱΪvar.pred��var.diff��degAD.df���б�����
#��1��Ϊn����Ա��var�����ֵ�����ݿ�
#��2��Ϊʵ������ֵ�Ĳ��3��Ϊ�������
##################
gwr.result<-function(Testdf.sp, Sampledf.sp, n, approachX="AICc", kernelX="gaussian",beta=0.5){
    var.pred<-NULL 
    degAD.df<-NULL 
    for(i in 1:n){
        gwrdfi<-Sampledf.sp[,c(i+5,3:5)] 
        dfi.formula<-DF2formula(gwrdfi@data)
        #�˺���gaussian������׼��AICc���������dif.bw
        dfi.bw<-bw.gwr(dfi.formula,data=gwrdfi,approach=approachX,
           kernel=kernelX,adaptive=T)
        #������Ȩ�ع�
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
