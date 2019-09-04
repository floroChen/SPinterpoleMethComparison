############���ݸ�ʽ˵��######
# df.sp��Test.spdf��Spatial*DataFrame����Test.spdfΪ����������ݣ�
# dataframe��n+5��Ԫ�أ�ǰ5��Ϊ'name','iii','lat','lon','hh'��
# ���Ϊ����Ҫ��ֵ��˳������һһ��Ӧ
# k��ȡ�ڽ���k����
# beta��������ȵ�ֵϵ��D����beta����ϵ��S����1-beta
#����ֵ����ԱΪvar.pred��var.diff��degAD.df���б�����
#��һ��Ϊn����Ա��var���ֵ�����ݿ򣬵ڶ���Ϊʵ��-���ֵ֮������ݿ�
#������Ϊ�������
##################
knn.result<-function(Test.spdf, df.sp, n, k, beta=0.5){
    dist.m<-gDistance(df.sp,Test.spdf, byid=T) 
    m.dim= dim(dist.m)
    var.pred<-NULL
    degAD.df<-NULL
    for(i in 1:n){
        pts.kmean<-NULL  #k���ڽ���ľ�ֵ
        for(j in 1:m.dim[1]){
            pts.kmean<-c(pts.kmean,mean(df.sp@data[which(rank(dist.m[j,])<k),i+5]))
        }
        var.pred<-cbind(var.pred, pts.kmean)
        degAD.df<-cbind(degAD.df, degAD2(Test.spdf@data[,i+5], var.pred[,i], beta) )
    }
    var.diff<-Test.spdf@data[, 6:(n+5)]- var.pred
    return(list(var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}
