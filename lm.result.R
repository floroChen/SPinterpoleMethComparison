############��Ԫ�ع� ���ݸ�ʽ˵��######
# df��Test.df��DataFrame����Test.dfΪ����������ݣ�
# dataframe��n+5��Ԫ�أ�ǰ5��Ϊ'name','iii','lat','lon','hh'��
# ���Ϊ����Ҫ��ֵ��˳������һһ��Ӧ
#����ֵ����ԱΪvar.lm, var.pred��var.diff��degAD.df���б�����
#��1,2��Ϊn����Ա��var�Ļع�ģ���б������ֵ�����ݿ�
#������Ϊʵ������ֵ�Ĳ���ĸ�Ϊ�������
##################
lm.result<-function(Test.df, df, n, beta=0.5){
    var.lm<-NULL
    var.pred<-NULL 
    degAD.df<-NULL 
    for(i in 1:n){
        var.lm<-df[,c(i+5,3:5)] %>% 
            lm(DF2formula(.),data=.) %>%   #var(i)~lat+lon+hh
            c(var.lm, list(.))  
        var.lm<-var.lm[-(1:12)]  #ɾ��list(.)������ģ��������Ϣ
        var.pred<-predict(var.lm[[i]], Test.df) %>% 
            cbind(var.pred, v.pred= . )
        degAD.df<-degAD2(Test.df[,i+5], var.pred[,i], beta) %>%
            cbind(degAD.df, degAD=. )  
    }
    var.diff<-Test.df[,6:(n+5)]-var.pred
    return(list(var.lm=var.lm, var.pred=var.pred, var.diff=var.diff, degAD.df=degAD.df))
}