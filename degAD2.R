#������� Degree of Analogue Deviation
#a,b:�ȳ���ֵ����
#ֵϵ��D����beta����ϵ��S����1-beta
degAD2<-function(a,b,beta=0.5){
    x=BBmisc::normalize(a)-BBmisc::normalize(b)
    E=mean(x);
    D=mean(abs(x));S=mean(abs(x-E));
    return(beta*D+(1-beta)*S)
}