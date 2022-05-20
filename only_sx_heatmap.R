setwd("D:/Term6/CBSB/")
PULSE=function(t,U,D,t0,num){
  res=c()
  for(i in 0:(num-1)){
    if(i*U+t0<=t & t<i*U+D+t0){
      res_tmp=T
      res=c(res,res_tmp)
    }else{
      res_tmp=F
      res=c(res,res_tmp)
    }
  }
  if(mean(res)>0){
    return(1)
  }else{
    return(0)
  }
}
IFFL<-function(t,state,parameters){with(as.list(c(state,parameters)),{
  dx=sx*PULSE(t,U1,D1,t01,num1)-beta*x
  dy=alpha*x-y+sy*PULSE(t,U2,D2,t02,num2)
  dz=alpha*x-z*gamma0-z*(gamma1*(y^n)/(y^n+1))
  list(c(list(dx,dy,dz)))
})
}

#alpha
#all
state=c(x=0,y=0,z=0)
times=seq(0,20,by = 0.01)
alpha_vector=array(seq(0,2,by = 0.1))
D1_vector=array(seq(0,4,by = 0.1))

conditions<-merge(alpha_vector,D1_vector)

#R2
alpha_res_R2<-pbapply(conditions,1,function(x){
  a=as.numeric(x[1])
  D1=as.numeric(x[2])
  parameters=c(alpha=a,beta=2,gamma0=0.01,gamma1=10,
               sx=3,sy=0,n=110,
               U1=4,D1=D1,t01=0,num1=5,
               U2=4,D2=1,t02=0,num2=5)
  out=ode(y=state,times=times,func=IFFL,parms = parameters)
  final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])
  fit<-lm(z~time,final_zs)
  R2<-summary(fit)$r.squared
  return(c(a,D1,R2))
})
alpha_res_R2<-t(alpha_res_R2)
colnames(alpha_res_R2)<-c("alpha","duration","R2")
alpha_res_R2<-data.frame(alpha_res_R2)

#slope
alpha_res_slope<-pbapply(conditions,1,function(x){
  a=as.numeric(x[1])
  D1=as.numeric(x[2])
  parameters=c(alpha=a,beta=2,gamma0=0.01,gamma1=10,
               sx=3,sy=0,n=110,
               U1=4,D1=D1,t01=0,num1=5,
               U2=4,D2=1,t02=0,num2=5)
  out=ode(y=state,times=times,func=IFFL,parms = parameters)
  final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])
  fit<-lm(z~time,final_zs)
  slope<-summary(fit)[["coefficients"]][1,1]
  return(c(a,D1,slope))
})
alpha_res_slope<-t(alpha_res_slope)
colnames(alpha_res_slope)<-c("alpha","duration","slope")
alpha_res_slope<-data.frame(alpha_res_slope)

#output
pdf("alpha_duration.pdf",width = 10,height = 6)
ggplot(alpha_res_R2)+geom_raster(aes(duration,alpha,fill=R2))+
  scale_fill_gradient(low = "#87CEEB", high = "#6A5ACD")
ggplot(alpha_res_slope)+geom_raster(aes(duration,alpha,fill=slope))+
  scale_fill_gradient(low = "#87CEEB", high = "#6A5ACD")
dev.off()

alpha_res_R2[which(alpha_res_R2$duration==4),]
#alpha 0.6-0.7

#gamma1
#all
state=c(x=0,y=0,z=0)
times=seq(0,20,by = 0.01)
gamma1_vector=array(seq(0,10,by = 0.5))
D1_vector=array(seq(0,4,by = 0.1))

conditions<-merge(gamma1_vector,D1_vector)

#R2
gamma1_res_R2<-pbapply(conditions,1,function(x){
  g1=as.numeric(x[1])
  D1=as.numeric(x[2])
  parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=g1,
               sx=3,sy=0,n=110,
               U1=4,D1=D1,t01=0,num1=5,
               U2=4,D2=1,t02=0,num2=5)
  out=ode(y=state,times=times,func=IFFL,parms = parameters)
  final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])
  fit<-lm(z~time,final_zs)
  R2<-summary(fit)$r.squared
  return(c(g1,D1,R2))
})
gamma1_res_R2<-t(gamma1_res_R2)
colnames(gamma1_res_R2)<-c("gamma1","duration","R2")
gamma1_res_R2<-data.frame(gamma1_res_R2)

#slope
gamma1_res_slope<-pbapply(conditions,1,function(x){
  g1=as.numeric(x[1])
  D1=as.numeric(x[2])
  parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=g1,
               sx=3,sy=0,n=110,
               U1=4,D1=D1,t01=0,num1=5,
               U2=4,D2=1,t02=0,num2=5)
  out=ode(y=state,times=times,func=IFFL,parms = parameters)
  final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])
  fit<-lm(z~time,final_zs)
  slope<-summary(fit)[["coefficients"]][1,1]
  return(c(g1,D1,slope))
})
gamma1_res_slope<-t(gamma1_res_slope)
colnames(gamma1_res_slope)<-c("gamma1","duration","slope")
gamma1_res_slope<-data.frame(gamma1_res_slope)

#output
pdf("gamma1_duration.pdf",width = 10,height = 6)
ggplot(gamma1_res_R2)+geom_raster(aes(duration,gamma1,fill=R2))+
  scale_fill_gradient(low = "#87CEEB", high = "#6A5ACD")
ggplot(gamma1_res_slope)+geom_raster(aes(duration,gamma1,fill=slope))+
  scale_fill_gradient(low = "#87CEEB", high = "#6A5ACD")
dev.off()

gamma1_res_R2[which(gamma1_res_R2$duration==4),]
#gamma1 0-0.5

#beta
#all
state=c(x=0,y=0,z=0)
times=seq(0,20,by = 0.01)
beta_vector=array(seq(1,5,by = 0.1))
D1_vector=array(seq(0,4,by = 0.1))

conditions<-merge(beta_vector,D1_vector)

#R2
beta_res_R2<-pbapply(conditions,1,function(x){
  b=as.numeric(x[1])
  D1=as.numeric(x[2])
  parameters=c(alpha=1,beta=b,gamma0=0.01,gamma1=10,
               sx=3,sy=0,n=110,
               U1=4,D1=D1,t01=0,num1=5,
               U2=4,D2=1,t02=0,num2=5)
  out=ode(y=state,times=times,func=IFFL,parms = parameters)
  final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])
  fit<-lm(z~time,final_zs)
  R2<-summary(fit)$r.squared
  return(c(b,D1,R2))
})
beta_res_R2<-t(beta_res_R2)
colnames(beta_res_R2)<-c("beta","duration","R2")
beta_res_R2<-data.frame(beta_res_R2)

#slope
beta_res_slope<-pbapply(conditions,1,function(x){
  b=as.numeric(x[1])
  D1=as.numeric(x[2])
  parameters=c(alpha=1,beta=b,gamma0=0.01,gamma1=10,
               sx=3,sy=0,n=110,
               U1=4,D1=D1,t01=0,num1=5,
               U2=4,D2=1,t02=0,num2=5)
  out=ode(y=state,times=times,func=IFFL,parms = parameters)
  final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])
  fit<-lm(z~time,final_zs)
  slope<-summary(fit)[["coefficients"]][1,1]
  return(c(b,D1,slope))
})
beta_res_slope<-t(beta_res_slope)
colnames(beta_res_slope)<-c("beta","duration","slope")
beta_res_slope<-data.frame(beta_res_slope)

#output
pdf("beta_duration.pdf",width = 10,height = 6)
ggplot(beta_res_R2)+geom_raster(aes(duration,beta,fill=R2))+
  scale_fill_gradient(low = "#87CEEB", high = "#6A5ACD")
ggplot(beta_res_slope)+geom_raster(aes(duration,beta,fill=slope))+
  scale_fill_gradient(low = "#87CEEB", high = "#6A5ACD")
dev.off()

beta_res_R2[which(beta_res_R2$duration==4),]
#beta 3.1-3.2

#sx
#all
state=c(x=0,y=0,z=0)
times=seq(0,20,by = 0.01)
sx_vector=array(seq(0,5,by = 0.1))
D1_vector=array(seq(0,4,by = 0.1))

conditions<-merge(sx_vector,D1_vector)

#R2
sx_res_R2<-pbapply(conditions,1,function(x){
  sx=as.numeric(x[1])
  D1=as.numeric(x[2])
  parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
               sx=sx,sy=0,n=110,
               U1=4,D1=D1,t01=0,num1=5,
               U2=4,D2=1,t02=0,num2=5)
  out=ode(y=state,times=times,func=IFFL,parms = parameters)
  final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])
  fit<-lm(z~time,final_zs)
  R2<-summary(fit)$r.squared
  return(c(sx,D1,R2))
})
sx_res_R2<-t(sx_res_R2)
colnames(sx_res_R2)<-c("sx","duration","R2")
sx_res_R2<-data.frame(sx_res_R2)

#slope
sx_res_slope<-pbapply(conditions,1,function(x){
  sx=as.numeric(x[1])
  D1=as.numeric(x[2])
  parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
               sx=sx,sy=0,n=110,
               U1=4,D1=D1,t01=0,num1=5,
               U2=4,D2=1,t02=0,num2=5)
  out=ode(y=state,times=times,func=IFFL,parms = parameters)
  final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])
  fit<-lm(z~time,final_zs)
  slope<-summary(fit)[["coefficients"]][1,1]
  return(c(sx,D1,slope))
})
sx_res_slope<-t(sx_res_slope)
colnames(sx_res_slope)<-c("sx","duration","slope")
sx_res_slope<-data.frame(sx_res_slope)

#output
pdf("sx_duration.pdf",width = 10,height = 6)
ggplot(sx_res_R2)+geom_raster(aes(duration,sx,fill=R2))+
  scale_fill_gradient(low = "#87CEEB", high = "#6A5ACD")
ggplot(sx_res_slope)+geom_raster(aes(duration,sx,fill=slope))+
  scale_fill_gradient(low = "#87CEEB", high = "#6A5ACD")
dev.off()

sx_res_R2[which(sx_res_R2$duration==4),]
#sx 1.9-2.0