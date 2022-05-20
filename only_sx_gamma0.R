library(deSolve)
library(ggplot2)
library(pbapply)
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
# for gamma0
## gamma0 = 0
pdf("gamma0_0.pdf")
# D1 = 1
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=1,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)
# D1 = 3
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=3,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)
dev.off()
## gamma0 = 0.01
pdf("gamma0_001.pdf")
# D1 = 1
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=1,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)
# D1 = 3
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=3,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)
dev.off()
## gamma0 = 0.1
pdf("gamma0_01.pdf")
# D1 = 1
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.1,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=1,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)
# D1 = 3
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.1,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=3,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)
dev.off()
### the increase of gamma0 will cause the increase of z in every pulse will be smaller
#a small gamma0 should exhibit the ability to differentiate

# for final concentration of z and its calibration
pdf("z_final_gamma0_0_01_1.pdf")
# gamma0 0
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=1,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)

final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])

fit<-lm(z~time,final_zs)
R2<-summary(fit)$r.squared
ggplot(final_zs)+geom_point(aes(time,z))+
  geom_text(aes(3,8,label=paste('R^2 =',round(R2,2))))+geom_smooth(aes(time,z),method = "lm")

# gamma0 0.1
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.1,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=1,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)

final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])

fit<-lm(z~time,final_zs)
R2<-summary(fit)$r.squared
ggplot(final_zs)+geom_point(aes(time,z))+
  geom_text(aes(3,8,label=paste('R^2 =',round(R2,2))))+geom_smooth(aes(time,z),method = "lm")

# gamma0 1
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=1,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=1,t01=0,num1=5,
             U2=4,D2=1,t02=0,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)

final_zs=data.frame(out[seq(101,2001,by = 100),c(1,4)])

fit<-lm(z~time,final_zs)
R2<-summary(fit)$r.squared
ggplot(final_zs)+geom_point(aes(time,z))+
  geom_text(aes(3,8,label=paste('R^2 =',round(R2,2))))+geom_smooth(aes(time,z),method = "lm")
dev.off()
