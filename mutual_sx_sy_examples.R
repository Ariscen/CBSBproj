setwd("D:/Term6/CBSB/")
#PULSE_new is able to decide which period to have pulses
PULSE_new=function(t,U,D,t0,n1,n2){
  res=c()
  for(i in (n1-1):(n2-1)){
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
IFFL_new<-function(t,state,parameters){with(as.list(c(state,parameters)),{
  dx=sx*PULSE_new(t,U1,D1,t01,n1_1,n2_1)-beta*x
  dy=alpha*x-y+sy*PULSE_new(t,U2,D2,t02,n1_2,n2_2)
  dz=alpha*x-z*gamma0-z*(gamma1*(y^n)/(y^n+1))
  list(c(list(dx,dy,dz)))
})
}
# y with different number of pulses
pdf("sxy_samefreq.pdf")
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
             sx=3,sy=1,n=110,
             U1=4,D1=1,t01=0,n1_1=1,n2_1=5,
             U2=4,D2=1,t02=0,n1_2=2,n2_2=2)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL_new,parms = parameters)
matplot.deSolve(out)

state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
             sx=3,sy=1,n=110,
             U1=4,D1=1,t01=0,n1_1=1,n2_1=5,
             U2=4,D2=1,t02=0,n1_2=4,n2_2=4)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL_new,parms = parameters)
matplot.deSolve(out)

state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
             sx=3,sy=1,n=110,
             U1=4,D1=1,t01=0,n1_1=1,n2_1=5,
             U2=4,D2=1,t02=0,n1_2=1,n2_2=3)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL_new,parms = parameters)
matplot.deSolve(out)
dev.off()

# y with different starting time
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
             sx=3,sy=1,n=110,
             U1=4,D1=1,t01=0,n1_1=1,n2_1=5,
             U2=4,D2=1,t02=2,n1_2=1,n2_2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL_new,parms = parameters)
matplot.deSolve(out)

state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0.01,gamma1=10,
             sx=3,sy=1,n=110,
             U1=4,D1=1,t01=0,n1_1=1,n2_1=5,
             U2=4,D2=1,t02=0,n1_2=1,n2_2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL_new,parms = parameters)
matplot.deSolve(out)

# more complicated situations can be simulated with two stimuli with changeable frequencies and etc.