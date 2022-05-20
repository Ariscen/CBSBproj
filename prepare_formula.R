# import library
# install.packages("deSolve")
library(deSolve)
library(ggplot2)
library(pbapply)

# set dir
setwd("D:/Term6/CBSB/")
load("D:/Term6/CBSB/CBSBproj_workspace.RData")
#PULSE is a square wave function where
#t is time, U set for how long a period is, D set for how long a pulse last, t0 set for the starting time point,
#num set for how many first waves exist
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

# ### original
# IFFL<-function(t,state,parameters){with(as.list(c(state,parameters)),{
#   dx = b*PULSE(t,U,D,t0)-x
#   dr = b*PULSE(t,U,D,t0)-((yR*(x^n))/((x^n)+1)+y0)*r
#   list(c(list(dx,dr)))
# })
# }
# state=c(x=0,r=0)
# parameters=c(b=1.2,yR=10,y0=0,n=110,U=4,D=1,t0=1)
# times=seq(0,20,by = 0.01)
# out=ode(y=state,times=times,func=IFFL,parms = parameters)
# plot(out)
# 
# ### original rematching
# IFFL<-function(t,state,parameters){with(as.list(c(state,parameters)),{
#   dY = p1*PULSE(t,U1,D1,t01)-dy*Y
#   dZ = p2*PULSE(t,U1,D1,t01)-dz*Z-Z*(dmax*(Y^n))/((Y^n)+Ky^n)
#   list(c(list(dY,dZ)))
# })
# }
# state=c(Y=0,Z=0)
# parameters=c(p1=1.2,p2=1.2,dy=1,dz=0,dmax=10,Ky=1,n=110,
#              U1=4,D1=1,t01=1)
# times=seq(0,20,by = 0.01)
# out=ode(y=state,times=times,func=IFFL,parms = parameters)
# plot(out)


###### choosing the typical type of IFFL OR CFFL for simplicity
### IFFL
# n:the induced degradation of R should have an ultrasensitive dependence on X, 
# as indicated by a high Hill coefficient (n = 110) (reference here)
IFFL_original<-function(t,state,parameters){with(as.list(c(state,parameters)),{
  dX = Sx*PULSE(t,U1,D1,t01,num1)-p1*X-p2*X-dx*X
  dY = p1*X-dy*Y+Sy*PULSE(t,U2,D2,t02,num2)
  dZ = p2*X-dz*Z-Z*(dmax*(Y^n))/((Y^n)+Ky^n)
  list(c(list(dX,dY,dZ)))
})
}
state=c(X=0,Y=0,Z=0)
parameters=c(Sx=3,Sy=0,
             p1=1,p2=1,
             dx=0,dy=1,dz=0,
             dmax=10,Ky=1,n=110,
             U1=4,D1=1,t01=1,num1=5,
             U2=4,D2=1,t02=1,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL_original,parms = parameters)
matplot.deSolve(out)



### after non-dimentionalizing
# alpha = p1/dy the ratio of Y production rate constant due to X activation and Y degradation rate constant
# beta = (p1+p2+dx)/dy the ratio of the overall consuming rate constant of X and Y degradation rate constant
# gamma0 = dz/dy the ratio of Z degradation rate constant and Y degradation rate constant
# gamma1 = dmax/dy the ratio of the max repressed rate constant of z due to Y inhibition and Y degradation rate constant
# dy can be seen as a constant divided for all parameters
# alpha < beta due to p1,p2,dx,dy>0
# x = X/Ky; y = Y/Ky; z = (p1*Z)/(Ky*p2)
# sx = Sx/(Ky*dy)
# sy = Sy/(Ky*dy)
# t = t*dy
IFFL<-function(t,state,parameters){with(as.list(c(state,parameters)),{
  dx=sx*PULSE(t,U1,D1,t01,num1)-beta*x
  dy=alpha*x-y+sy*PULSE(t,U2,D2,t02,num2)
  dz=alpha*x-z*gamma0-z*(gamma1*(y^n)/(y^n+1))
  list(c(list(dx,dy,dz)))
})
}
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=1,t01=1,num1=5,
             U2=4,D2=1,t02=1,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)

#D1=1
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=1,t01=1,num1=5,
             U2=4,D2=1,t02=1,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)+title(sub = 'D1=1')

pdf("D1_1.pdf")
ggplot(data.frame(out))+geom_line(aes(time,x))+ylim(0,2)
ggplot(data.frame(out))+geom_line(aes(time,y))+ylim(0,1.5)
ggplot(data.frame(out))+geom_line(aes(time,z))+ylim(0,8)
dev.off()

#D1=3
state=c(x=0,y=0,z=0)
parameters=c(alpha=1,beta=2,gamma0=0,gamma1=10,
             sx=3,sy=0,n=110,
             U1=4,D1=3,t01=1,num1=5,
             U2=4,D2=1,t02=1,num2=5)
times=seq(0,20,by = 0.01)
out=ode(y=state,times=times,func=IFFL,parms = parameters)
matplot.deSolve(out)+title(sub = 'D1=3')

pdf("D1_3.pdf")
ggplot(data.frame(out))+geom_line(aes(time,x))+ylim(0,2)
ggplot(data.frame(out))+geom_line(aes(time,y))+ylim(0,1.5)
ggplot(data.frame(out))+geom_line(aes(time,z))+ylim(0,8)
dev.off()
