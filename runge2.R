f=function(x,y){-2*x-y}
h=0.1;x0=0;y0=-1
k1=h*f(x0,y0); k2=h*f(x0+0.5*h,y0+0.5*k1)
k3=h*f(x0+0.5*h,y0+0.5*k2);k4=h*f(x0+h,y0+k3)
y1=y0+(1/6)*(k1+2*k2+2*k3+k4)
data.frame(k1,k2,k3,k4,y1)

f=function(x,y){-2*x-y}
h=0.1;x0=0.1;y0=-0.9145125
k1=h*f(x0,y0); k2=h*f(x0+0.5*h,y0+0.5*k1)
k3=h*f(x0+0.5*h,y0+0.5*k2);k4=h*f(x0+h,y0+k3)
y1=y0+(1/6)*(k1+2*k2+2*k3+k4)
data.frame(k1,k2,k3,k4,y1)