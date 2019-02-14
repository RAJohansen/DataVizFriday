#SOURCE: https://stackoverflow.com/questions/8082429/plot-a-heart-in-r

#Heart #1
dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l"))
with(dat, polygon(x,y, col="red"))  

#Heart #2 
MASS::eqscplot(0:1,0:1,type="n",xlim=c(-1,1),ylim=c(-0.8,1.5))
curve(4/5*sqrt(1-x^2)+sqrt(abs(x)),from=-1,to=1,add=TRUE,col=2)
curve(4/5*-sqrt(1-x^2)+sqrt(abs(x)),from=-1,to=1,add=TRUE,col=2)
