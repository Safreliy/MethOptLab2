main = function()
{ 
  f = function(x){
    return(x-log(x, exp(1)))
  }
  
  lambda=(1+sqrt(5))/2
  A=0.1
  B=2
  delta=(B-A)/(lambda^2)
  
  X=A+delta
  Y=B-delta
  
  FX=f(X)
  FY=f(Y)
  
  h=0.001
  E=0.01
  
  
  iter=0
  textpos = 2.4
  x=seq(A,B,h)
  ans=0
  while(B-A > 2*E){
    iter=iter+1
    plot(x,f(x), type="l", col="red", lwd="2",
         ylim=c(-0.5,2.5), xlim=c(0,2))
    grid()
    
    points(c(A,B), c(f(A),f(B)), lwd="2", pch=19)
    lines(c(A,A,B,B),c(f(A), -100, f(B), -100),lty="dotted", lwd="3", col="blue")
    
    text(0.8,textpos,paste("Iteration number = ", iter), adj = c(0,0))
    text(0.8,textpos-0.2,paste("A = ", A), adj = c(0,0))
    text(0.8,textpos-0.4,paste("B = ", B), adj = c(0,0))
    text(0.8,textpos-0.6,paste("X = ", X), adj = c(0,0))
    text(0.8,textpos-0.8,paste("Y = ", Y), adj = c(0,0))
    text(1.5,textpos-0.0,paste("FX = ", FX), adj = c(0,0))
    text(1.5,textpos-0.2,paste("FY = ", FY), adj = c(0,0))
  
    
    if(FX>FY){
      A=X
      X=Y
      FX=FY
      Y=A+B-X
      FY=f(Y)
    }
    else{
      B=Y
      Y=X
      FY=FX
      X=A+B-Y
      FX=f(X)
    }
    browser()  
    
    #Sys.sleep(1)
  }
  #finter = f(xinter)
  plot(x,f(x), type="l", col="red", lwd="2",
       ylim=c(-0.5,2.5), xlim=c(0,2))
  grid()
  #points(xinter, y=finter, lwd="3", pch=19)
  #lines(c(xinter,xinter,xinter, -1000),c(-1000, finter, finter, finter),lty="dotted", lwd="3", col="blue")
  xmin=(A+B)/2
  ymin=f(xmin)
  text(0.8,textpos,paste("Iteration number = ", iter), adj = c(0,0))
  text(0.8,textpos-0.2,paste("A = ", A), adj = c(0,0))
  text(0.8,textpos-0.4,paste("B = ", B), adj = c(0,0))
  text(0.8,textpos-0.6,paste("X = ", X), adj = c(0,0))
  text(0.8,textpos-0.8,paste("Y = ", Y), adj = c(0,0))
  text(1.5,textpos-0.0,paste("FX = ", FX), adj = c(0,0))
  text(1.5,textpos-0.2,paste("FY = ", FY), adj = c(0,0))
  text(1.5,textpos-0.4,paste("xmin = ", FY), adj = c(0,0))
  text(1.5,textpos-0.6,paste("ymin = ", FY), adj = c(0,0))
  
}
main()