
#Generalization Error

main = function() {
  
  compareEinVsEout(40,0.2)
  
}

compareEinVsEout = function(dof=5,errPct=0.1) {
  setwd("C:/Users/rwdavis513/Documents/R/Training/")
  x = createX(dof,2000)
  trueWeights = round(rnorm(dof,10,5),0)
  noiseStdDev = mean(x)*mean(trueWeights)*errPct
  y = createY(trueWeights,x,noiseStdDev)
  
  #plot(x[,3],y)
  
  square = function(x1){x1^2}
  Ein = c()
  Eout = c()
  Einrsq = c()
  #title = paste(trueWeights,collapse=" ")
  title = paste(dof, " parameters with ErrPct of ",errPct,sep="")
  png(paste(title,".png",sep=""))
  plot(c(1,500),c(10,10000),log="y",col="white",ylab="Error",xlab="n = # of Samples out of 1000",main=title)
  abline(h=noiseStdDev^2,col="black")
  abline(v=dof+1)
  for (i in (dof+1):500) {
    c(xSub,ySub) := sampleXY(i,x,y)
    
    xyLM = lm(ySub ~ ., data=data.frame(xSub,ySub))
    
    Ein[i] = sum(sapply(xyLM$residuals, square))/nrow(xSub)
    #Einrsq[i-dof] = summary(xyLM)$r.squared
    
    Eout[i] = sum(sapply(predict(xyLM,newdata=data.frame(x))-y,square))/nrow(x)
    points(i,Ein[i],pch=16,col="blue")
    points(i,Eout[i],pch=16,col="red")
  }
  #points((dof+1):500,Ein,pch=16,col="blue")
  #points((dof+1):500,Eout,pch=16,col="red")
  
  dev.off()
}



#http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value

':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}


sampleXY = function(nSample=1,x,y){
  
  #xy = data.frame(x,y)
  #xySub = xy[sample(nrow(x),nSample),]
  #x = matrix(xySub[,1:3])
  #y = matrix(xySub[,4])
  sampleRows = sample(nrow(x),nSample)
  x = x[sampleRows,]
  y = y[sampleRows]
  
  return(list(x,y))
}


createX = function(d=3,n=1000) {
  # create a matrix x with d parameters and n points
  x = matrix(data=NA,ncol=d,nrow=n)
  for (i in 1:d){
    x[,i] = rnorm(n,10,3)
  }
  return(x)
}

createY = function(weights,x,noiseStdDev) {
  # weights = a vector of weights to be applied to x
  
  if (ncol(x) != length(weights)) {die ("The number of columns in x doesn't equal the rows in the weights.")}
  
  y = x%*%weights + rnorm(nrow(x),0,noiseStdDev)
  #   [1:1000,1:3] %*% [1:3,1]
}