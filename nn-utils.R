
init.wgts=function(n.in,n.hid,n.out){
  b1 = runif(n.hid,-.1,.1)
  w1 = matrix(rnorm(n.in*n.hid,0,.1),nrow=n.hid,ncol=n.in)
  b2 = runif(n.out,-.1,.1)
  w2 = matrix(rnorm(n.out*n.hid,0,.1),nrow=n.out,ncol=n.hid)
  list(b1=b1,w1=w1,b2=b2,w2=w2)
}

init.zeros=function(n.in,n.hid,n.out){
  b1 = rep(0, n.hid)
  w1 = matrix(0, nrow=n.hid,ncol=n.in)
  b2 = rep(0, n.out)
  w2 = matrix(0, nrow=n.out,ncol=n.hid)
  list(b1=b1,w1=w1,b2=b2,w2=w2)
}

sigmoid=function(x){
  return(1 / (1 + exp(-x)))
}

relu=function(x){
  for (i in 1:length(x)){
    x[i] = max(0,x[i])
  }
  return(x)
}
drelu=function(x){
  return(1*(x>=0))
}

cost.squared.error=function(x,y,weights,g=sigmoid){
  # Takes dataset x and labels y
  # Computes the squared error cost of the dataset 
  # Returns the cost
  yhats = fwd.prop(x,weights)$a2
  result = sum((y-yhats)^2)
  return(result/(2*length(y)))
}

num.gradient=function(x,y,agent,f,g,cost,eps=1e-8){
  
  c(b1,w1,b2,w2) %<-% agent$weights
  db1=matrix(rep(0), nrow=1, ncol=length(b1))
  for (i in 1:nrow(matrix(b1))){
    bp=bm=as.vector(b1)
    bp[i]=bp[i]+eps
    bm[i]=bm[i]-eps
    db1[i]=(cost(x,y,list(b1=bp,b2=b2,w1=w1,w2=w2))-cost(x,y,list(b1=bm,b2=b2,w1=w1,w2=w2)))/(2*eps)
  } 
  db2=matrix(rep(0), nrow=1, ncol=length(b2))
  for (i in 1:nrow(matrix(b2))){
    bp=bm=as.vector(b2)
    bp[i]=bp[i]+eps
    bm[i]=bm[i]-eps
    db2[i]=(cost(x,y,list(b1=b1,b2=bp,w1=w1,w2=w2))-cost(x,y,list(b1=b1,b2=bm,w1=w1,w2=w2)))/(2*eps)
  }
  dw1=matrix(rep(0), nrow=nrow(w1), ncol=ncol(w1))
  for (i in 1:length(dw1)){
    wp=wm=w1
    wp[i]=wp[i]+eps
    wm[i]=wm[i]-eps
    dw1[i]=(cost(x,y,list(b1=b1,b2=b2,w1=wp,w2=w2))-cost(x,y,list(b1=b1,b2=b2,w1=wm,w2=w2)))/(2*eps)
  }
  dw2=matrix(rep(0), nrow=nrow(w2), ncol=ncol(w2))
  for (i in 1:length(dw2)){
    wp=wm=w2
    wp[i]=wp[i]+eps
    wm[i]=wm[i]-eps
    dw2[i]=(cost(x,y,list(b1=b1,b2=b2,w1=w1,w2=wp))-cost(x,y,list(b1=b1,b2=b2,w1=w1,w2=wm)))/(2*eps)
  } 
  return(list(db1=db1,db2=db2,dw1=dw1,dw2=dw2))
}




