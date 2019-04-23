
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