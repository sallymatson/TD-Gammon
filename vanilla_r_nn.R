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


stable.softmax=function(a){
  a=a-max(a)
  ea=exp(a)
  return(t(t(ea)/colSums(ea)))
}

sigmoid=function(x){
  return(1 / (1 + exp(-x)))
}
dsigmoid=function(x){
  return (sigmoid(x)*(1-sigmoid(x)))
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

fwd.prop=function(x,b1,w1,b2,w2,f,g){
  z1 = b1+w1%*%x
  a1 = f(z1)
  z2 = b2+w2%*%a1
  a2 = g(z2)
  return(list(a1=a1,z1=z1,a2=a2,z2=z2))
}

bk.prop=function(x,y,w2,fprop,df){
  m = ncol(y)
  yhat = fprop$a2
  a1 = fprop$a1
  z1 = fprop$z1
  # L = 2
  dc2 = (yhat-y)
  db2 = rowSums(dc2)/m
  dw2 = (dc2 %*% t(a1))/m
  dc1 = df(z1)*(t(w2)%*%dc2)
  # L = 1
  db1 = rowSums(dc1)/m
  dw1 = (dc1 %*% t(x))/m
  return(list(db1=db1,db2=db2,dw1=dw1,dw2=dw2))
}

nnet1.fit=function(x,y,model,f,g,df,cost,ALPHA,max.games,LAMBDA,n.hid){
  
  c(b1,w1,b2,w2) %<-% model
  cost_hist = list()
  b1_hist = list()
  b2_hist = list()
  w1_hist = list()
  w2_hist = list()
  
  for (i in 1:max_games){
    game = play.game(td.move, td.move, FALSE)
    c(et.b1,et.w1,et.b2,et.w2) %<-% init.wgts(28,n.hid, 1)
    for (turn in game){
      fprop = fwd.prop(turn,b1,w1,b2,w2,f,g)
      bprop = bk.prop(x,fprop_next,w2,fprop,df)
      et.b1 = LAMBDA * et.b1 + bprop$db1
      b1 = b1 - ALPHA*et.b1*(fprop_next - fprop)
    }
    
    # Compute error, adjust gradient:
    fprop = fwd.prop(x,b1,w1,b2,w2,f,g)
    bprop = bk.prop(x,y,w2,fprop,df)
    b1 = b1 - ALPHA*bprop$db1
    b2 = b2 - ALPHA*bprop$db2
    w1 = w1 - ALPHA*bprop$dw1
    w2 = w2 - ALPHA*bprop$dw2
    cost_hist[i] = cost(x,y,b1,w1,b2,w2,f,g)
    b1_hist[[i]] = b1
    b2_hist[[i]] = b2
    w1_hist[[i]] = w1
    w2_hist[[i]] = w2
  }
  return(list(b1=b1,w1=w1,b2=b2,w2=w2,cost_hist=cost_hist,b1_hist=b1_hist,b2_hist=b2_hist,w1_hist=w1_hist,w2_hist=w2_hist))
}



