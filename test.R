H <- function(data, memsize){
  ys <- 1:length(data)
  gs <- 1:length(data)
  hs <- 1:length(data)
  g <- 1:memsize
  for(i in 1:memsize){
    g[i]<-round(runif(1))
  }
  h <- sum(g)>(1+memsize)/2
  for(j in 1:length(data)){
    for(i in 1:memsize-1){
      g[i]<-g[i+1]
    }
    g[memsize] <- !xor(data[j],h)
    y <- !xor(data[j], h)
    h <- sum(g)>(1+memsize)/2
    ys[j] <- y
    gs[j] <- g[memsize]
    hs[j] <- h
  }
  return(data.frame(y=ys, g=gs, h=hs))
}
