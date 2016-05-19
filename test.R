H <- function(data, memsize, bit){
  ys <- 1:length(data)
  gs <- 1:length(data)
  hs <- 1:length(data)
  h = 0
  g <- 1:memsize
  for(i in 1:memsize){
    g[i]<-0
  }
  for(j in 1:length(data)){
    for(i in 1:memsize-1){
      g[i]<-g[i+1]
    }
    g[memsize] <- !xor(data[j], h%%2)
    y <- xor(data[j], h%%2)
    h <- (sum(g*1:memsize))%%bit
    ys[j] <- y
    gs[j] <- g[memsize]
    hs[j] <- h
  }
  return(data.frame(y=ys, g=gs, h=hs))
}
