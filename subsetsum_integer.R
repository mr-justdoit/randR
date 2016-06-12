/*
The MIT License (MIT)

Copyright Shun Sugiyama(c) 2016

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

sum.subset.int <- function(U, b){
  if(sum(U<0)==0)return(sum.subset(U, b))
  a <- abs(min(U))+1
  U.prime <- U+a
  for(k in 1:length(U)){
    flag <- sum.subset.k.int(U.prime, b+k*a, k, a)
    if(flag)return(flag)
  }
}

sum.subset.k.int <- function(U, b, k, a){
  L <- U[U<b]
  R <- U[U>=b]
  i <- 0
  s <- c()
  if(length(R)!=0 && min(R)==b){
    s[i<-i+1] <- min(R)
    if(i==k){
      print(s-a)
      return(T)
    }
    else{
      s <- c()
      i <- 0
    }
  }

  while(length(L)>1){
    m <- max(L)
    L <- setdiff(L, m)
    if(sum.subset.k.int(L, b-m, k-1, a)){
       s[i<-i+1] <- m
       print(s-a)
       return(T)
    }
  }
  return(F)
}


sum.subset.k <- function(U, b, k){ 
  L <- U[U<b]
  R <- U[U>=b]
  i <- 0
  s <- c()
  if(length(R)!=0 && min(R)==b){
    s[i<-i+1] <- min(R)
    if(i==k){
      print(s)
      return(T)
    }
    else{
      s <- c()
      i <- 0
    }
  }

  while(length(L)>1){
    m <- max(L)
    L <- setdiff(L, m)
    if(sum.subset.k(L, b-m, k-1)){
       s[i<-i+1] <- m
       print(s)
       return(T)
    }
  }
  return(F)
}



sum.subset <- function(U, b){ 
  L <- U[U<b]
  R <- U[U>=b]
  if(length(R)!=0 && min(R)==b){
    print(min(R))
    return(T)
  }

  while(length(L)>1){
    m <- max(L)
    L <- setdiff(L, m)
    if(sum.subset(L, b-m)){
       print(m)
       return(T)
    }
  }
  return(F)
}
