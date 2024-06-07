ebmaMedian<-function(W, x, sdVec){
  .x <- x[!is.na(x)]
  .W <- W[!is.na(x)]
  .sdVec <- sdVec[!is.na(x)]

  print(1)  
  ebmaCdf<-function(z, .x, .sdVec, .W){
        print(z)        
        sum(.W*pnorm(z, mean=.x, sd=.sdVec))
            }
  l <- min(.x-6*.sdVec)
  u <- max(.x+6*.sdVec)
  print(2)
  out <- uniroot(function(z){ebmaCdf(z)-.5}
                  ,.x=.x, .sdVec=.sdVec, .W=.W
                  , lower = l
                  , upper = u
  )
               
  out
  }

ebmaCdf(0, .W=a, .x=b, .sdVec=gr)

pnorm(.1, )
sum(a*pnorm(.1, a, gr))
a=c(.1, .8, .1)
b=c(-1, 0, 1)
gr=c(1, 2, 8)
ebmaMedian(a, b, gr)


ensembleBMA:::quantBMAnormal(.5, .W, .x, ..sdVec)

cdfBMAnormal` <-
  function (x, WEIGHTS, MEAN, SD, offset = 0)
  {
    #
    # copyright 2006-present, University of Washington. All rights reserved.
    # for terms of use, see the LICENSE file
    #
    sum(WEIGHTS*pnorm(x, mean = MEAN, sd = SD)) - offset
  }
