## f<-function(df) { #just the ratios
##     b0<-df$difmgc
##     del<-df$difmgt-df$difmgc
##     sd(del)/sd(b0)
## }
## load("df.Rdata")
## L<-split(df,df$fn)
## z0<-sapply(L,f)
## z0<-data.frame(fn=names(z0),z0=z0)

## summary(z0$z0)

     
