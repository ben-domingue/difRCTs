
## load("/home/bd/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")

## del<-df$difmgt-df$difmgc
## cor(del,df$lpm)

## summary(del)
## cor(df$lpm,df$dif,method='spearman')
## summary(df$lpm)




## f<-function(df) { #just the ratios
##     b0<-df$difmgc
##     del<-df$difmgt-df$difmgc
##     sd(del)/sd(b0)
## }
## L<-split(df,df$fn)
## z0<-lapply(L,f)

## summary(unlist(z0))
