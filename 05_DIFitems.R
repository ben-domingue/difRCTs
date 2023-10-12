load("/home/bd/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")

del<-df$difmgt-df$difmgc
cor(del,df$lpm)

tab1<-list()
tab2<-list()
tab1$dif<-cor(df$difficulty,del)
tab2$dif<-cor(df$difficulty,del,method='spearman')
tab1$disc<-cor(df$disc,del)
tab2$disc<-cor(df$disc,del,method='spearman')


f<-function(fn) {
    load(paste0('/home/bd/Dropbox/projects/measurement_rcts/02_itemresponse_data/',fn))
    for (i in 1:length(y)) assign(names(y)[i],y[[i]])
    r2<-resp[treat==0,]
    cm<-colMeans(r2)
    rm<-rowMeans(r2,na.rm=TRUE)
    corr<-numeric()
    for (i in 1:ncol(resp)) corr[i]<-cor(rm,r2[,i],use='p')
    data.frame(fn=fn,item=1:ncol(resp),difficulty.c=cm,discrimination.c=corr)
    } 
lf<-list.files(path='/home/bd/Dropbox/projects/measurement_rcts/02_itemresponse_data')
x<-lapply(lf,f)
x<-data.frame(do.call("rbind",x))

df<-merge(df,x)
tab1$dif.c<-cor(df$difficulty.c,del)
tab2$dif.c<-cor(df$difficulty.c,del,method='spearman')
tab1$disc.c<-cor(df$discrimination.c,del)
tab2$disc.c<-cor(df$discrimination.c,del,method='spearman')

tab<-rbind(tab1,tab2)
library(xtable)
xtable(tab)

## pdf("/home/bd/Dropbox/Apps/Overleaf/DIF Education RCTs/difitems.pdf",width=7,height=3.3)
## par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.75,4))
## plot(df$difficulty,df$lpm,pch=19,col='gray',cex=.5,xlab="Difficulty",ylab="LPM DIF estimate")
## txt<-paste0("r=",round(cor(df$difficulty,df$lpm),2))
## legend("topleft",bty='n',legend=txt)
## plot(df$disc,df$lpm,pch=19,col='gray',cex=.5,xlab="Discrimination",ylab="LPM DIF estimate")
## txt<-paste0("r=",round(cor(df$disc,df$lpm),2))
## legend("topleft",bty='n',legend=txt)
## dev.off()
