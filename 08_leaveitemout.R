load("~/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")

f<-function(fn) {
    load(paste0('~/Dropbox/projects/measurement_rcts/02_itemresponse_data/',fn))
    for (i in 1:length(y)) assign(names(y)[i],y[[i]])
    resp0<-resp
    est<-numeric()
    for (i in 1:ncol(resp0)) {
        resp<-resp0[,-i]
        th<-rowSums(resp,na.rm=TRUE)
        th<-(th-mean(th))/sd(th)
        est[i]<-(mean(th[treat==1])-mean(th[treat==0]))
    }
    data.frame(fn=rep(fn,ncol(resp0)),item=1:ncol(resp0),est.loo=est)
} 
lf<-list.files(path='~/Dropbox/projects/measurement_rcts/02_itemresponse_data')
x<-lapply(lf,f)
x<-data.frame(do.call("rbind",x))
df<-merge(df,x)
df$del<-df$difmgt-df$difmgc
df$per<-(df$es-df$est.loo)

cols1<-colorRampPalette(c("red", "white"))(500)
cols2<-colorRampPalette(c("white", "blue"))(500)
cols<-c(cols1,cols2)
ran<-seq(min(df$es),max(df$es),length.out=length(cols))
col<-numeric()
for (i in 1:nrow(df)) col[i]<-which.min(abs(df$es[i]-ran))
cols<-cols[col]
cols<-col2rgb(cols)
col<-numeric()
for (i in 1:ncol(cols)) col[i]<-rgb(cols[1,i],cols[2,i],cols[3,i],alpha=160,max=255)

pdf("~/Dropbox/Apps/Overleaf/DIF Education RCTs/loo.pdf",width=4,height=3.3)
par(mgp=c(2,1,0),mar=c(3,3,1,1))
#plot(df$est.loo,df$del)
#cor(df$del,df$est.loo)
plot(df$del,df$per,col=col,pch=19,cex=.5,xlab=expression(beta[j]),ylab="LOO difference")
cor(df$del,df$per)
abline(lm(per~del,df))
##
z<-df$per/df$es
ii<-which.max(z*ifelse(df$effect.sig<.05,1,0))
points(df$del[ii],df$per[ii],pch=1,cex=3)
dev.off()

df[ii,]

## f<-function(x) {
##     i<-which.max(x$per/x$es)
##     j<-which.min(x$per/x$es)
##     list(max=x[i,],min=x[j,])
## }
## L<-split(df,df$fn)
## L<-lapply(L,f)
## ##
## par(mfrow=c(1,2))
## l1<-lapply(L,"[[",1)
## l1<-data.frame(do.call("rbind",l1))
## cols<-ifelse(l1$effect.sig<.05,'red','gray')
## plot(l1$es,l1$est.loo,col=cols,pch=19,xlab='Effect Size',ylab='LOO difference'); abline(0,1)
## ##
## l2<-lapply(L,"[[",2)
## l2<-data.frame(do.call("rbind",l2))
## cols<-ifelse(l2$effect.sig<.05,'red','gray')
## plot(l2$es,l2$est.loo,col=cols,pch=19,xlab='Effect Size',ylab='LOO difference'); abline(0,1)
