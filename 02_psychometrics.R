##counts
load("~/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")
length(unique(df$fn))
table(df$group)
L<-split(df,df$fn)
table(sapply(L,function(x) x$group[1]))
z<-lapply(L,function(x) x[1,])
z<-data.frame(do.call("rbind",z))
by(z$nresp,z$group,sum)
sum(z$nresp)
f<-function(fn) {
    load(fn)
    for (i in 1:length(y)) assign(names(y)[i],y[[i]])
    sum(!is.na(resp))
}
setwd('~/Dropbox/projects/measurement_rcts/02_itemresponse_data/')
lf<-list.files()
NN<-sapply(lf,f)
sum(NN)
#####################################################


load("~/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")

df$del<-df$difmgt-df$difmgc
x1<-by(df$del,df$fn,mean)
x2<-by(df$es,df$fn,mean)
cor(x1,x2)
n<-by(df$item,df$fn,length)
z<-data.frame(x1,x2)
wt<-as.numeric(n)
weighted_corr <- cov.wt(z, wt = wt, cor = TRUE)
weighted_corr

plot(x2,x1,xlab='SS',ylab='delta')
legend("topleft",bty='n',legend=round(cor(x1,x2),4))
cbind(x1,x2)->z
#z<-z[z[,1]> -1.5,]
#cor(z)
#53_parental_math__1.Rdata
#GIRLSEDU_literacy__1.Rdata




f<-function(fn) {
    load(fn)
    for (i in 1:length(y)) assign(names(y)[i],y[[i]])
    #N and Ni
    N<-nrow(resp)
    ni<-ncol(resp)
    #alpha
        kr20<-function(resp) { 
            k<-ncol(resp)
            p<-colMeans(resp,na.rm=TRUE)
            q<-1-p
            o<-rowSums(resp)
            (k/(k-1))*(1-sum(p*q)/var(o))
        }
    alpha<-kr20(resp)
    c(fn=fn,N=N,ni=ni,alpha=alpha)
}
setwd('~/Dropbox/projects/measurement_rcts/02_itemresponse_data/')
lf<-list.files()
df<-lapply(lf,f)
x<-data.frame(do.call("rbind",df))
for (i in 2:ncol(x)) x[,i]<-as.numeric(x[,i])

load("~/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")
tmp<-df[,c("fn","es")]
tmp<-tmp[!duplicated(tmp$fn),]
x<-merge(x,tmp)



pdf("~/Dropbox/Apps/Overleaf/DIF Education RCTs/psychometrics.pdf",width=8,height=3)
par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
hist(x$N,xlab="N respondents",col='red',main='')
mtext(side=3,adj=0,line=0,'A')
mean(x$N)
plot(x$ni,x$alpha,pch=19,xlab="N items",ylab="Reliability",cex=.7,col='gray',bty='n')
abline(h=.7,lty=2)
mtext(side=3,adj=0,line=0,'B')
hist(x$es,xlab="Effect Size",col='blue',main='',xlim=c(-.3,.3))
mean(x$es)
abline(v=0)
mtext(side=3,adj=0,line=0,'C')
plot(df$difficulty,df$disc,cex=.7,pch=19,col='gray',xlab="Difficulty",ylab="Discrimination",bty='n')
mtext(side=3,adj=0,line=0,'D')
dev.off()

sum(x$alpha>0.7)
