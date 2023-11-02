load("~/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")

####################################
##descriptives of delta
df$del<-df$difmgt-df$difmgc
range(df$del)
quantile(df$del,c(.25,.75))
library(e1071)
skewness(df$del)
summary(df$del)

summary(df$lpm) #lpm dif estimates

###get new p-values
se<-sqrt(df$difmgc.se^2+df$difmct.se^2)
est<-(df$difmgt-df$difmgc)
z<-est/se
p2<-2*pnorm(abs(z),lower.tail=FALSE)
plot(-log10(df$pv),-log10(p2))
df$pvmg<-p2

####################################
##descipritives of p-values
mean(df$pvmg<.05)
sum(df$pvmg<.05)
dim(df)

by(df$pvmg,df$group,function(x) mean(x<.05))



L<-split(df,df$fn)
nn<-sapply(L,nrow)
df<-merge(df,data.frame(fn=names(nn),nitem=nn))
alpha<-.05/df$nitem
nrow(df)
sum(df$pvmg<.05)
sum(df$pvmg<alpha)

test<-df$pvmg<alpha
table(by(test,df$fn,sum))

####################################
##figure
pdf("~/Dropbox/Apps/Overleaf/DIF Education RCTs/pvalues.pdf",width=7.5,height=3.3)
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.75,4))
layout(matrix(c(1,3,4,2,3,4),nrow=2,ncol=3,byrow=TRUE))
##
hist(df$del,breaks=40,freq=FALSE,xlab=expression(beta[j]),
     col='gray',main='')
mtext(side=3,adj=0,line=0,'A')
hist(df$pvmg,xlim=c(0,1),breaks=40,freq=FALSE,xlab="P-value",
     col='gray',main='')
abline(v=.05,col='red')
mtext(side=3,adj=0,line=0,'B')

##
pf2<-function(df,yl,topnm) {
    yy<-seq(0,1,length.out=2+length(df$pvmg))
    yy<-yy[-c(1,length(yy))]
    plot(NULL,xlab="-log10(p), Uniform",ylab="-log10(p), empircal",
         ylim=yl,
         xlim=c(0,max(-log10(yy))),
         bty='n'
         )
    abline(0,1)
    pf<-function(df,...) {
        xx<-sort(df$pvmg)
        yy<-seq(0,1,length.out=2+length(xx))
        yy<-yy[-c(1,length(yy))]
        points(-log10(yy),-log10(xx),...)
    }
    cols<-c('blue','red')
    L<-split(df,df$group)
    for (i in 1:length(L)) pf(L[[i]],col=cols[i],pch=19,cex=.5)
    legend("topleft",bty='n',fill=cols,names(L),title=topnm)
}
yl<-c(0,max(-log10(df$pvmg)))
pf2(df[df$del<0,],yl,"In favor of Control")
mtext(side=3,adj=0,line=0,'C')
pf2(df[df$del>=0,],yl,"In favor of Treatment")
mtext(side=3,adj=0,line=0,'D')
dev.off()



