set.seed(101010320)
load("~/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")
df$del<-df$difmgt-df$difmgc

f<-function(df) { #just the ratios
    b0<-df$difmgc
    del<-df$difmgt-df$difmgc
    sd(del)/sd(b0)
}
L<-split(df,df$fn)
z0<-sapply(L,f)
summary(z0)

z0<-data.frame(fn=names(z0),r=z0)

z0[grep("^SIL",z0$fn),]

z0[z0$fn=="SIL3__2.Rdata",]
z0[z0$fn=="GIRLSEDU_math__7.Rdata",]


f<-function(x,niter=100) {
    if (x$effect.sig[1]<.05) {
        N<-unique(x$nresp)
        ni<-nrow(x)
        es0<-c(unique(x$es),unique(x$cil),unique(x$ciu))
        ##
        offsets<-x$del
        den<-density(offsets)
        dx<-den$x
        dy<-cumsum(den$y)
        dy<-dy/max(dy)
        ##
        out<-list()
        for (i in 1:niter) {
            pvalues<-runif(length(offsets))
            oo<-outer(dy,pvalues,'-')
            ii<-apply(oo,2,function(x) which.min(abs(x)))    
            new.offsets<-dx[ii]
            gr<-rbinom(N,1,unique(x$mt))
            th<-rnorm(N,mean=0,sd=ifelse(gr==0,1,unique(x$treat.var)))
            df<-data.frame(treat=gr,th=th)
            resp<-list()
            #b<-rnorm(ni)
            a<-x$a
            b<-x$difmgc
            for (j in 1:ni) {
                p<-1/(1+exp(-(a[j]*th+b[j]+df$treat*new.offsets[j])))
                resp[[j]]<-rbinom(N,1,p)
            }
            resp<-do.call("cbind",resp)
            #library(mirt)
            #m<-mirt(data.frame(resp),1)
            #th<-fscores(m)
            th<-rowSums(resp)
            th<-(th-mean(th))/sd(th)
            est<-(mean(th[df$treat==1])-mean(th[df$treat==0]))
            tt<-t.test(th[df$treat==1],th[df$treat==0])
            pv<-tt$p.value
            out[[i]]<-c(est,pv)
        }
        z<-do.call("rbind",out)
        qu<-quantile(z[,1],c(.025,.975))
        c(es0,qu,mean(z[,1]))
    } else {
        NULL
    }
}

L<-split(df,df$fn)
library(parallel)
z<-mclapply(L,f,mc.cores=3,niter=5000)
z<-do.call("rbind",z)
z<-data.frame(z)
names(z)<-c("es","cil","ciu","qu2.5","qu97.5","mean")
yy<-merge(z,z0,by.x=0,by.y='fn')
rownames(yy)<-yy[,1]
yy[,1]<-NULL

z<-yy
z<-z[order(z$es),]
pdf("~/Dropbox/Apps/Overleaf/DIF Education RCTs/ratios.pdf",width=7,height=3.3)
layout(matrix(c(1,2,3,3,3,3),nrow=2,ncol=,byrow=FALSE))
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
#
plot(z$es,z$mean,pch=19,col='gray',xlab="Effect",ylab="Simulation effect",cex=1)
mtext(side=3,adj=0,line=0,'A')
abline(0,1)
#
d1<-z$qu97.5-z$qu2.5
d0<-z$ciu-z$cil
plot(z$r,d1,pch=19,col='gray',xlab="R",ylab="Simulated CI",cex=1)
mtext(side=3,adj=0,line=0,'B')
r<-cor(z$r,d1)
legend("bottomright",bty='n',legend=paste0('r=',round(r,digits=3)))
#
plot(NULL,ylim=c(-.5,.5),xlim=c(1,nrow(z)),xlab='',ylab="Effect")
mtext(side=3,adj=0,line=0,'C')
abline(h=0)
ss2<-0<z$qu2.5 | 0>z$qu97.5
cols<-ifelse(!ss2,'red','black')
for (i in 1:nrow(z)) {
    points(i,z$es[i],pch=19,cex=.5)
    segments(i,z$qu2.5[i],i,z$qu97.5[i],col=cols[i],lwd=2)
}
dev.off()


summary(d1/d0)
   ## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   ## 1.00    1.11    1.18    1.21    1.23    1.86 

ss1<-0<z[,2] | 0>z[,3]
ss2<-0<z[,4] | 0>z[,5]
table(ss1,ss2)

##       ss2
## ss1    FALSE TRUE
##   TRUE     8   21

     
