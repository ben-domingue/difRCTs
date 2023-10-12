simfun<-function(N,es,item.es,ni=20) {
    gr<-rbinom(N,1,.5)
    th<-rnorm(N,mean=ifelse(gr==1,es,0))
    df<-data.frame(treat=gr,th=th)
    resp<-list()
    b<-rnorm(ni)
    for (i in 1:ni) {
        if (i==1) bb<-b[i]+item.es*df$treat else bb<-b[i]
        p<-1/(1+exp(-(th-bb)))
        resp[[i]]<-rbinom(N,1,p)
    }
    resp<-data.frame(do.call("cbind",resp))
    names(resp)<-paste("item",1:ncol(resp))
    resp$treat<-df$treat
    resp
}

LL<-list()
for (N in c(1000,2500,5000)) {
    for (item.es in seq(0,.5,length.out=5)) {
        out<-list()
        for (iii in 1:250) {
            print(iii)
            resp<-simfun(N,es=0,item.es=item.es)
            treat<-resp$treat
            resp$treat<-NULL
            library(mirt)
            models <- paste('F1 = 1-',ncol(resp),"\nCONSTRAIN=(1-",ncol(resp),",a1)","\nCONSTRAINB=(1-",ncol(resp),",a1)",sep='')
            models<-mirt.model(models)
            resp<-data.frame(resp)
            group<-paste("D",treat,sep='')
            mod<-multipleGroup(resp,model=models,group=group
                              ,SE=TRUE
                               #invariance=c('slopes', 'intercepts', 'free_var','free_means')
                               )
            f<-function(x) {
                x<-x[-length(x)]
                do.call("rbind",x)
            }
            L<-lapply(coef(mod,printSE=TRUE),f)
            apar<-L[[1]][,1]
            z<-cbind(L[[1]][,2],L[[2]][,2])
            test<-grepl("SE",rownames(z))
            se<-z[test,]
            z<-z[!test,]
            se<-apply(se,1,function(x) sqrt(x[1]^2+x[2]^2))
            z<-(z[,2]-z[,1])/se
            out[[iii]]<-list(N,item.es,z)
        }
        LL[[paste(N,item.es)]]<-out
    }
}

LL<-do.call("c",LL)
f<-function(x) c(x[[1]],x[[2]],x[[3]][1],max(abs(x[[3]][-1])))
z<-lapply(LL,f)
z<-data.frame(do.call("rbind",z))

L<-split(z,z[,1:2])
z<-lapply(L,function(x) c(unique(x[,1]),unique(x[,2]),mean(abs(x[,3])>1.96)))
z<-data.frame(do.call("rbind",z))
zz<-split(z,z[,1])

pdf("/home/bd/Dropbox/Apps/Overleaf/DIF Education RCTs/power.pdf",width=4,height=3.3)
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
plot(NULL,xlim=range(z[,2]),ylim=0:1,xlab=expression(beta[1]),ylab="Power")
lines(zz[[1]][,2:3],lwd=2)
lines(zz[[2]][,2:3],col='red',lwd=2)
lines(zz[[3]][,2:3],col='blue',lwd=2)
legend("bottomright",bty='n',fill=c("black","red","blue"),names(zz),title="N")
abline(h=.8,lty=2)
dev.off()



##power of 0.3
th<-rnorm(1000000)
f<-function(th,b) {
    p<-1/(1+exp(-1*(th-b)))
    z<-rbinom(length(th),1,p)
    mean(z)
}
f(th,0)
f(th+.3,0)
