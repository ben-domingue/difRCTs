simfun<-function(N=5000, #number of people
                 es=.2, #effect size of intervention
                 g2sd=1,
                 item.offsets=NULL, #item-specific offsetse
                 ni=NULL #number of items
                 ) {
    if (is.null(ni)) ni<-20
    if (is.null(item.offsets)) item.offsets<-rep(0,ni)
    ##
    gr<-rbinom(N,1,.5)
    th<-rnorm(N,mean=ifelse(gr==1,es,0),sd=ifelse(gr==1,g2sd,1))
    df<-data.frame(treat=gr,th=th)
    resp<-list()
    b<-rnorm(ni)
    for (i in 1:ni) {
        bb<-b[i]+item.offsets[i]*df$treat 
        p<-1/(1+exp(-(th+bb)))
        resp[[i]]<-rbinom(N,1,p)
    }
    resp<-data.frame(do.call("cbind",resp))
    names(resp)<-paste("item",1:ncol(resp))
    resp$treat<-df$treat
    resp
}

analyze<-function(resp) {
    library(mirt)
    resp<-data.frame(resp)
    group<-paste("D",resp$treat,sep='')
    ii<-grep("treat",names(resp))
    resp<-resp[,-ii]
    ##
    models <- paste('F1 = 1-',ncol(resp),"\nCONSTRAIN=(1-",ncol(resp),",a1)","\nCONSTRAINB=(1-",ncol(resp),",a1)",sep='')
    models<-mirt.model(models)
    mod<-multipleGroup(resp,
                       model=models,
                       group=group,
                       SE=TRUE,
                       invariance=c('free_var')
                       )
    coef(mod,simplify=TRUE)
}


resp<-simfun(g2sd=1.5) #no item offsets
out<-analyze(resp)
delta<-out$D1$items[,2]-out$D0$items[,2]
mean(delta) #estimate of treatment effect
plot(out$D0$items[,2],out$D1$items[,2],type='n',xlab='control',ylab='treatment')
text(out$D0$items[,2],out$D1$items[,2],1:nrow(out$D0$items))
sqrt(out$D1$cov) #sD for group 2

##first two items in favor of treatment
resp<-simfun(g2sd=1.5,item.offsets=c(.5,.5,rep(0,18)))
out<-analyze(resp)
delta<-out$D1$items[,2]-out$D0$items[,2]
mean(delta) #estimate of treatment effect
plot(out$D0$items[,2],out$D1$items[,2],type='n',xlab='control',ylab='treatment')
text(out$D0$items[,2],out$D1$items[,2],1:nrow(out$D0$items),col=c("red","red",rep("gray",18)))
sqrt(out$D1$cov) #sD for group 2

#first two items in favof of treatment, second two in favor of control
resp<-simfun(g2sd=1.5,item.offsets=c(.5,-.5,rep(0,18)))
out<-analyze(resp)
delta<-out$D1$items[,2]-out$D0$items[,2]
mean(delta) #estimate of treatment effect
plot(out$D0$items[,2],out$D1$items[,2],type='n',xlab='control',ylab='treatment')
text(out$D0$items[,2],out$D1$items[,2],1:nrow(out$D0$items),col=c("red","red",rep("gray",18)))
sqrt(out$D1$cov) #sD for group 2

#first ten items in favof of treatment, second ten in favor of control
resp<-simfun(g2sd=1.5,item.offsets=c(rep(.25,10),rep(-.25,10)))
out<-analyze(resp)
delta<-out$D1$items[,2]-out$D0$items[,2]
mean(delta) #estimate of treatment effect
plot(out$D0$items[,2],out$D1$items[,2],type='n',xlab='control',ylab='treatment')
text(out$D0$items[,2],out$D1$items[,2],1:nrow(out$D0$items))
sqrt(out$D1$cov) #sD for group 2
