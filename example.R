simfun<-function(N=5000, #number of people
                 es=.2, #effect size of intervention
                 item.offsets=NULL, #item-specific offsetse
                 ni=NULL #number of items
                 ) {
    if (is.null(ni)) ni<-20
    if (is.null(item.offsets)) item.offsets<-rep(0,ni)
    ##
    gr<-rbinom(N,1,.5)
    th<-rnorm(N,mean=ifelse(gr==1,es,0))
    df<-data.frame(treat=gr,th=th)
    resp<-list()
    b<-rnorm(ni)
    for (i in 1:ni) {
        if (i==1) bb<-b[i]+item.offsets[i]*df$treat else bb<-b[i]
        p<-1/(1+exp(-(th-bb)))
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
    z<- z*-1 #due to mirt's easiness parametrization
    mu<--1*mean(z[,2]-z[,1])
    data.frame(treatment.effect=mu,control.group=z[,1],offset=z[,2]-(z[,1]-mu))
}


resp<-simfun() #no item offsets
output<-analyze(resp)
##effect size (truth was 0.2)
unique(output$treatment.effect)
##item-level offsets centered around zero after we account for treatment effect
summary(output$offset)

resp<-simfun(item.offsets=c(.5,rep(0,19)),ni=20) #the first item is especially sensitive to treatment
output<-analyze(resp)
##effect size (truth was 0.2)
unique(output$treatment.effect)
##item-level offsets centered around zero after we account for treatment effect
output$offset #note the first item's offset


resp<-simfun(item.offsets=c(-.5,rep(0,19)),ni=20) #the first item is especially insensitive to treatment
output<-analyze(resp)
##effect size (truth was 0.2)
unique(output$treatment.effect)
##item-level offsets centered around zero after we account for treatment effect
output$offset #note the first item's offset
