#number of tests (subject-grade combinations), the total number of items and range of items-per-test, and the total sample size and range of sample-size-per-test (after your exclusion criteria)

load("/home/bd/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")

L<-split(df,df$fn)
f<-function(x) {
    rct<-unique(x$fn)
    dom<-unique(x$domain)
    gr<-unique(x$grade)
    ni<-nrow(x)
    np<-unique(x$nresp)
    c(rct,dom,gr,ni,np)
}
tab<-lapply(L,f)
tab<-do.call("rbind",tab)
tab<-data.frame(tab)

library(xtable)
print(xtable(tab),include.rownames=FALSE)
