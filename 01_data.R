##First step is to produce standardized item-response datasets. 
bigfun<-function(fn) {
    #Load packages
    library(readstata13)
    procfun<-function(df, fn) {
        df<-as.data.frame(df)
        tr<-list()
        #tr$test<-'READ' #not sure how we want to assign a name or RCT--can we input the 'file' that is being read into the larger function?
        tr$rct<-df$rct[1]
        tr$domain<- df$domain[1]
        tr$grade<-as.character(unique(df$grade))
        ##get responses
        ii<-grep("^test",names(df))
        resp<-apply(df[,ii],2,as.numeric)
        resp[is.na(resp)]<-0  #treat missing as incorrect
        nn<-apply(resp,2,
                  function(x) length(unique(x[!is.na(x)])) #length of response (0 or 1) -> length(c(0,1)) =2
                  )
        resp<-as.matrix(resp[,nn==2]) #select Dischotomous items
        ##
        cmr<-colMeans(resp)
        test<-cmr<.02 | cmr>.98
        resp<-resp[,!test,drop=FALSE]
        if (nrow(resp)>50 && ncol(resp)>=4) {
            if (nrow(resp)!=length(df$an_treat)) stop("problem")
            return(list(info=tr,resp=resp,treat=df$an_treat))
        } else return(NULL)
    }
    #test code
    rawData <-read.dta13(paste(fn,'.dta',sep=''))
    #Score missing as incorrect
    rawData<- rawData[!is.na(rawData$grade),]
    #Subset data
    rawData$grade<-as.factor(rawData$grade)
    grades<-c(levels(rawData$grade))
    dfs<- list()
    for (i in grades){
        z<- rawData[rawData$grade==i,]
        tr_onegrade <- procfun(z, fn)
        if (sum(is.na(tr_onegrade$dif))==0) {  
            tmp<- tr_onegrade  #i as a character to name one list in dfs
            dfs[[as.character(i)]]<-tmp
        }
    }
    dfs
}
Chinafilenames<-c("SIL3","TP4P1","TP4P2",
                  #"TFF1", https://domingue-lab.slack.com/archives/G01BLPY7L5S/p1677683688572599
                  "READ", "READ2", "OCAL2017", "OCAL2021", "TI3_8", "NTTP")
JPALfilenames <- c(#"2_math",  "2_literacy",
    "46_math",  "46_literacy",  "46_literacy_oral",  "53_parental_math",  "53_parental_literacy",  "84_teachers_math1",  "84_teachers_math2",  "84_teachers_literacy1",  "84_teachers_literacy2",  "GIRLSEDU_math",  "GIRLSEDU_literacy",  "132_lit_swahili.1",  "132_lit_swahili.2",  "132_lit_swahili.3",  "132_lit_english.1",  "132_lit_english.2",  "132_lit_english.3",  "132_math.1",  "132_math.2",  "132_math.3",
    #"156_bonuspay_math" , https://domingue-lab.slack.com/archives/G01BLPY7L5S/p1677683688572599
    "161_finedu_2to6_math",  "161_finedu_7to8_math",  "161_finedu_2to6_literacy",  "161_finedu_7to8_literacy")
filenames<-c(JPALfilenames,Chinafilenames)

wd<-'/home/bd/Dropbox/projects/measurement_rcts/01_processed_data'
setwd(wd)
output<-list()
for (fn in filenames) {
    z<-bigfun(fn)
    if (fn %in% Chinafilenames) group<-"REAP" else group<-"JPAL"
    for (ii in 1:length(z)) {
        y<-z[[ii]]
        y$info$group<-group
        fn.out<-paste('/home/bd/Dropbox/projects/measurement_rcts/02_itemresponse_data/',fn,'__',ii,'.Rdata',sep='')
        save(y,file=fn.out)
    }
}

#############################################################################
##Now take item-responses dataset and produce item-level data across datasets (for one final item-level dataset)

f<-function(fn) {
    load(fn)
    for (i in 1:length(y)) assign(names(y)[i],y[[i]])
    tr<-list()
    ##dif
    library(difR)
    r <- difLogistic(resp, group=treat, focal.name = 1,alpha=1,type='udif')
    tr$dif<-r$logitPar[,3] #getting the GROUP coefficient only
    tr$treat <- ifelse(tr$dif>0, "Treatment", "Control") #does the DIF favor treatment or control
    tr$pv<- r$p.value
    ##other stuff
    tr$nresp<-apply(resp,2,function(x) sum(!is.na(x)))
    tr$difficulty<-colMeans(resp,na.rm=TRUE) #measure of difficulty from CTT 
    rm<-rowMeans(resp,na.rm=TRUE)
    corr<-numeric()
    for (i in 1:ncol(resp)) corr[i]<-cor(rm,resp[,i],use='p')
    tr$disc<-corr #item discrim? not sure
    ##
    ##
    rs<-rowMeans(resp,na.rm=TRUE)
    treat<-treat
    lpm<-numeric()
    for (jj in 1:ncol(resp)) lpm[jj]<-coef(lm(resp[,jj]~rs+treat))[3]
    tr$lpm<-lpm
    ##
    df<-data.frame(tr)
    df$item<-1:nrow(df)
    for (nm in names(info)) df[[nm]]<-info[[nm]]
    df$fn<-fn
    return(df)
}

setwd('/home/bd/Dropbox/projects/measurement_rcts/02_itemresponse_data/')
lf<-list.files()
df<-lapply(lf,f)
df<-data.frame(do.call("rbind",df))


ff<-function(fn) {
    load(fn)
    for (i in 1:length(y)) assign(names(y)[i],y[[i]])
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
    data.frame(fn=fn,item=1:nrow(z),a=apar[!test],difmgc=z[,1],difmgt=z[,2],difmgc.se=se[,1],difmct.se=se[,2])
}

setwd('/home/bd/Dropbox/projects/measurement_rcts/02_itemresponse_data/')
lf<-list.files()
library(parallel)
z<-mclapply(lf,ff,mc.cores=3)
z<-do.call("rbind",z)
z<-data.frame(z)

df<-merge(df,z)

#############################################################################
##3. cleaning up and getting some test-level stuff
f<-function(fn) {
    load(paste0('/home/bd/Dropbox/projects/measurement_rcts/02_itemresponse_data/',fn))
    for (i in 1:length(y)) assign(names(y)[i],y[[i]])
    th<-rowSums(resp,na.rm=TRUE)
    th<-(th-mean(th))/sd(th)
    est<-(mean(th[treat==1])-mean(th[treat==0]))
    tt<-t.test(th[treat==1],th[treat==0])
    cil<-tt$conf.int[1]
    ciu<-tt$conf.int[2]
    effect.sig<-tt$p.value
    c(fn=fn,es=est,mt=mean(treat),cil=cil,ciu=ciu,effect.sig=effect.sig)
} 
lf<-list.files(path='/home/bd/Dropbox/projects/measurement_rcts/02_itemresponse_data')
x<-lapply(lf,f)
x<-do.call("rbind",x)
x<-data.frame(x)
for (i in 2:ncol(x)) x[,i]<-as.numeric(x[,i])
df<-merge(df,x)

save(df,file='/home/bd/Dropbox/projects/measurement_rcts/bdwd/df.Rdata')






