load("~/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")
df$del<-df$difmgt-df$difmgc

summary(df$lpm) #lpm dif estimates

###get new p-values
se<-sqrt(df$difmgc.se^2+df$difmct.se^2)
est<-(df$difmgt-df$difmgc)
z<-est/se
p2<-2*pnorm(abs(z),lower.tail=FALSE)
#plot(-log10(df$pv),-log10(p2))
df$pvmg<-p2

####################################
mean(df$pvmg<.05)
sum(df$pvmg<.05)
dim(df)

by(df$pvmg,df$group,function(x) mean(x<.05))

##p value
##
pdf("/tmp/fig2.pdf",width=4,height=3)
par(mgp=c(2,1,0),oma=rep(.5,4),mar=c(3,3,1,1))
hist(df$pvmg,xlim=c(0,1),breaks=40,freq=FALSE,xlab="P-value",
     col='gray',main='')
abline(v=.05,col='red')
dev.off()


##########################################################################
load("~/Dropbox/projects/measurement_rcts/bdwd/df.Rdata")
df$del<-df$difmgt-df$difmgc

f<-function(df) { #just the ratios
    b0<-df$difmgc
    del<-df$difmgt-df$difmgc
    sd(del)/sd(b0)
}
L<-split(df,df$fn)
z0<-sapply(L,f)
z0<-data.frame(fn=names(z0),r=z0)

summary(z0$r)

z0[grep("^SIL",z0$fn),]


pdf("/tmp/fig4.pdf",width=4,height=3)
par(mgp=c(2,1,0),oma=rep(.5,4),mar=c(3,3.5,1,1))

tmp<-df[,c("fn","es","effect.sig")]
tmp<-tmp[!duplicated(tmp$fn),]
z0<-merge(z0,tmp)
tmp<-z0[z0$effect.sig>.05,]
tmp<-tmp[order(tmp$r),]
z0[z0$fn=="SIL3__2.Rdata",]
z0[z0$fn=="132_lit_english.1__1.Rdata",]

#df<-df[df$effect.sig>.05,]
col<-rep('gray',nrow(df))
col<-ifelse(df$fn=="SIL3__2.Rdata",'blue',col)
######col<-ifelse(df$fn=="GIRLSEDU_math__7.Rdata",'red',col)
col<-ifelse(df$fn=="132_lit_english.1__1.Rdata",'red',col)
col<-ifelse(df$effect.sig<.05,'black',col)
plot(df$es,df$del,cex=ifelse(col %in% c("blue","red"),1,.5),pch=19,col=col,xlab='Effect Size',ylab=expression(beta[j]))
test<-col %in% c("blue","red")
points(df$es[test],df$del[test],cex=1,pch=19,col=col[test])
dev.off()
