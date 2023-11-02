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


pdf("~/Dropbox/Apps/Overleaf/DIF Education RCTs/box.pdf",width=7,height=3.3)
layout(matrix(c(1,2,2,2),nrow=1))
par(mgp=c(2,1,0),mar=c(3,4,1,1),oma=rep(.5,4))
hist(z0$r,xlab="R",ylab="",main='')
mtext(side=3,adj=0,line=0,'A')
abline(v=mean(z0$r))

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
mtext(side=3,adj=0,line=0,'B')
dev.off()
