u<-c("u1","u2")
v<-c("v1","v2","v3")
w<-c("w1","w2","w3")
matr.u0<-paste("u0",outer(v,w,paste,sep=","),sep=",")
dim(matr.u0)<-c(3,3)
matr.u1<-paste("u1",outer(v,w,paste,sep=","),sep=",")
dim(matr.u1)<-c(3,3)
matr.u0

matr.u1

dataPath = "D:/datascience/bayesian/wk1/"

data3way<-read.csv(file=paste(dataPath,"3WayData.csv",sep="/"))
data3way = read.csv(file.choose())

head(data3way)

mat.u0<-table(subset(data3way,u==0)[,1],subset(data3way,u==0)[,2])
mat.u1<-table(subset(data3way,u==1)[,1],subset(data3way,u==1)[,2])
mat.u0

mat.u1

idx.v1<-data3way$v==1
idx.w1<-data3way$w==1
idx.u1<-data3way$u==1
sum(idx.v1*idx.w1*idx.u1) #element (1,1) of mat.u1

idx.v2<-data3way$v==2
sum(idx.v2*idx.w1*idx.u1) #element (1,2) of mat.u1

idx.w2<-data3way$w==2
sum(idx.v1*idx.w2*idx.u1) #element (2,1) of mat.u1

colnames(mat.u1)<-colnames(mat.u0)<-c("v1","v2","v3")
rownames(mat.u1)<-rownames(mat.u0)<-c("w1","w2","w3")

data3way.array<-array(rep(NA,18),dim=c(3,3,2),dimnames=list(paste("w",1:3,sep=""),
                                                            paste("v",1:3,sep=""),
                                                            paste("u",0:1,sep="")))
data3way.array[,,1]<-mat.u0
data3way.array[,,2]<-mat.u1
data3way.array

N<-sum(data3way.array)
(data3way.array.p<-data3way.array/N)

uMarginal = apply(data3way.array.p,FUN = sum,MARGIN = 3)
vMarginal = apply(data3way.array.p,FUN = sum,MARGIN = 2)
wMarginal = apply(data3way.array.p,FUN = sum,MARGIN = 1)

uMarginal
vMarginal
wMarginal

### Conditional

data3way.array.p

(cond.v.w.given.u1 = matrix(data = data3way.array.p[10:18]/uMarginal["u1"],ncol = 3))

# all vs
(cond.v.given.u1 = apply(cond.v.w.given.u1,MARGIN = 2,FUN = sum))
cond.v.given.u1
sum(cond.v.given.u1)

cond.v.given.u1/uMarginal["u1"]

# last

cond.w.given.u1.v2

# p(w,v,u) / p(v=2, w = 1)
# p(w,v,u) / p(w=1 | v=2) * p(v=2)

.09/((.07+.09)/.35)*.35

# simulate

(pCond.v.given.u0<-c(.7,.2,.1))
(pCond.v.given.u1<-c(.1,.2,.7))

p.given.u0.v1<-c(.3,.3,.4)
p.given.u0.v2<-c(.5,.3,.2)
p.given.u0.v3<-c(.6,.2,.2)
p.given.u1.v1<-c(.2,.3,.5)
p.given.u1.v2<-c(.2,.2,.6)
p.given.u1.v3<-c(.1,.7,.2)

sample = 500
set.seed(11)
# u
(sim.u = rbinom(n = 500,prob = .45,size = 1))

# v

set.seed(11)

sim.u<-rbinom(500,size=1,p=.45)
sim.v<-sapply(sim.u,function(z) sample(1:3,1,replace=T,
                                       prob=ifelse(rep(z==1,3),pCond.v.given.u1,pCond.v.given.u0)))
sim.w<-apply(cbind(sim.u,sim.v),1,
             function(z) sample(1:3,1,replace=T,prob=switch(3*z[1]+z[2],p.given.u0.v1,p.given.u0.v2,
                                                            p.given.u0.v3,p.given.u1.v1,p.given.u1.v2,
                                                            p.given.u1.v3)))

head(cbind(sim.w,sim.v,sim.u),25)
