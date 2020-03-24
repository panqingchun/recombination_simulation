rm(list=ls())
chrl<-c(307041676, 244442276, 235667822, 246994605, 223902239, 174033109, 182381541,181122619, 159769782, 150982312)
chrll<-round(chrl/1000000,0)
chrtwo<-chrll*8

pyy1<-function(pjj1){  
pww<-1:pjj1/8
pww1<-pww*1000000
pww2<-c(0,pww1[1:(length(pww1)-1)]+1)
pww3<-cbind(pww2,pww1)
pww3
}

pfi<-read.table("map_data.txt",head=T,sep="\t")
phh1<-NULL; phh2<-NULL;hhp1<-NULL
for (i in 1:10){
phh1[[i]]<-pyy1(chrtwo[i])
phh2[[i]]<-pfi[which(pfi[,3]==i),]
hhp1[[i]]<-cbind(i,phh1[[i]])
}

psss<-function(puu1,puu2){

phh3<-list();phh4<-NULL;
for (i in 1:nrow(puu1)){
phh3[[i]]<-intersect(which(puu2[,4]>puu1[i,1]),which(puu2[,4]<puu1[i,2]))
if(length(phh3[[i]])>0){
phh4[i]<-sum(puu2[phh3[[i]],6])
}else{phh4[i]=0}
}

phh5<-cbind(puu1,phh4)
phh6<-cbind(1:nrow(phh5),phh4)

phh6
}

pjjone<-NULL
for (i in 1:10){
pjjone[[i]]<-psss(phh1[[i]],phh2[[i]])
}

final_pos<-do.call(rbind,hhp1)

head(final_pos)


hh1<-NULL; hh2<-NULL;hh3<-NULL;ttt1<-NULL;ttt2<-NULL
for  (i in 1:10){
hh1[[i]]<-1:chrtwo[i]
hh2[[i]]<-rep(2,chrtwo[i])
hh3[[i]]<-rep(1,chrtwo[i])
ttt1[[i]]<-cbind(hh1[[i]],hh2[[i]],hh3[[i]])
ttt2[[i]]<-cbind(i,ttt1[[i]],pjjone[[i]][,2])
}
ttt3<-do.call(rbind,ttt2)

jj<-cbind(c(0:3),c(0.16,0.39,0.32,0.13)) 

sss2<-function(niu){ 
 
for (i in 1:3000)
{
www<-sample(niu[2:(nrow(niu)-2),1],2,pro=niu[2:(nrow(niu)-2),4]) 
www1<-sort(www)
ww1<-(www1[2]-www1[1])
ww2<-NULL
if(ww1>10){  
ww2<-www1
}
break
}
ww2   
}

sss3<-function(niu1){ 
for (i in 1:3000)
{
www<-sample(niu1[2:(nrow(niu1)-2),1],3,pro=niu1[2:(nrow(niu1)-2),4])  
www1<-sort(www)
ww1<-(www1[2]-www1[1])
ww1_2<-(www1[3]-www1[2])
ww3<-NULL
if (ww1>10 && ww1_2>10){  
ww3<-www1
}
break
}
ww3    
}

uuu<-function(rrr){

oo<-sample(jj[,1],1,pro=jj[,2])
##oo<-1
if (oo==0){ 
se<-sample(c(1,2),1)
fu<-rrr[,1+se] 
}

if(oo==1){  
se<-sample(c(1,2),1)
fk_1<-sample(rrr[,1][2:(nrow(rrr)-1)],1,pro=rrr[2:(nrow(rrr)-1),4])
if (se==1){
fu<-c(rrr[1:fk_1,2],rrr[(fk_1+1):nrow(rrr),3])}else{
fu<-c(rrr[1:fk_1,3],rrr[(fk_1+1):nrow(rrr),2])
}
}
if(oo==2){ 
pp1<-NULL;pp2<-NULL
for (i in 1:100){
pp1[[i]]<-sss2(rrr) 
}
pp2<-do.call(rbind,pp1)
pp3<-pp2[sample(1:nrow(pp2),1),] 
se<-sample(c(1,2),1)
if (se==1){
fu<-c(rrr[1:pp3[1],2],rrr[(pp3[1]+1):pp3[2],3],rrr[(pp3[2]+1):nrow(rrr),2])}else{
fu<-c(rrr[1:pp3[1],3],rrr[(pp3[1]+1):pp3[2],2],rrr[(pp3[2]+1):nrow(rrr),3])    
}
}

if(oo==3){ 
pp1_1<-NULL;pp2_1<-NULL
for (i in 1:100){
pp1_1[[i]]<-sss3(rrr)  
}
pp2_1<-do.call(rbind,pp1_1)
pp3_1<-pp2_1[sample(1:nrow(pp2_1),1),]
se<-sample(c(1,2),1)
if (se==1){
fu<-c(rrr[1:pp3_1[1],2],rrr[(pp3_1[1]+1):pp3_1[2],3],rrr[(pp3_1[2]+1):pp3_1[3],2],rrr[(pp3_1[3]+1):nrow(rrr),3])}else{
fu<-c(rrr[1:pp3_1[1],3],rrr[(pp3_1[1]+1):pp3_1[2],2],rrr[(pp3_1[2]+1):pp3_1[3],3],rrr[(pp3_1[3]+1):nrow(rrr),2])
}
}
fu_1<-cbind(rrr[,1],fu,rrr[,4])  
fu_1 
}

lilinone<-function(tttt){
lele<-tttt  
le_1<-NULL; le_2<-NULL
for (i in 1:10){
le_1[[i]]<-lele[which(lele[,1]==i),]
le_2[[i]]<-le_1[[i]][,2:5]
}
geng<-NULL;geng1<-NULL;geng2<-NULL
for (i in 1:10){
geng[[i]]<-cbind(i,uuu(le_2[[i]])) 
}
jby<-do.call(rbind,geng)
jby
}

nayiye<-function(nanana){
DH_200<-NULL;
for (i in 1:200){
DH_200[[i]]<-lilinone(nanana) 
}
DH_re<-do.call(cbind,DH_200)
DH_re
}

fiss<-nayiye(ttt3)
id_yy<-NULL
for (i in 1:200){
id_yy[i]<-4*i-1
}

fiss_1<-cbind(final_pos,fiss[,id_yy])

issi<-fiss_1[,4:ncol(fiss_1)]
issi_1<-apply(issi,1,table)
iss_rate<-issi_1[1]/200

tjone<-function(fiii){
iss<-NULL
iss<-which(fiii[1:(length(fiii)-1)]!=fiii[2:length(fiii)])
isss<-length(iss)
isss
}

pdss<-cbind(fiss_1[,1],issi)
pdd<-function(pdd11){
tj<-NULL;tjj<-NULL
for (i in 1:10){
tj[[i]]<-pdd11[which(pdd11[,1]==i),]
tjj[[i]]<-tjone(tj[[i]][,2])
}
tjsu<-sum(unlist(tjj))
tjsu
}
kktj<-NULL
for (i in 1:200){
kktj[[i]]<-pdd(pdss[,c(1,i+1)])
}
mean(kktj) 

fiii<-fiss_1; tete<-NULL;tete1<-NULL
for (i in 1:10){
tete[[i]]<-fiii[which(fiii[,1]==i),]
tete1[[i]]<-tete[[i]][,4:ncol(tete[[i]])]
}

finnn<-function(ninini,ninini1){
fiii_1<-ninini;fififi<-ninini1;
iss<-NULL
for (i in 1:nrow(fiii_1)){
iss[i]<-paste(fiii_1[i,],collapse="")
}
issout<-NULL
for (i in 1:(nrow(fiii_1)-1)){
issout[i]<-(iss[i]==iss[i+1])
}
issout1<-which(issout==FALSE)
issout2<-cbind(c(1,issout1),c(issout1,nrow(fiii_1)))
issout3<-fififi[issout2[,2],3]-fififi[issout2[,1],2]
issout3
}
leng_re<-NULL
for (i in 1:10){
leng_re[[i]]<-finnn(tete1[[i]],tete[[i]])
}
leng_re1<-unlist(leng_re)
mean(leng_re1)



