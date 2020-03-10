##### The script is to simulate the genotype of 200 MAGIC materials of 8 eight with crossing-selfed method, and evaluate the number of recombination event and the size of recombinant fragments.
##### Copyright Qingchun Pan, published on March 4, 2020

rm(list=ls())

#####  Define the physical length of different chromosome

chrl<-c(307041676, 244442276, 235667822, 246994605, 223902239, 174033109, 182381541,181122619, 159769782, 150982312)

#####  Divide into 125 kb segmentation of each chrosmome

chrll<-round(chrl/1000000,0)
chrtwo<-chrll*8

pyy1<-function(pjj1){ 
pww<-1:pjj1/8
pww1<-pww*1000000
pww2<-c(0,pww1[1:(length(pww1)-1)]+1)
pww3<-cbind(pww2,pww1)
pww3
}

##### Import recombiantion rate file based on the IBM genotype

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

#### To simulate two chromatids of 10 chromosomes in maize

hh1<-NULL; hh2<-NULL;hh3<-NULL;ttt1<-NULL;ttt2<-NULL 
for  (i in 1:10){
hh1[[i]]<-1:chrtwo[i]
hh2[[i]]<-rep(2,chrtwo[i])
hh3[[i]]<-rep(1,chrtwo[i])
ttt1[[i]]<-cbind(hh1[[i]],hh2[[i]],hh3[[i]])
ttt2[[i]]<-cbind(i,ttt1[[i]],pjjone[[i]][,2])
}
ttt3<-do.call(rbind,ttt2)

#### Define the probability of recombination event  for each generation on each chromosome 

hh1<-NULL; hh2<-NULL;hh3<-NULL;ttt1<-NULL;ttt2<-NULL  
for  (i in 1:10){
hh1[[i]]<-1:chrtwo[i]
hh2[[i]]<-rep(4,chrtwo[i])
hh3[[i]]<-rep(3,chrtwo[i])
ttt1[[i]]<-cbind(hh1[[i]],hh2[[i]],hh3[[i]])
ttt2[[i]]<-cbind(i,ttt1[[i]],pjjone[[i]][,2])
}
ttt4<-do.call(rbind,ttt2)

hh1<-NULL; hh2<-NULL;hh3<-NULL;ttt1<-NULL;ttt2<-NULL 
for  (i in 1:10){
hh1[[i]]<-1:chrtwo[i]
hh2[[i]]<-rep(6,chrtwo[i])
hh3[[i]]<-rep(5,chrtwo[i])
ttt1[[i]]<-cbind(hh1[[i]],hh2[[i]],hh3[[i]])
ttt2[[i]]<-cbind(i,ttt1[[i]],pjjone[[i]][,2])
}
ttt5<-do.call(rbind,ttt2)

hh1<-NULL; hh2<-NULL;hh3<-NULL;ttt1<-NULL;ttt2<-NULL  
for  (i in 1:10){
hh1[[i]]<-1:chrtwo[i]
hh2[[i]]<-rep(8,chrtwo[i])
hh3[[i]]<-rep(7,chrtwo[i])
ttt1[[i]]<-cbind(hh1[[i]],hh2[[i]],hh3[[i]])
ttt2[[i]]<-cbind(i,ttt1[[i]],pjjone[[i]][,2])
}
ttt6<-do.call(rbind,ttt2)


jj<-cbind(c(0:3),c(0.16,0.39,0.32,0.13)) 

#### Define the funciton of two times recombinaiton on each chromosome

sss2<-function(niu){ 
  
for (i in 1:3000)
{
www<-sample(niu[2:(nrow(niu)-2),1],2,pro=niu[2:(nrow(niu)-2),4]) 
www1<-sort(www)
ww1<-(www1[2]-www1[1])
ww2<-NULL
if(ww1>10){  ##### Threshold setting of recombination interference and linkage
ww2<-www1
}
break
}
ww2   
}

####  Define the function of three times recombination on each chromosome

sss3<-function(niu1){  
for (i in 1:3000)
{
www<-sample(niu1[2:(nrow(niu1)-2),1],3,pro=niu1[2:(nrow(niu1)-2),4])  
www1<-sort(www)
ww1<-(www1[2]-www1[1])
ww1_2<-(www1[3]-www1[2])
ww3<-NULL
if (ww1>10 && ww1_2>10){   ##### Threshold setting of recombination interference and linkage 
ww3<-www1
}
break
}
ww3   
}

#### Recombination event on chromosome

uuu<-function(rrr){
oo<-sample(jj[,1],1,pro=jj[,2])   #### Random selection of recombination times (0,1,2,3)
if (oo==0){    ###  No recombination
se<-sample(c(1,2),1)   ####  Random selection of a gamete between two parents
fu<-rrr[,1+se]  
}

if(oo==1){    ###  One time recombination
se<-sample(c(1,2),1)   #### Random selection of a gamete between two parents
fk_1<-sample(rrr[,1][2:(nrow(rrr)-1)],1,pro=rrr[2:(nrow(rrr)-1),4])
if (se==1){
fu<-c(rrr[1:fk_1,2],rrr[(fk_1+1):nrow(rrr),3])}else{
fu<-c(rrr[1:fk_1,3],rrr[(fk_1+1):nrow(rrr),2])
}
}
if(oo==2){     ### Two time recombinations
pp1<-NULL;pp2<-NULL
for (i in 1:100){
pp1[[i]]<-sss2(rrr) 
}
pp2<-do.call(rbind,pp1)
pp3<-pp2[sample(1:nrow(pp2),1),]  
se<-sample(c(1,2),1)    ####  Random selection of a gamete between two parents
if (se==1){
fu<-c(rrr[1:pp3[1],2],rrr[(pp3[1]+1):pp3[2],3],rrr[(pp3[2]+1):nrow(rrr),2])}else{
fu<-c(rrr[1:pp3[1],3],rrr[(pp3[1]+1):pp3[2],2],rrr[(pp3[2]+1):nrow(rrr),3])  
}
}

if(oo==3){     ### Two time recombinations
pp1_1<-NULL;pp2_1<-NULL
for (i in 1:100){
pp1_1[[i]]<-sss3(rrr)  

}
pp2_1<-do.call(rbind,pp1_1)
pp3_1<-pp2_1[sample(1:nrow(pp2_1),1),]
se<-sample(c(1,2),1)    ####  Random selection of a gamete between two parents
if (se==1){
fu<-c(rrr[1:pp3_1[1],2],rrr[(pp3_1[1]+1):pp3_1[2],3],rrr[(pp3_1[2]+1):pp3_1[3],2],rrr[(pp3_1[3]+1):nrow(rrr),3])}else{
fu<-c(rrr[1:pp3_1[1],3],rrr[(pp3_1[1]+1):pp3_1[2],2],rrr[(pp3_1[2]+1):pp3_1[3],3],rrr[(pp3_1[3]+1):nrow(rrr),2])
}
}
fu_1<-cbind(rrr[,1],fu,rrr[,4]) 
fu_1 
}

####  Ten chromosomes

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

####  200 lines

MAGIC<-function(M1,M2,M3,M4){
F2_12<-cbind(lilinone(M1)[,1:3],lilinone(M2)[,3:4]) #### F2 ###### 
F2_34<-cbind(lilinone(M3)[,1:3],lilinone(M4)[,3:4]) #### F2 ###### 
F3_1234<-cbind(lilinone(F2_12)[,1:3],lilinone(F2_34)[,3:4]) #### F3 ###### 
F4_1234<-cbind(lilinone(F3_1234)[,1:3],lilinone(F3_1234)[,3:4]) #### F4 ######
F5_1234<-cbind(lilinone(F4_1234)[,1:3],lilinone(F4_1234)[,3:4])  #### F5 ######
F6_1234<-cbind(lilinone(F5_1234)[,1:3],lilinone(F5_1234)[,3:4])  #### F6 ######
F7_1234<-cbind(lilinone(F6_1234)[,1:3],lilinone(F6_1234)[,3:4])  #### F7 ######
F8_1234<-lilinone(F7_1234) #### F8 ######
F8_1234
}


MAGIC_200<-NULL;
for (i in 1:200){
MAGIC_200[[i]]<-MAGIC(ttt3,ttt4,ttt5,ttt6) 
}


fiss<-do.call(cbind,MAGIC_200)

id_yy<-NULL
for (i in 1:200){
id_yy[i]<-4*i-1
}

#### Genotype of 200 lines 

fiss_1<-cbind(final_pos,fiss[,id_yy])

colnames(fiss_1)<-c("Chr","Phy_start","Phy_end",paste("L",1:200,sep=""))
write.table("MAGIC-8parent-125k-genotype.txt",col.names=T,row.names=F,sep="\t",quote=F) ###  the result file of genotype with 200 lines 

