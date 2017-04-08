Ruthger Righart

Blogs, tutorials & videos: https://rrighart.github.io

Email: rrighart@googlemail.com

# LOAD DATA

library(jpeg)
myurl <- "https://raw.githubusercontent.com/RRighart/Digits/master/HandwrittenDigits.JPG" 
z <- tempfile()
download.file(myurl,z,mode="wb")
img <- readJPEG(z)
file.remove(z)

# DISPLAY A4 SHEET OF DIGITS

par(mfrow=c(1,1),
        oma = c(0.5,0.5,0.5,0.5) + 0.1,
        mar = c(0,0,0,0) + 0.1)
image(t(apply(img[c(1:dim(img)[1]), c(1:dim(img)[2]), 1], 2, rev)), col=grey.colors(255), axes=F, asp=1)
mtext("Whole image of handwritten digits", cex=0.6, col="red")

# RESIZING DATA

library(EBImage)
ximg<-img[c(1:dim(img)[1]), c(1:dim(img)[2]), 1]
nhsq=42
pix=28
nimg <- resize(ximg, h = nhsq*pix)
dim(nimg)

# SELECTION OF DATA

nimg<-nimg[1:1568, ]
dim(nimg)

# SEGMENTING DATA

matsplitter<-function(M, r, c) {
    rg <- (row(M)-1)%/%r+1
    cg <- (col(M)-1)%/%c+1
    rci <- (rg-1)*max(cg) + cg
    N <- prod(dim(M))/r/c
    cv <- unlist(lapply(1:N, function(x) M[rci==x]))
    dim(cv)<-c(r,c,N)
    cv
} 

nimg<-nimg[c(1:dim(nimg)[1]), ]
dat<-matsplitter(nimg, 28, 28)
class(dat)
dim(dat)

# ADDING LABELS

labels=rep(c(NA, rep(seq.int(0, 9, by=1),4), NA), 56)
table(labels)

# REMOVAL NA

ndat<-dat[,,which(!is.na(labels))]
nlabels<-labels[which(!is.na(labels))]

# DISPLAY DIGITS

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 1:10){
image(t(apply(ndat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1);   mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
}

# CREATING NEGATIVE IMAGES

neg <- function(M,i){
  apply(M, 3, max)[i]-M[,,i]
  }

mmat<-array(0,dim=dim(ndat))

for(i in 1:dim(ndat)[3]){
  mmat[,,i]<-neg(ndat,i)
  }

# DISPLAY DIGITS

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 1:10){
image(t(apply(mmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
}

# AVERAGE AND STANDARD DEVIATION (SD) OF DIGIT INTENSITIES (AT THIS POINT...)

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 0:9){
tm<-apply(mmat[,,which(nlabels==i)], c(1,2), mean)
image(t(apply(tm, 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(i, cex=0.8, col="red", side=3, line=-1)  

# DISPLAY DIGITS

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 0:9){
tm<-apply(mmat[,,which(nlabels==i)], c(1,2), sd)
image(t(apply(tm, 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(i, cex=0.8, col="red", side=3, line=-1)
}

# HISTOGRAMS OF IMAGE INTENSITIES

par(mfrow=c(2,5),
        oma = c(2,2,2,2) + 0.1,
        mar = c(2,2,2,2) + 0.1)
for(i in 0:9){
tm<-apply(mmat[,,which(nlabels==i)], c(1,2), mean)
hist(tm, labels=FALSE, axes=TRUE, freq=FALSE, col="black", xlim=c(0,1), ylim=c(0,16), main=i)
}

# SCALE IMAGES

range01 <- function(M){(M-min(M))/(max(M)-min(M))}

scmat<-array(0,dim=dim(mmat))
for(i in 1:dim(mmat)[3]){
scmat[,,i]<-range01(mmat[,,i])
}

# DISPLAY DIGITS

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 1:10){
image(t(apply(scmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
}

# SUMMARY STATS

apply(scmat[,,c(1:10)], 3, min)
apply(scmat[,,c(1:10)], 3, max)

# THRESHOLD IMAGES

thresh <- function(M){ifelse(M<0.2, 0, M)}
thmat<-thresh(scmat)

# DISPLAY DIGITS

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 1:10){
image(t(apply(thmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
}

# CENTRALIZE IMAGES

bmat<-array(0,dim=dim(thmat))
for(i in 1:dim(thmat)[3]){
temp<-thmat[,,i]
w<-temp[apply(temp,1,mean)>0,apply(temp,2,mean)>0]
if(is.null(dim(w))) next
if(dim(w)[1]<4) next
if(dim(w)[2]<4) next
if(dim(w)[1]>26) next
if(dim(w)[2]>26) next
bim<-matrix(rep(0,28*28),nrow=28)
ly=floor(((dim(bim)[1]-dim(w)[1])/2)+0.5)
uy=ly+dim(w)[1]-1
lx=floor(((dim(bim)[2]-dim(w)[2])/2)+0.5)
ux=lx+dim(w)[2]-1
bim[c(ly:uy),c(lx:ux)]<-w
bmat[,,i]<-bim
}

# DISPLAY DIGITS

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 1:10){
image(t(apply(bmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
}

# SELECT A FRAME OF 24X24 PIXELS

sfr<-bmat[c(3:26), c(3:26), ]

# DISPLAY DIGITS

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 1:10){
image(t(apply(sfr[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
}

# DISPLAY AVERAGE DIGITS

par(mfrow=c(2,5),
        oma = c(3,3,3,3) + 0.1,
        mar = c(0,0.1,0,0.1) + 0.1)
for(i in 0:9){
tm<-apply(sfr[,,which(nlabels==i)], c(1,2), mean)
image(t(apply(tm, 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(i, cex=0.8, col="red", side=3, line=-1)  
}

# BRING ARRAY TO MATRIX

ownset<-aperm(sfr, c(3,2,1))
dim(ownset)<-c(dim(sfr)[3],576)
ownset<-data.frame(ownset)