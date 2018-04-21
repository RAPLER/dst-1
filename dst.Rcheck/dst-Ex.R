pkgname <- "dst"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('dst')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("addTobca")
### * addTobca

flush(stderr()); flush(stdout())

### Name: addTobca
### Title: Add some elements of 0 mass to an existing mass function
### Aliases: addTobca

### ** Examples

 
y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), 
m=c(0.6, 0.4),  cnames = c("a", "b", "c"), varnb=1)
addTobca(y, matrix(c(0,1,0,0,0,1, 0,1,1), nrow=3, byrow = TRUE))
x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
byrow = TRUE), m=c(0.2,0.5, 0.3), 
cnames =c("a", "b", "c"), varnb=1)
xy <- dsrwon(x,y)
xy1 <- addTobca(nzdsr(xy), matrix(c(0,1,0,0,0,1), nrow=2, byrow = TRUE))
xy1
addTobca(x, f = diag(1,  ncol(x$tt) ) ) # add all singletons



cleanEx()
nameEx("bca")
### * bca

flush(stderr()); flush(stdout())

### Name: bca
### Title: Basic chance assignment mass function
### Aliases: bca bpa

### ** Examples

f<- t(matrix(c(1,0,1,1),ncol=2))
m<- c(.9,.1)
cnames <- c("yes","no")
bca(f, m)
bca(f, m, cnames)
bca(f, m, cnames, varnb = 1)
x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
byrow = TRUE), m=c(0.2,0.5, 0.3), 
cnames =c("a", "b", "c"), varnb = 1)
y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, 
byrow = TRUE), m=c(0.6,0.4), 
cnames =c("a", "b", "c"),infovarnames = "y", varnb = 1)
frame <- bca(matrix(c(1,1,1), nrow=1), m=1, cnames = c("a","b","c"))



cleanEx()
nameEx("bcaRel")
### * bcaRel

flush(stderr()); flush(stdout())

### Name: bcaRel
### Title: Representation of a mass function in a product space
### Aliases: bcaRel

### ** Examples

# A logical implication rule
# A typical relation between two variables in the context of expert systems is the
# logical implication \code{(a -> b)}. Let us suppose
# that \code{a} stands for \code{Rain: {yes, no}} and \code{b} stands for
# \code{RoadWorks: {yes, no}}. From experience,
# I am 75 % sure that there will be RoadWorks if there is no rain.
## 1. The tt table of the logical implication
 ttrwf <- matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),
 nrow=4, byrow = TRUE, 
 dimnames = list(NULL, c("rWdy", "rWdn", "Ry", "Rn")) )
 ## The mass distribution
 specrw <-  matrix(c(1,1,1,2,0.75,0.75,0.75,0.25), ncol = 2, 
 dimnames = list(NULL, c("specnb", "mass"))) 
 ## Variables numbers and sizes
 inforw <- matrix(c(4,5,2,2), ncol = 2, 
 dimnames = list(NULL, c("varnb", "size")) )
bcaRel(tt = ttrwf, spec = specrw, infovar = inforw,
 infovarnames = c("RdWorks", "Rain"), relnb = 6)
 



cleanEx()
nameEx("belplau")
### * belplau

flush(stderr()); flush(stdout())

### Name: belplau
### Title: Calculation of the degrees of Belief and Plausibility
### Aliases: belplau

### ** Examples

x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
byrow = TRUE), m=c(0.2,0.5, 0.3), 
cnames =c("a", "b", "c"), infovarnames = "x", varnb = 1)
belplau(x)
y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, 
byrow = TRUE), m=c(0.6, 0.4),  
cnames = c("a", "b", "c"),  infovarnames = "y", varnb = 1)
belplau(nzdsr(dsrwon(x,y)))
print("compare all elementary events")
xy1 <- addTobca(nzdsr(dsrwon(x,y)), 
matrix(c(0,1,0,0,0,1), nrow=2, byrow = TRUE))
belplau(xy1) 



cleanEx()
nameEx("decode")
### * decode

flush(stderr()); flush(stdout())

### Name: decode
### Title: Find the value in base 10 of a number coded in another base
### Aliases: decode aplDecode

### ** Examples

decode(c(2,2,2,2), c(1,0,1,1)) #   Find the base 10 value of the base 2 number 1011.
decode(2, c(1,0,1,1))  # left argument is extended to vector c(2,2,2,2)
decode(c(365,24,60), c(2,1,57)) # transform 2 days 1 h 57 min in minutes
decode(c(365,24,60), c(1,57))   # right vector extended
decode(c(24,60), c(2,1,57))     # left vector extended
decode(1.5, c(1,2,3)) # polynomial 1*x^2 +2*x +3 evaluated at x=1.5



cleanEx()
nameEx("dotprod")
### * dotprod

flush(stderr()); flush(stdout())

### Name: dotprod
### Title: Generalized inner product of two matrices
### Aliases: dotprod

### ** Examples

print("Standard matrix product")
x <- y <- matrix(c(1:6), nrow = 2, byrow = TRUE)
dotprod(x, t(y), g = "+", f = "*")  ## same as x %*% t(y)
print("Find some data x2 in the rows of a larger matrix y2")
x2 <- matrix(c(1,0,0,1,1,1), nrow = 2, byrow = TRUE)
y2 <- matrix(c(1,0,0,0,1,0,1,1,0,0,1,1,1,1,1), 
nrow = 5, byrow = TRUE)
(1:nrow(y2)) * dotprod(x2, t(y2), g = "&", f = "==")

print("Find some names in a long list")
team_names <- matrix(c("Patrick", "Dole", "Amanda",
 "Dole", "Robert", "Calvin", "Alvina", "Klein",
  "Robert", "Gariepy", "Nellie", "Arcand"),
   ncol = 2, byrow = TRUE)
colnames(team_names) <- c("First_name", "Last_name")
print("Where in the list are the person with first name Robert and where are the Doles?")
BobandDoles <- matrix(c("Robert", "", "", "Dole"),
 ncol = 2, byrow = TRUE)
dotprod(team_names, t(BobandDoles),g="|",f="==") * (1:nrow(team_names))



cleanEx()
nameEx("doubles")
### * doubles

flush(stderr()); flush(stdout())

### Name: doubles
### Title: Remove duplicate rows in a two-dimensional table
### Aliases: doubles

### ** Examples

td0<-matrix(c(rep(c(1,0,1),times=3),0,0,1,1,1,1, 1,1,1),ncol=3,byrow=TRUE)
(doubles(td0))
td1<-matrix(c(rep(c(1,0,1),times=3),0,0,1,1,1,1),ncol=3,byrow=TRUE)
(doubles(td1))
td2<-matrix(c(1:3, 1:3,4:6,1:3),nrow=4,byrow=TRUE)
(doubles(td2))
td3<-matrix(c("d","e","f", rep(c("a","b","cc"),times=3),"g","h","i"),nrow=5,byrow=TRUE)
(doubles(td3))
td4<-matrix(as.logical(td1),nrow=5,byrow=TRUE)
(doubles(td4))



cleanEx()
nameEx("dsrwon")
### * dsrwon

flush(stderr()); flush(stdout())

### Name: dsrwon
### Title: Combination of two mass functions
### Aliases: dsrwon

### ** Examples

x1 <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
byrow = TRUE), m=c(0.2,0.5, 0.3), 
cnames =c("a", "b", "c"),  
infovarnames = "x", varnb=1)
x2 <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, 
byrow = TRUE), m=c(0.6, 0.4),  
cnames = c("a", "b", "c"),  
infovarnames = "x", varnb = 1)
dsrwon(x1,x2)



cleanEx()
nameEx("elim")
### * elim

flush(stderr()); flush(stdout())

### Name: elim
### Title: Reduction of a relation
### Aliases: elim

### ** Examples

 
wr_tt <- matrix(c(0,1,rep(0,5),rep(c(1,0),2),1,1,0,1,0,
rep(1,3),0,1,0,rep(1,6)), ncol=4, byrow = TRUE)
colnames(wr_tt) <- c("rWdy Ry", "rWdy Rn", "rWdn Ry", "rWdn Rn")
 wr_spec = matrix(c(1:7, 0.0476, 0.7619, 0.1905, 0,0,0,0), 
 ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
 wr_infovar = matrix(c(4,5,2,2), ncol = 2, 
 dimnames = list(NULL, c("varnb", "size")) )
 wr_rel <- list(tt=wr_tt, con=0.16, spec=wr_spec,
  infovar=wr_infovar, 
  infovaluenames= list(Rain=c("Ry", "Rn"), RdWorks=c("rWdy", "rWdn") ))
 class(wr_rel)="bcaspec"
 elim(wr_rel, xnb = 5)
 elim(wr_rel, xnb = 4)
 
 mrt_tt <- matrix(c(1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,1,0,0,1,0,1,rep(1,4)), 
 ncol=4, byrow = TRUE)
colnames(mrt_tt) <- c("t6", "f6", "t8", "f8")
 mrt_spec = matrix(c(1,1,1,2,2,2,3, 0.1, 0.1, 0.1, 0.7,0.7,0.7,0.2), 
 ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
 mrt_infovar =matrix(c(6,8,2,2), ncol = 2, 
 dimnames = list(NULL, c("varnb", "size")) )
 mrt_rel <- bcaRel(tt=mrt_tt, spec=mrt_spec, 
 infovar=mrt_infovar, 
 infovarnames= c("Maintenance", "Repair") )
 elim(mrt_rel, xnb = 6)
 elim(mrt_rel, xnb = 8)



cleanEx()
nameEx("encode")
### * encode

flush(stderr()); flush(stdout())

### Name: encode
### Title: Convert a value to its representation in another chosen base
### Aliases: encode aplEncode

### ** Examples

encode(c(2,2,2,2), 11)  # find the base 2 representation of number 11
encode(c(365,24,60), 2997) # convert 2997 minutes to days-hrs-min.



cleanEx()
nameEx("extmin")
### * extmin

flush(stderr()); flush(stdout())

### Name: extmin
### Title: Extension of a relation
### Aliases: extmin

### ** Examples

# making an empty reference relation with mass(frame) = 1 and
# extending a bca to it.
init_tt= matrix(rep(1,10),nrow=1, 
dimnames =list(NULL, c("0", "1", "2", "3", 
"true", "false", "foul", "fair", "true", "false")) )
 init_spec <- matrix(c(1,1), ncol = 2, 
 dimnames = list(NULL, c("specnb", "mass")))
 init_info <- matrix(c(2,4,5,6,4,2,2,2), ncol = 2,
  dimnames = list(NULL, c("varnb", "size")) )
 relRef <- bcaRel(tt = init_tt, spec = init_spec,
  infovar = init_info, 
  infovarnames = c("Delay", "Loading", "Forecast", "Maintenance"),
  relnb = 0)
 # a bcaspec defined on one variable
 l_rel <- bca(f=matrix(c(1,0,1,0,1,1), ncol=2), 
 m=c(0.3,0.5,0.2), cnames=c("true", "false"), 
 infovar=matrix(c(4,2), ncol = 2, 
 dimnames = list(NULL, c("varnb", "size"))), 
 infovarnames= c("Loading"), 
 inforel= matrix(c(7,1), ncol = 2, 
 dimnames = list(NULL, c("relnb", "depth"))))
 z <- extmin(l_rel, relRef)
 prmatrix(t(z$tt), collab = rep("", nrow(z$tt)))
 



cleanEx()
nameEx("inters")
### * inters

flush(stderr()); flush(stdout())

### Name: inters
### Title: Intersection of two tables of propositions
### Aliases: inters

### ** Examples

mx<-matrix(c(0,1,0,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c")))
 rownames(mx) <- nameRows(mx)
my<-matrix(c(0,0,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c")))
 rownames(my) <- nameRows(my)
inters(mx,my)
b1 <- c(FALSE, TRUE, TRUE)
b2 <- c(TRUE, TRUE, FALSE)
names(b1) <- names(b2) <- c("c1","c2","c3")
inters(b1,b2)
x3<-matrix(c(1,1,0,1), ncol=2, dimnames=list(NULL, c("a","b")))
y3<-matrix(c(0,1,1,1), ncol=2, dimnames=list(NULL, c("a","b")))
inters(x3,y3)
x4 <-matrix(c(1,0,1,1,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c","d")))
y4 <-matrix(c(1,0,0,1,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c","d")))
inters(x4,y4)



cleanEx()
nameEx("nameRows")
### * nameRows

flush(stderr()); flush(stdout())

### Name: nameRows
### Title: Using the column names of a matrix to construct names for the
###   rows
### Aliases: nameRows

### ** Examples

f <- matrix(c(0,0,0,1,0,0,0,0,1,1,0,1,1,1,1),ncol=3, byrow = TRUE)
colnames(f) <- c("A","B","C")
rownames(f) <-nameRows(f)
f
f2 <- matrix(c(0,0,0,1,0,0,0,0,1,1,0,1),ncol=3, byrow = TRUE)
colnames(f2) <- c("A2","B2","C2")
rownames(f2) <-nameRows(f2) 
f2



cleanEx()
nameEx("nzdsr")
### * nzdsr

flush(stderr()); flush(stdout())

### Name: nzdsr
### Title: Normalization of a bca mass function
### Aliases: nzdsr

### ** Examples

x1 <- bca(f=matrix(c(1,0,1,1),nrow=2, byrow = TRUE), 
m=c(0.9,0.1), cnames =c("yes", "no"),
infovarnames = "x", varnb = 1)
x2 <- bca(f=matrix(c(0,1,1,1),nrow=2, byrow = TRUE), 
m=c(0.5,0.5), cnames =c("yes", "no"), 
infovarnames = "x", varnb = 1)
print("combination of x1 and x2")
x1x2 <- dsrwon(x1,x2)
nzdsr(x1x2) 

print("normalization of a bca definition.")
y2 <- bca(f=matrix(c(0,0,0,1,0,0,1,1,1),nrow=3, 
byrow = TRUE), m=c(0.2,0.5,0.3), 
cnames =c("a", "b", "c"), varnb = 1)
nzdsr(y2)  



cleanEx()
nameEx("plautrans")
### * plautrans

flush(stderr()); flush(stdout())

### Name: plautrans
### Title: Plausibility transformation of the singletons of a frame
### Aliases: plautrans

### ** Examples

 
x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
byrow = TRUE), m=c(0.2,0.5, 0.3), 
cnames =c("a", "b", "c"), 
infovarnames = "x", varnb = 1)
plautrans(x)



cleanEx()
nameEx("productSpace")
### * productSpace

flush(stderr()); flush(stdout())

### Name: productSpace
### Title: Product space representation of a relation
### Aliases: productSpace

### ** Examples

 ttfw= matrix(c(1,0,1,0,0,1,0,1,1,1,1,1),nrow=3,
  byrow = TRUE, 
  dimnames =list(NULL, c("foul", "fair", "foul", "fair")) )
 specfw = c(1,1,2) 
 infovarfw =matrix(c(5,7,2,2), ncol = 2, 
 dimnames = list(NULL, c("varnb", "size")) )
productSpace(tt=ttfw, specnb=specfw, infovar=infovarfw)



cleanEx()
nameEx("shape")
### * shape

flush(stderr()); flush(stdout())

### Name: shape
### Title: Obtain dimensions of an array or length of a vector with a
###   single command
### Aliases: shape aplShape

### ** Examples

shape(array(c(1:6), c(2,3)))
shape(c("a", "b"))



cleanEx()
nameEx("tabresul")
### * tabresul

flush(stderr()); flush(stdout())

### Name: tabresul
### Title: Prepare a table of results
### Aliases: tabresul

### ** Examples

 
x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
byrow = TRUE), m=c(0.2,0.5, 0.3), 
cnames =c("a", "b", "c"), 
infovarnames = "x", varnb = 1)
y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, 
byrow = TRUE), m=c(0.6, 0.4),  
cnames = c("a", "b", "c"), infovarnames = "y", varnb = 1)
xy <- dsrwon(x,y)
xyNorm <- nzdsr(xy)
tabresul(xyNorm) 
## print("Show all elementary events")
xy1 <- addTobca(nzdsr(dsrwon(x,y)), 
matrix(c(0,1,0,0,0,1), 
nrow=2, byrow = TRUE))
tabresul(xy1)
## print("Remove focal elements with 0 mass")
tabresul(xy1, removeZeroes = TRUE)
print("Retain singletons only")
tabresul(xy1, singletonsOnly = TRUE)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
