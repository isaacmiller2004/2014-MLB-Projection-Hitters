library(plyr)

#Load data
batting <- read.csv("Batting_names.csv", header=TRUE)
Roster <- read.csv("roster2013.csv")
retro.2013 <- read.csv("all2013.csv", header=FALSE)
retro.2012 <- read.csv("all2012.csv", header=FALSE)
retro.2011 <- read.csv("all2011.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(retro.2013) <- fields[, "Header"]
names(retro.2012) <- fields[, "Header"]
names(retro.2011) <- fields[, "Header"]


#function for collapsing 'stint' variable
sum.function <- function(d){
  d1 <- d[, 8:26]
  apply(d1, 2, sum)
}

#Collapse stints and add Plate Appearances and BABIP
batting <- ddply(batting, .(playerID, yearID, first, last), sum.function)
batting$PA <- with(batting, AB+BB+HBP+SF)
batting$BABIP <- with(batting, round((H-HR)/(AB-SO-HR+SF), 3))

## MAIN PROJECTION FUNCTION
projection <- function(First, Last, auto=FALSE){

#Get player name
if(First==""){
  First <- readline("Enter the hitter's first name: ")
}
if(Last==""){
  Last <- readline("Enter the hitter's last name: ")
}
  
#Get retrosheet ID
retro.id <- subset(Roster, First.Name == First &
                      Last.Name == Last)$Player.ID
retro.id <- as.character(retro.id)

#Generate subsets for player
batter <- subset(batting, first==First & last==Last)
batter.2013 <- subset(batter, yearID==2013)
batter.2012 <- subset(batter, yearID==2012)
batter.2011 <- subset(batter, yearID==2011)
batter.100 <- subset(batter, AB>99)
batter.retro.2013 <- subset(retro.2013, BAT_ID==retro.id & BATTEDBALL_CD!="")
batter.retro.2012 <- subset(retro.2012, BAT_ID==retro.id & BATTEDBALL_CD!="")
batter.retro.2011 <- subset(retro.2011, BAT_ID==retro.id & BATTEDBALL_CD!="")

#Plate appearances
PAs <- c(batter.2013$PA, batter.2012$PA, batter.2011$PA)
if(auto==FALSE){
cat("Last 3 years of PA:\n2013: ", PAs[1], 
    "\n2012: ", PAs[2],
    "\n2011: ", PAs[3],
    "\nMean: ", round(mean(PAs[]), 2))                                         
proj.PA <- as.numeric(readline("Enter the hitter's projected PLATE APPEARANCES: "))
} else {
  proj.PA <- mean(PAs)
}

#Runs
r.pa <- (batter.2013$R+batter.2012$R+batter.2011$R)/(batter.2013$PA+batter.2012$PA+batter.2011$PA)
proj.R <- r.pa*proj.PA*(sample(90:110, 1)/100)

#RBI
rbi.pa <- (batter.2013$RBI+batter.2012$RBI+batter.2011$RBI)/(batter.2013$PA+batter.2012$PA+batter.2011$PA)
proj.RBI <- rbi.pa*proj.PA*(sample(90:110, 1)/100)

#Doubles
X2B.rate <- c(batter.2013$AB/batter.2013$X2B, batter.2012$AB/batter.2012$X2B, batter.2011$AB/batter.2011$X2B)
if(auto==FALSE){
cat("Last 3 years of AB/2B:\n2013: ", round(X2B.rate[1], 2), 
    "\n2012: ", round(X2B.rate[2], 2),
    "\n2011: ", round(X2B.rate[3], 2),
    "\nMean: ", round((batter.2013$AB+batter.2012$AB+batter.2011$AB)/
                        (batter.2013$X2B+batter.2012$X2B+batter.2011$X2B), 2))                                                                                                               
proj.2B.rate <- as.numeric(readline("Enter the hitter's projected AB/Double Rate: "))
} else {
  proj.2B.rate <- (batter.2013$AB+batter.2012$AB+batter.2011$AB)/
    (batter.2013$X2B+batter.2012$X2B+batter.2011$X2B)
}

#Triples
X3B.rate <- c(batter.2013$AB/batter.2013$X3B, batter.2012$AB/batter.2012$X3B, batter.2011$AB/batter.2011$X3B)
if(auto==FALSE){
cat("Last 3 years of AB/3B:\n2013: ", round(X3B.rate[1], 2), 
    "\n2012: ", round(X3B.rate[2], 2),
    "\n2011: ", round(X3B.rate[3], 2),
    "\nMean: ", round((batter.2013$AB+batter.2012$AB+batter.2011$AB)/
                        (batter.2013$X3B+batter.2012$X3B+batter.2011$X3B), 2))                                                                                                               
    proj.3B.rate <- as.numeric(readline("Enter the hitter's projected AB/Triple Rate: "))
} else {
  proj.3B.rate <- (batter.2013$AB+batter.2012$AB+batter.2011$AB)/
    (batter.2013$X3B+batter.2012$X3B+batter.2011$X3B)
}

#Hit by pitches
HBP.rate <- c(batter.2013$PA/batter.2013$HBP, batter.2012$PA/batter.2012$HBP, batter.2011$PA/batter.2011$HBP)
if(auto==FALSE){
cat("Last 3 years of PA/HBP:\n2013: ", round(HBP.rate[1], 2), 
    "\n2012: ", round(HBP.rate[2], 2),
    "\n2011: ", round(HBP.rate[3], 2),
    "\nMean: ", round((batter.2013$PA+batter.2012$PA+batter.2011$PA)/
                        (batter.2013$HBP+batter.2012$HBP+batter.2011$HBP), 2))                                                                                                               
    proj.HBP.rate <- as.numeric(readline("Enter the hitter's projected PA/HBP Rate: "))
} else {
  proj.HBP.rate <- (batter.2013$PA+batter.2012$PA+batter.2011$PA)/
    (batter.2013$HBP+batter.2012$HBP+batter.2011$HBP)
}

#Sacrifice flies
SF.rate <- c(batter.2013$PA/batter.2013$SF, batter.2012$PA/batter.2012$SF, batter.2011$PA/batter.2011$SF)
if(auto==FALSE){
cat("Last 3 years of PA/SF:\n2013: ", round(SF.rate[1], 2), 
    "\n2012: ", round(SF.rate[2], 2),
    "\n2011: ", round(SF.rate[3], 2),
    "\nMean: ", round((batter.2013$PA+batter.2012$PA+batter.2011$PA)/
                        (batter.2013$SF+batter.2012$SF+batter.2011$SF), 2))                                                                                                               
    proj.SF.rate <- as.numeric(readline("Enter the hitter's projected PA/SF Rate: "))
} else {
  proj.SF.rate <- (batter.2013$PA+batter.2012$PA+batter.2011$PA)/
    (batter.2013$SF+batter.2012$SF+batter.2011$SF)
}

#Stolen base attempts
SBA.rate <- c(batter.2013$PA/(batter.2013$SB+batter.2013$CS), 
              batter.2012$PA/(batter.2012$SB+batter.2012$CS), 
              batter.2011$PA/(batter.2011$SB+batter.2011$CS))
if(auto==FALSE){
cat("Last 3 years of PA/SBA:\n2013: ", round(SBA.rate[1], 2), 
    "\n2012: ", round(SBA.rate[2], 2),
    "\n2011: ", round(SBA.rate[3], 2),
    "\nMean: ", round((batter.2013$PA+batter.2012$PA+batter.2011$PA)/
                        (batter.2013$SB+batter.2012$SB+batter.2011$SB
                         +batter.2013$CS+batter.2012$CS+batter.2011$CS), 2))                                                                                                                
proj.SBA.rate <- as.numeric(readline("Enter the hitter's projected PA/SBA Rate: "))
} else {
  proj.SBA.rate <- (batter.2013$PA+batter.2012$PA+batter.2011$PA)/
    (batter.2013$SB+batter.2012$SB+batter.2011$SB
     +batter.2013$CS+batter.2012$CS+batter.2011$CS)
}

#Stolen base success rate
SB.succ.rate <- c(round(batter.2013$SB/(batter.2013$SB+batter.2013$CS), 2),
                  round(batter.2012$SB/(batter.2012$SB+batter.2012$CS), 2),
                  round(batter.2011$SB/(batter.2011$SB+batter.2011$CS), 2))
if(auto==FALSE){
cat("Last 3 years of SB Success Rate:\n2013: ", round(SB.succ.rate[1], 2), 
    "\n2012: ", round(SB.succ.rate[2], 2),
    "\n2011: ", round(SB.succ.rate[3], 2),
    "\nMean: ", round((batter.2013$SB+batter.2012$SB+batter.2011$SB)/
                        (batter.2013$SB+batter.2013$CS+batter.2012$SB+batter.2012$CS
                         +batter.2011$SB+batter.2011$CS), 2))                                         
proj.SB.succ.rate <- as.numeric(readline("Enter the hitter's projected SB Success Rate: "))
} else {
  proj.SB.succ.rate <- (batter.2013$SB+batter.2012$SB+batter.2011$SB)/
    (batter.2013$SB+batter.2013$CS+batter.2012$SB+batter.2012$CS
     +batter.2011$SB+batter.2011$CS)
}

#Walk rate
BB.rate <- c(batter.2013$BB/batter.2013$PA, batter.2012$BB/batter.2012$PA, batter.2011$BB/batter.2011$PA)
if(auto==FALSE){
cat("Last 3 years of BB Rate:\n2013: ", round(BB.rate[1], 4), 
    "\n2012: ", round(BB.rate[2], 4),
    "\n2011: ", round(BB.rate[3], 4),
    "\nMean: ", round((batter.2013$BB+batter.2012$BB+batter.2011$BB)/
                        (batter.2013$PA+batter.2012$PA+batter.2011$PA), 2))   
proj.BB.rate <- as.numeric(readline("Enter the hitter's projected BB Rate: "))
} else {
  proj.BB.rate <- (batter.2013$BB+batter.2012$BB+batter.2011$BB)/
    (batter.2013$PA+batter.2012$PA+batter.2011$PA)
}

#Strikeout rate
SO.rate <- c(batter.2013$SO/batter.2013$PA, batter.2012$SO/batter.2012$PA, batter.2011$SO/batter.2011$PA)
if(auto==FALSE){
cat("Last 3 years of SO Rate:\n2013: ", round(SO.rate[1], 4), 
    "\n2012: ", round(SO.rate[2], 4),
    "\n2011: ", round(SO.rate[3], 4),
    "\nMean: ", round((batter.2013$SO+batter.2012$SO+batter.2011$SO)/
                        (batter.2013$PA+batter.2012$PA+batter.2011$PA), 2))            
proj.SO.rate <- as.numeric(readline("Enter the hitter's projected SO Rate: "))
} else {
  proj.SO.rate <- (batter.2013$SO+batter.2012$SO+batter.2011$SO)/
    (batter.2013$PA+batter.2012$PA+batter.2011$PA)
}

#BABIP
BABIPs <- c(batter.2013$BABIP, batter.2012$BABIP, batter.2011$BABIP)
if(auto==FALSE){
cat("Last 3 years of BABIP:\n2013: ", BABIPs[1], 
    "\n2012: ", BABIPs[2],
    "\n2011: ", BABIPs[3],
    "\nMean: ", round((batter.2013$H-batter.2013$HR+batter.2012$H-batter.2012$HR+batter.2011$H-batter.2011$HR)
                      /(batter.2013$AB+batter.2012$AB+batter.2011$AB-batter.2013$SO-batter.2012$SO
                        -batter.2011$SO-batter.2013$HR-batter.2012$HR-batter.2011$HR+
                          batter.2013$SF+batter.2012$SF+batter.2011$SF
                        ), 3))    
proj.BABIP <- as.numeric(readline("Enter the hitter's projected BABIP: "))
} else {
  proj.BABIP <- (batter.2013$H-batter.2013$HR+batter.2012$H-batter.2012$HR+batter.2011$H-batter.2011$HR)/
  (batter.2013$AB+batter.2012$AB+batter.2011$AB-batter.2013$SO-batter.2012$SO-
    batter.2011$SO-batter.2013$HR-batter.2012$HR-batter.2011$HR+
    batter.2013$SF+batter.2012$SF+batter.2011$SF)
}

#Groundballs
GBs <- c(round(sum(ifelse(batter.retro.2013$BATTEDBALL_CD=="G", 1, 0)/length(batter.retro.2013[,1])), 3),
         round(sum(ifelse(batter.retro.2012$BATTEDBALL_CD=="G", 1, 0)/length(batter.retro.2012[,1])), 3),
         round(sum(ifelse(batter.retro.2011$BATTEDBALL_CD=="G", 1, 0)/length(batter.retro.2011[,1])), 3))
if(auto==FALSE){
cat("Last 3 years of GB rates:\n2013: ", GBs[1], 
    "\n2012: ", GBs[2],
    "\n2011: ", GBs[3],
    "\nMean: ", round((sum(ifelse(batter.retro.2013$BATTEDBALL_CD=="G", 1, 0))+
                            sum(ifelse(batter.retro.2012$BATTEDBALL_CD=="G", 1, 0))+      
                                sum(ifelse(batter.retro.2011$BATTEDBALL_CD=="G", 1, 0)))/
                        (length(batter.retro.2013[,1])+length(batter.retro.2012[,1])+
                           length(batter.retro.2011[,1])), 3))                                        
proj.GB <- as.numeric(readline("Enter the hitter's projected GB rate: "))
} else {
  proj.GB <- (sum(ifelse(batter.retro.2013$BATTEDBALL_CD=="G", 1, 0))+
                sum(ifelse(batter.retro.2012$BATTEDBALL_CD=="G", 1, 0))+      
                sum(ifelse(batter.retro.2011$BATTEDBALL_CD=="G", 1, 0)))/
    (length(batter.retro.2013[,1])+length(batter.retro.2012[,1])+
       length(batter.retro.2011[,1]))
}

#Flyballs
FBs <- c(round(sum(ifelse(batter.retro.2013$BATTEDBALL_CD=="F" | 
          batter.retro.2013$BATTEDBALL_CD=="P", 
          1, 0)/length(batter.retro.2013[,1])), 3),
         round(sum(ifelse(batter.retro.2012$BATTEDBALL_CD=="F" |
          batter.retro.2012$BATTEDBALL_CD=="P",                 
          1, 0)/length(batter.retro.2012[,1])), 3),
         round(sum(ifelse(batter.retro.2011$BATTEDBALL_CD=="F" | 
          batter.retro.2011$BATTEDBALL_CD=="P",                 
          1, 0)/length(batter.retro.2011[,1])), 3))
    
FB1 <-  sum(ifelse(batter.retro.2013$BATTEDBALL_CD=="F" | 
                     batter.retro.2013$BATTEDBALL_CD=="P", 
                   1, 0))
FB2 <- sum(ifelse(batter.retro.2012$BATTEDBALL_CD=="F" | 
                  batter.retro.2012$BATTEDBALL_CD=="P", 
                  1, 0))
FB3 <-  sum(ifelse(batter.retro.2011$BATTEDBALL_CD=="F" | 
                   batter.retro.2011$BATTEDBALL_CD=="P", 
                  1, 0))
FB.tot <- FB1+FB2+FB3
BatBall.tot <- length(batter.retro.2013[,1])+length(batter.retro.2012[,1])+length(batter.retro.2011[,1])   
FB.mean <- FB.tot/BatBall.tot            
    
if(auto==FALSE){
cat("Last 3 years of FB rates:\n2013: ", FBs[1], 
    "\n2012: ", FBs[2],
    "\n2011: ", FBs[3],
    "\nMean: ", round(FB.mean, 2))                                         
proj.FB <- as.numeric(readline("Enter the hitter's projected FB rate: "))
} else {
  proj.FB <- FB.mean
}

#Home run/Flyball rate
HRFB <- c(round(batter.2013$HR/FB1, 3),
          round(batter.2012$HR/FB2, 3),
          round(batter.2011$HR/FB3, 3))
if(auto==FALSE){
cat("Last 3 years of HR/FB rates:\n2013: ", HRFB[1], 
    "\n2012: ", HRFB[2],
    "\n2011: ", HRFB[3],
    "\nMean: ", round((batter.2013$HR+batter.2012$HR+batter.2011$HR)/FB.tot, 3))                                         
proj.HRFB <- as.numeric(readline("Enter the hitter's projected HR/FB rate: "))
} else {
  proj.HRFB <- (batter.2013$HR+batter.2012$HR+batter.2011$HR)/FB.tot
}

#Final projections
final.bb <- proj.PA*proj.BB.rate
final.so <- proj.PA*proj.SO.rate
final.hbp <- ifelse(proj.HBP.rate>0, proj.PA/proj.HBP.rate, 0)
final.sf <- ifelse(proj.SF.rate>0, proj.PA/proj.SF.rate, 0)
final.sb <- ifelse(proj.SBA.rate>0, proj.PA/proj.SBA.rate*proj.SB.succ.rate, 0)
final.cs <- ifelse(proj.SBA.rate>0, proj.PA/proj.SBA.rate*(1-proj.SB.succ.rate), 0)
final.ldperc <- 1-proj.FB-proj.GB
final.ab <- proj.PA-final.bb-final.hbp-final.sf
final.bip <- final.ab-final.so+final.sf
final.2B <- ifelse(proj.2B.rate>0, final.ab/proj.2B.rate, 0)
final.3B <- ifelse(proj.3B.rate>0, final.ab/proj.3B.rate, 0)
final.hr <- proj.HRFB*proj.FB*final.bip
final.bip <- final.ab-final.so+final.sf-final.hr
final.hits <- proj.BABIP*final.bip+final.hr
final.1B <- final.hits-final.hr-final.2B-final.3B
final.avg <- ifelse(final.hits>0, final.hits/final.ab, 0)
final.obp <- (final.hits+final.bb+final.hbp)/(final.ab+final.bb+final.hbp+final.sf)
final.slg <- (final.1B+(2*final.2B)+(3*final.3B)+(4*final.hr))/final.ab
final.ops <- final.obp+final.slg

#Print results
cat("2014 Projection for ", First, " ", Last, ": ",
    "\nPA: ", round(proj.PA, 0),
    "\nAB: ", round(final.ab, 0),
    "\nAVG: ", round(final.avg, 3),
    "\nHR: ", round(final.hr, 0),
    "\nR: ", round(proj.R, 0),
    "\nRBI: ", round(proj.RBI, 0),
    "\nOBP: ", round(final.obp, 3),
    "\nSLG: ", round(final.slg, 3),
    "\nOPS: ", round(final.ops, 3),
    "\nBB: ", round(final.bb, 0),
    "\nSO: ", round(final.so, 0), 
    "\nHits: ", round(final.hits, 0),
    "\n1B: ", round(final.1B, 0),
    "\n2B: ", round(final.2B, 0),
    "\n3B: ", round(final.3B, 0),
    "\nSB: ", round(final.sb, 0),
    "\nCS: ", round(final.cs, 0),
    "\nBABIP: ", round(proj.BABIP, 3),
    sep=""
    )

}








