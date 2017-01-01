#Author: Ramanpreet Chand
#Date: January 1, 2017

# Set Working Directory
setwd("C:/Workspace/baseball-project")

# Load Packages
library(RMySQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)
library(pryr)
library(data.table)
library(plotly)

#plotly("rchand95", "p6JY7EUYhc5Gqe30Yr4Q")

# Disable Scientific Notation
options(scipen = 999)

# Open MySQL connection: Private
con <- dbConnect(RMySQL::MySQL(), user = '', password = '', host = '', dbname = '')

# Close MySQL connection using on.exit()
on.exit(dbDisconnect(con))

# Build Queries by decade
query.events <- dbSendQuery(con, "SELECT GAME_ID, YEAR_ID, BAT_ID, PIT_ID, BAT_TEAM_ID, FLD_TEAM_ID, H_CD, AB_FL, EVENT_CD, SH_FL, SF_FL, BATTEDBALL_CD, RBI_CT, DP_FL, TP_FL, PB_FL, BUNT_FL, BAT_HAND_CD, PIT_HAND_CD, PA_NEW_FL, ERR_CT, PO1_FLD_CD, PO2_FLD_CD, PO3_FLD_CD
                            FROM retrosheet.events WHERE YEAR_ID BETWEEN 1976 AND 2015")

# Store Queries in local data frames
data.events <- fetch(query.events, n = -1)

# Class Declarations
data.events$YEAR_ID <- factor(data.events$YEAR_ID)
data.events$AB_FL <- factor(data.events$AB_FL, labels = c('Not At Bat', 'At Bat'))
data.events$H_CD <- factor(data.events$H_CD, labels = c('No Hit', '1B', '2B', '3B', 'HR'))
data.events$EVENT_CD <- factor(data.events$EVENT_CD, labels = c('Out', 'K', 'SB', 'DI', 'CS', 'PK', 'WP', 'PB', 'BK', 'OA', 'FE', 'NIBB', 'IBB', 'HBP', 'XI', 'ROE', 'FC', '1B', '2B', '3B', 'HR'))
data.events$BATTEDBALL_CD <- factor(data.events$BATTEDBALL_CD, labels = c('Null', 'FB', 'GB', 'LD', 'PU'))
data.events$BUNT_FL <- factor(data.events$BUNT_FL)
data.events$SH_FL <- factor(data.events$SH_FL, labels = c('Not Awarded', 'Awarded'))
data.events$SF_FL <- factor(data.events$SF_FL, labels = c('Not Awarded', 'Awarded'))
data.events$DP_FL <- factor(data.events$DP_FL, labels = c('Not Awarded', 'Awarded'))
data.events$TP_FL <- factor(data.events$TP_FL, labels = c('Not Awarded', 'Awarded'))
data.events$PB_FL <- factor(data.events$PB_FL, labels = c('Not Awarded', 'Awarded'))
data.events$PA_NEW_FL <- factor(data.events$PA_NEW_FL, labels = c('Not Awarded', 'Awarded'))
data.events$PIT_HAND_CD <- factor(data.events$PIT_HAND_CD, labels = c('Both', 'Left', 'Right'))
data.events$BAT_HAND_CD <- factor(data.events$BAT_HAND_CD, labels = c('Unknown', 'Left', 'Right'))

# Declare new variables
data.events <- data.events %>%
    mutate(h = ifelse(H_CD == 'No Hit', 0, 1), 
           ab = ifelse(AB_FL == 'Not At Bat', 0, 1),
           bb = ifelse(EVENT_CD == 'NIBB', 1, 0), 
           hbp = ifelse(EVENT_CD == 'HBP', 1, 0),
           sf = ifelse(SF_FL == 'Not Awarded', 0, 1), 
           weightH = ifelse(EVENT_CD == '1B', 1, ifelse(EVENT_CD == '2B', 2, ifelse(EVENT_CD == '3B',3, ifelse(EVENT_CD == 'HR', 4, 0)))),
           cs = ifelse(EVENT_CD == 'CS', 1, 0), 
           ibb = ifelse(EVENT_CD == 'IBB', 1, 0),
           sh = ifelse(SH_FL == 'Not Awarded', 0, 1), 
           sb = ifelse(EVENT_CD == 'SB', 1, 0), 
           out = ifelse(EVENT_CD == 'Out', 1, 0),
           k = ifelse(EVENT_CD =='K', 1, 0), 
           bk = ifelse(EVENT_CD == 'BK', 1, 0),
           di = ifelse(EVENT_CD == 'DI', 1, 0),
           flyBall = ifelse(BATTEDBALL_CD == 'FB', 1, 0),
           groundBall = ifelse(BATTEDBALL_CD == 'GB', 1, 0),
           firstB = ifelse(EVENT_CD=='1B', 1, 0), 
           secB = ifelse(EVENT_CD=='2B',1,0), 
           thirdB = ifelse(EVENT_CD=='3B',1,0), 
           hr = ifelse(EVENT_CD=='HR', 1, 0),
           gidp = ifelse(DP_FL=='Not Awarded', 0, 1),
           tp = ifelse(TP_FL == 'Not Awarded', 0, 1),
           pb = ifelse(PB_FL == 'Not Awarded', 0, 1),
           pa = ifelse(PA_NEW_FL == 'Not Awarded', 0, 1),
           wp = ifelse(EVENT_CD == 'WP', 1, 0),
           roe = ifelse(EVENT_CD == "ROE", 1, 0))

# Dimensions
dim(data.events) #7085356 obs of 50 variables

# Data Frame
str(data.events, max.level=1)

# Memory Size
object_size(data.events) #2.44 GB


################################################################################################################################################################
################################################################################################################################################################


# Import Other Datasets

# Extra for All
fileID <- "player_ids.csv"
col <- c('BAT_ID', 'FIRST', 'LAST', 'DEBUT')
Player <- fread(fileID, data.table = FALSE, header = TRUE, showProgress = FALSE, na.strings = "NULL")
names(Player) <- col
# Substring
Player <- Player %>% mutate(DEBUT = substr(DEBUT, 0, 10)) %>% data.frame()
# Class Declaration
Player$DEBUT <- mdy(Player$DEBUT)

# Memory Size: 
object_size(Player) #2.36MB

# Extra for Fielders and Pitchers
fileMas <- "lahmanDB/2015/baseballdatabank-master/core/Master.csv"
masVar <- c("playerID", "retroID")
master <- fread(fileMas, data.table = FALSE, showProgress = FALSE, na.strings = "NULL", select = masVar)

# Memory Size: 
object_size(master) #2.41 MB

# Extra for Pitchers
file <- "lahmanDB/2015/baseballdatabank-master/core/Pitching.csv"
myVar <- c("playerID", "yearID", "W", "L", "G", "GS", "CG", "SHO", "SV", "IPouts", "ER", "BFP", "GF", "R")
Pitching.file <- fread(file, data.table = FALSE, showProgress = FALSE, na.strings = "NULL", select = myVar)
Pitching.file <- Pitching.file %>% filter(yearID >= 1976)

# Memory Size: 
object_size(Pitching.file) #1.61 MB

# Extra for Fielders 
fileField <- "lahmanDB/2015/baseballdatabank-master/core/Fielding.csv"
Fielding.file <- fread(fileField, data.table = FALSE, showProgress = FALSE, na.strings = "NULL", select=c("yearID", "teamID", "playerID", "POS", "InnOuts", "A"))
Fielding.file <- merge(Fielding.file, master, by="playerID")
Fielding.file <- Fielding.file %>% filter(yearID >= 1976)
colsF <- c("playerID", "YEAR_ID", "FLD_TEAM_ID", "POS", "InnOuts", "Assists", "BAT_ID")
names(Fielding.file) <- colsF

# Memory Size: 
object_size(Fielding.file) #4.81 MB


################################################################################################################################################################
################################################################################################################################################################


AtBat.significance <- data.events %>% select(YEAR_ID, BAT_ID, BAT_HAND_CD, AB_FL) %>% mutate(ab = ifelse(AB_FL == 'Not At Bat', 0, 1)) %>% group_by(YEAR_ID, BAT_ID, BAT_HAND_CD) %>% summarize(sumAB = sum(ab)) %>% as.data.frame()
class(AtBat.significance$sumAB) <- "numeric"
quantile(AtBat.significance$sumAB, probs = seq(0, 1, 0.25), na.rm = FALSE)

ggplot(AtBat.significance) + labs(x="Year", y="At Bat", title="Distribution of Total At Bats") + geom_boxplot(aes(YEAR_ID, sumAB, color=factor(YEAR_ID)), outlier.shape = 1) + coord_flip() + theme(legend.position="none")


plotAtBat <- ggplot(AtBat.significance, aes(x=sumAB)) + labs(x="Total AB", y="Frequency", title="Frequency of At Bats") + 
  geom_histogram(bins=30, col="black", fill="pink", alpha=0.3)
plotAtBat

plotAtBat2 <- plot_ly(AtBat.significance, alpha=0.6, x=~sumAB, type = "histogram") %>% layout(title="Frequency of At Bats",
                                                                                             xaxis = list(title="Total AB"),
                                                                                             yaxis = list(title="Frequency"))
plotAtBat
qbinom(c(0.9, 0.1), 500, 0.260)/500

# Batting Statistics Summarized over Time
Batting.active <- data.events %>% group_by(YEAR_ID, BAT_ID, BAT_HAND_CD) %>% filter(sum(ab)>=250) %>%
  summarize(firstBase = sum(firstB), secondBase=sum(secB), thirdBase=sum(thirdB), HR=sum(hr), K=sum(k),
            H=sum(h), AB = sum(ab), BB=sum(bb), CS=sum(cs), GIDP=sum(gidp), HBP=sum(hbp), IBB=sum(ibb), PA=sum(pa), RBI=sum(RBI_CT), SF=sum(sf), SH=sum(sh), SB=sum(sb), TB=sum(weightH)) %>%
  as.data.frame()

# Class Declaration
Batting.active$YEAR_ID <- ymd(paste(Batting.active$YEAR_ID, "-01-01", sep=""))

# mutate new statistics for each roster
Batting.active <- Batting.active %>% group_by(YEAR_ID, BAT_ID, BAT_HAND_CD) %>%
  mutate(Avg = round(H/AB,3),
         OBP = round(100*((H + BB + HBP)/(AB + BB + HBP + SF)),2),
         SLG = round((TB/AB),3),
         OPS = round((sum(OBP)/100) + sum(SLG),3),
         ISO = (TB-H)/AB,
         RC = round((((H + BB + HBP - CS - GIDP) * (TB + 0.26 * (BB - IBB + HBP)) + (0.52 * (SH + SF + SB)))/(AB + BB + HBP + SH + SF)),3),
         BABIP = round(((H - HR)/(AB - K - HR + SF)),3)) %>% as.data.frame()

# Merge Datasets to Player
Batting.active <- merge(Batting.active, Player, by="BAT_ID")

# Print Data Frames
str(Batting.active, max.level=1)

# Scatter Plot : Plot the relationship of RC to each metric
plotBat <- plot_ly(data=Batting.active, x=~OBP, y=~RC,
        text= ~paste("OBP: ", OBP, "<br>Runs: ", RC),
        color = ~OBP, RC = ~OBP,
        colors = c("#E5BABA", "#28294c)") %>% layout(title = 'Runs Created by On-Base Percentage', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotBat

plotBat2 <- plot_ly(data=Batting.active, x=~SLG, y=~RC,
                    text= ~paste("SLG: ", SLG, "<br>Runs: ", RC),
                    color = ~SLG, RC = ~SLG,
                    colors = c("#BF382A", "#0C4B8E")) %>% layout(title = 'Runs Created by Slugging', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotBat2

plotBat3 <- plot_ly(data=Batting.active, x=~OPS, y=~RC,
                    text= ~paste("OPS: ", OPS, "<br>Runs: ", RC),
                    color = ~OPS, RC = ~OPS,
                    colors = c("#e16135", "#723446")) %>% layout(title = 'Runs Created by On-Base Plus Slugging', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotBat3

plotBat4 <- plot_ly(data=Batting.active, x=~ISO, y=~RC,
                    text= ~paste("ISO: ", ISO, "<br>Runs: ", RC),
                    color = ~ISO, RC = ~ISO,
                    colors= c("#00ffff", "#ff0000")) %>% layout(title = 'Runs Created by Isolated Power', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotBat4

plotBat5 <- plot_ly(data=Batting.active, x=~BABIP, y=~RC,
                    text= ~paste("BABIP: ", OPS, "<br>Runs: ", RC),
                    color = ~BABIP, RC = ~BABIP, 
                    colors = c("#ffff00", "#0000ff")) %>% layout(title = 'Runs Created by Batting Average on Balls in Play', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))

plotBat5


# Boxplot : Distribution of each metric over time
plotBat6 <- ggplot(Batting.active) + labs(x="Year", y="Runs Created") + geom_boxplot(aes(YEAR_ID, RC, color=factor(YEAR_ID)), outlier.shape = 1) + coord_flip() + theme_gray()
plotBat7 <- ggplot(Batting.active) + labs(x="Year", y="On Base Percentage") + geom_boxplot(aes(YEAR_ID, OBP, color=factor(YEAR_ID)), outlier.shape = 1) + coord_flip()+ theme_gray()
plotBat8 <- ggplot(Batting.active) + labs(x="Year", y="Slugging") + geom_boxplot(aes(YEAR_ID, SLG, color=factor(YEAR_ID)), outlier.shape = 1) + coord_flip() + theme_gray()
plotBat9 <- ggplot(Batting.active) + labs(x="Year", y="On Base Plus Slugging") + geom_boxplot(aes(YEAR_ID, OPS, color=factor(YEAR_ID)), outlier.shape = 1) + coord_flip() + theme_gray()
plotBat10 <- ggplot(Batting.active) + labs(x="Year", y="Isolated Power") + geom_boxplot(aes(YEAR_ID, ISO, color=factor(YEAR_ID)), outlier.shape = 1) + coord_flip() + theme_gray()
plotBat11 <- ggplot(Batting.active) + labs(x="Year", y="Batting Average on Balls in Play") + geom_boxplot(aes(YEAR_ID, BABIP, color=factor(YEAR_ID)), outlier.shape = 1) + coord_flip() + theme_gray()

plotBat12 <- ggplot(Batting.active, aes(RC)) + labs(x="Runs Created", y="Density", main="Runs Created per Season", xlim=c(0,100), ylim=c(0,30)) +  geom_histogram(aes(y=..density..), bins = 30, col="red", fill="green", alpha=0.2) + geom_density(aes(color=factor(YEAR_ID))) + facet_wrap(~YEAR_ID, nrow=8)
plotBat12 + theme_light()

# Plot a Time Series of the average of each metric
Batting <- Batting.active %>% group_by(YEAR_ID) %>% summarize(Avg.avg = mean(Avg, na.rm=TRUE),
                                                              RC.avg = mean(RC, na.rm=TRUE),
                                                             OBP.avg = mean((OBP/100),na.rm=TRUE),
                                                             SLG.avg = mean(SLG, na.rm=TRUE),
                                                             OPS.avg = mean(OPS, na.rm=TRUE),
                                                             ISO.avg = mean(ISO, na.rm=TRUE),
                                                             BABIP.avg = mean(BABIP, na.rm=TRUE)) %>% as.data.frame()

Batting.plot <- plot_ly(Batting, x=~YEAR_ID, y=~OBP.avg, name = "OBP", type="scatter", mode="lines") %>%
  add_trace(y=~SLG.avg, name="SLG", type="scatter", mode="lines+markers") %>%
  add_trace(y=~OPS.avg, name="OPS", type="scatter", mode="lines") %>%
  add_trace(y=~ISO.avg, name="ISO", type="scatter", line=list(width = 4, dash = 'dash')) %>%
  add_trace(y=~BABIP.avg, name="BABIP", type="scatter", line=list(width = 4, dash = 'dot')) %>%
  add_trace(y=~Avg.avg, name="Avg", type="scatter", mode="lines+marker") %>%
  layout(title="Batting Statistics per Year",
         xaxis=list(title="Year"),
         yaxis=list(title="Averages"))

Batting.plot <- ggplot(Batting) + labs(x="Year", y="Averages", title="Batting Statistics per Year") + 
  geom_line(aes(YEAR_ID, Avg.avg, color="Avg")) + geom_line(aes(YEAR_ID, OBP.avg, color="OBP")) + 
  geom_line(aes(YEAR_ID, SLG.avg, color="SLG")) + geom_line(aes(YEAR_ID, OPS.avg, color="OPS")) +
  geom_line(aes(YEAR_ID, ISO.avg, color="ISO")) + geom_line(aes(YEAR_ID, BABIP.avg, color="BABIP"))

Batting.plot + theme_light()

Batting.plot 

# perform linear regressions
linearModel <- lm(formula = RC ~ BABIP + OBP + SLG + OPS + ISO + BAT_HAND_CD, data = Batting.active)
summary(linearModel)

  #BABIP is statistically significant in determining Runs Created
  #Being Right Handed and Left Handed are statistically significant in creating runs
  #Omitted Variable Bias doesn't apply here because BABIP is still statistically significant even after the addition of BAT_HAND_CD


# Career Leaders
Batting.Career <- Batting.active %>% group_by(BAT_ID, YEAR_ID) %>% 
  filter(BABIP>=quantile(BABIP, 0.25, na.rm=TRUE),
         BABIP<=quantile(BABIP, 0.75, na.rm=TRUE), 
         ISO>=quantile(ISO, 0.25, na.rm=TRUE),
         ISO<=quantile(ISO, 0.75, na.rm=TRUE),
         OPS>=quantile(OPS, 0.25, na.rm=TRUE),
         OPS<=quantile(OPS, 0.75, na.rm=TRUE),
         SLG>=quantile(SLG, 0.25, na.rm=TRUE),
         SLG<=quantile(SLG, 0.75, na.rm=TRUE),
         OBP>=quantile(OBP, 0.25, na.rm=TRUE),
         OBP<=quantile(OBP, 0.75, na.rm=TRUE),
         RC>=quantile(RC, 0.25, na.rm=TRUE),
         RC<=quantile(RC, 0.75, na.rm=TRUE)) %>% as.data.frame()

Batting.Career.Leaders <- Batting.Career %>% group_by(BAT_ID) %>% 
  summarize(RC.avg = mean(RC, na.rm=TRUE),
            OBP.avg = mean((OBP/100),na.rm=TRUE),
            SLG.avg = mean(SLG, na.rm=TRUE),
            OPS.avg = mean(OPS, na.rm=TRUE),
            ISO.avg = mean(ISO, na.rm=TRUE),
            BABIP.avg = mean(BABIP, na.rm=TRUE)) %>% as.data.frame()

Batting.Career.Leaders <- merge(Batting.Career.Leaders, Player, by="BAT_ID")

#Career Leaders by Metric
Batting.Career.Leaders.BABIP <- Batting.Career.Leaders %>% arrange(desc(BABIP.avg)) %>% head(10)
Batting.Career.Leaders.ISO <- Batting.Career.Leaders %>% arrange(desc(ISO.avg)) %>% head(10)
Batting.Career.Leaders.OPS <- Batting.Career.Leaders %>% arrange(desc(OPS.avg)) %>% head(10)
Batting.Career.Leaders.SLG <- Batting.Career.Leaders %>% arrange(desc(SLG.avg)) %>% head(10)
Batting.Career.Leaders.OBP <- Batting.Career.Leaders %>% arrange(desc(OBP.avg)) %>% head(10)
Batting.Career.Leaders.RC <- Batting.Career.Leaders %>% arrange(desc(RC.avg)) %>% head(10)

#Active Roster Qualifiers
Batting.Career.Active <- Batting.Career %>% group_by(BAT_ID, YEAR_ID) %>% filter(YEAR_ID >= "2008-01-01",
                                                                                 YEAR_ID <= "2014-01-01") %>% as.data.frame()

  #Average out the multi-appearing players

Batting.Career.Active.Player <- Batting.Career.Active %>% group_by(BAT_ID, BAT_HAND_CD) %>%
  summarize(RC.avg = mean(RC, na.rm=TRUE),
            OBP.avg = mean((OBP/100),na.rm=TRUE), 
            SLG.avg = mean(SLG, na.rm=TRUE), 
            OPS.avg = mean(OPS, na.rm=TRUE), 
            ISO.avg = mean(ISO, na.rm=TRUE), 
            BABIP.avg = mean(BABIP, na.rm=TRUE)) %>% as.data.frame()

Batting.Career.Active.Player <- merge(Batting.Career.Active.Player, Player, by="BAT_ID")

# Top 25 by Runs created avg
Batting.Career.Active.Player.Final <- Batting.Career.Active.Player %>% arrange(desc(RC.avg)) %>% head(25)
col <- c("BAT_ID", "BAT_HAND_CD", "RC", "OBP", "SLG", "OPS", "ISO", "BABIP", "First", "Last", "Debut")
names(Batting.Career.Active.Player.Final) <- col

# Predictions

Trout <- data.frame(Batting.Career.Active.Player.Final[1,2:8])
Votto <- data.frame(Batting.Career.Active.Player.Final[2,2:8])
Cabrera <- data.frame(Batting.Career.Active.Player.Final[3,2:8])
McCutchen <- data.frame(Batting.Career.Active.Player.Final[4,2:8])
Fielder <- data.frame(Batting.Career.Active.Player.Final[5,2:8])
Bautista <- data.frame(Batting.Career.Active.Player.Final[6,2:8])
Goldschmidt <- data.frame(Batting.Career.Active.Player.Final[72:8])
Braun <- data.frame(Batting.Career.Active.Player.Final[8,2:8])
Abreu <- data.frame(Batting.Career.Active.Player.Final[9,2:8])
Cano <- data.frame(Batting.Career.Active.Player.Final[10,2:8])


Trout.model <- predict(linearModel, newdata = Trout)
Votto.model <- predict(linearModel, newdata = Votto)
Cabrera.model <- predict(linearModel, newdata = Cabrera)
McCuthcen.model <- predict(linearModel, newdata = McCutchen)
Fielder.model <- predict(linearModel, newdata = Fielder)
Bautista.model <- predict(linearModel, newdata = Bautista)
Goldschmidt.model <- predict(linearModel, newdata = Goldschmidt)
Braun.model <- predict(linearModel, newdata = Braun)
Abreu.model <- predict(linearModel, newdata = Abreu)
Cano.model <- predict(linearModel, newdata = Cano)


Batting.Career.Active.Future <- Batting.Career %>% group_by(BAT_ID, YEAR_ID) %>% filter(YEAR_ID == "2015-01-01") %>% as.data.frame()

#Average out the multi-appearing players

Batting.Career.Active.Player.Future <- Batting.Career.Active %>% group_by(BAT_ID, BAT_HAND_CD) %>%
  summarize(RC.avg = mean(RC, na.rm=TRUE),
            OBP.avg = mean((OBP/100),na.rm=TRUE), 
            SLG.avg = mean(SLG, na.rm=TRUE), 
            OPS.avg = mean(OPS, na.rm=TRUE), 
            ISO.avg = mean(ISO, na.rm=TRUE), 
            BABIP.avg = mean(BABIP, na.rm=TRUE)) %>% as.data.frame()

Batting.Career.Active.Player.Future <- merge(Batting.Career.Active.Player.Future, Player, by="BAT_ID")

#Actually used in predict
Trout <- data.frame(Batting.Career.Active.Player.Final[1,2:8])
Cabrera <- data.frame(Batting.Career.Active.Player.Final[2,2:8])
Fielder <- data.frame(Batting.Career.Active.Player.Final[3,2:8])

Trout.model <- predict(linearModel, newdata = Trout)
Cabrera.model <- predict(linearModel, newdata = Cabrera)
Fielder.model <- predict(linearModel, newdata = Fielder)

Trout.Future <- Batting.Career.Active.Player.Future %>% filter(BAT_ID == "troum001")
Cabrera.Future <- Batting.Career.Active.Player.Future %>% filter(BAT_ID == "cabrm001")
Fielder.Future <- Batting.Career.Active.Player.Future %>% filter(BAT_ID == "fielp001")



################################################################################################################################################################
################################################################################################################################################################


# Pitching Statistics
#5 Starting Pitchers
#7 relief pitchers

# merge the pitching and master datasets in Lahman's DB.
Pitching.small <- merge(Pitching.file, master, by="playerID")

# rename them & eliminate the "playerID" column
col <- c("playerID", "YEAR_ID", "W", "L", "G", "GS", "CG", "SHO", "SV", "IPouts", "ER", "BFP", "GF", "R", "PIT_ID")
names(Pitching.small)<- col
Pitching.small <- Pitching.small %>% select(PIT_ID, YEAR_ID, W, L, G, GS, CG, SHO, SV, IPouts, ER, BFP, GF, R)

# Summarize Pitching Statistics
Pitching.events <- data.events %>% group_by(YEAR_ID, PIT_ID, PIT_HAND_CD) %>% 
  summarize(AB=sum(ab), BB=sum(bb), BK=sum(bk), FB=sum(flyBall), GB=sum(groundBall), GIDP=sum(gidp),
            H=sum(h), SF=sum(sf), HBP=sum(hbp), HR=sum(hr), IBB=sum(ibb), K=sum(k), WP=sum(wp)) %>% as.data.frame()

# Class Declarations
Pitching.events$PIT_ID <- factor(Pitching.events$PIT_ID)
Pitching.events$YEAR_ID <- factor(Pitching.events$YEAR_ID)

# Merge
Pitching.Active <- merge(Pitching.events, Pitching.small, by=c("PIT_ID","YEAR_ID"))

# Mutate variables
Pitching.Active <- Pitching.Active %>% group_by(YEAR_ID, PIT_ID, PIT_HAND_CD) %>%
  mutate(PIT = (round((3.3*BFP) + (1.5*K) + (2.2*BB),3)),
         IP = (round((IPouts/3),3)),
         ERA = round((9 * (ER/IP)),3),
         PTB = round((0.89 * (1.255 * (H - HR) + 4 * HR) + 0.56 * (BB + HBP - IBB)),3),
         ERC = round(((9 * (((H + BB + HBP) * PTB)/(BFP * IP)) - 0.56)),3),
         GFRatio = round((GB/FB),3),
         HNineIP = round((H/9),3),
         IPGS = round((IP/GS),3),
         KNine = round((K/9),3),
         KBB = round((K/BB),3),
         PFR = round(((K+BB)/IP),3),
         QS = sum(ifelse((IP>=6 && ER<=3), 1, 0)),
         RA = round(((R*9)/IP),3),
         WS = (W+SV),
         WHIP = round(((BB+H)/IP),3),
         LOBpct = round(((H+BB+HBP-R)/(H+BB+HBP-(1.4*HR))),3) ,
         DICE = round((3.00 + (((13 * HR) + 3*(BB + HBP)- (2 * K))/IP)),3),
         FIP = round((((13 * HR) + (3 * BB) - (2 * K))/IP),3)) %>% as.data.frame()

Pitching.Active <- Pitching.Active %>% filter(PIT>=mean(PIT, na.rm=TRUE), R<=mean(R, na.rm=TRUE), IP>=mean(IP, na.rm=TRUE))
# Class Declaration
Pitching.Active$YEAR_ID <- ymd(paste(Pitching.Active$YEAR_ID, "-01-01", sep = ""))

# Scatter Plot : Plot the relationship of RA to each metric over time
plotPitching <- plot_ly(data=Pitching.Active, x=~WHIP, y=~RA, z=~YEAR_ID,
                        text=~paste("WHIP: ", WHIP, "<br>RA: ", RA, "<br>Year: ", YEAR_ID),
                        color = ~WHIP, RA = ~WHIP,
                        colors = c("#2f5973", "#f2edd5")) %>% layout(title="Run Averages by Walks & Hits (per Innings Pitched)", yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotPitching

plotPitching2 <- plot_ly(data=Pitching.Active, x=~LOBpct, y=~RA, z=~YEAR_ID,
                        text=~paste("LOB%: ", LOBpct, "<br>RA: ", RA, "<br>Year: ", YEAR_ID),
                        color = ~LOBpct, RA = ~LOBpct,
                        colors = c("#f2a25c", "#402711")) %>% layout(title="Run Averages by Left on Base Percentage", yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotPitching2

plotPitching3 <- plot_ly(data=Pitching.Active, x=~DICE, y=~RA, z=~YEAR_ID,
                        text=~paste("DICE: ", DICE, "<br>RA: ", RA, "<br>Year: ", YEAR_ID),
                        color = ~DICE, RA = ~DICE,
                        colors = c("#f2edd5", "#402711")) %>% layout(title="Run Averages by Defense-Independent Component ERA", yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotPitching3

plotPitching4 <- plot_ly(data=Pitching.Active, x=~FIP, y=~RA, z=~YEAR_ID,
                        text=~paste("FIP: ", FIP, "<br>RA: ", RA, "<br>Year: ", YEAR_ID),
                        color = ~FIP, RA = ~FIP,
                        colors = c("#a6662d", "#2f5973")) %>% layout(title="Run Averages by Fielding Independent Pitching", yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotPitching4

# Boxplot : Distribution of select Metrics over Time
plotPitching5 <- ggplot(Pitching.Active) + labs(x="Year", y="WHIP") + geom_boxplot(aes(YEAR_ID, WHIP, color=factor(YEAR_ID)), outlier.shape = 2) + coord_flip()
plotPitching6 <- ggplot(Pitching.Active) + labs(x="Year", y="LOB%") + geom_boxplot(aes(YEAR_ID, LOBpct, color=factor(YEAR_ID)), outlier.shape = 2) + coord_flip()
plotPitching7 <- ggplot(Pitching.Active) + labs(x="Year", y="DICE") + geom_boxplot(aes(YEAR_ID, DICE, color=factor(YEAR_ID)), outlier.shape = 2) + coord_flip()
plotPitching8 <- ggplot(Pitching.Active) + labs(x="Year", y="FIP") + geom_boxplot(aes(YEAR_ID, FIP, color=factor(YEAR_ID)), outlier.shape = 2) + coord_flip()

linearModel2 <- lm(formula = RA ~ PIT + LOBpct + FIP + WHIP + DICE + PIT_HAND_CD, data = Pitching.Active)
summary(linearModel2)

# Career Leaders
Pitching.Career <- Pitching.Active %>% group_by(PIT_ID, YEAR_ID, PIT_HAND_CD) %>%
  filter(PIT>=quantile(PIT, 0.25, na.rm=TRUE),
         PIT<=quantile(PIT, 0.75, na.rm=TRUE),
         WHIP>=quantile(WHIP, 0.25, na.rm=TRUE),
         WHIP<=quantile(WHIP, 0.75, na.rm=TRUE),
         LOBpct>=quantile(LOBpct, 0.25, na.rm=TRUE),
         LOBpct<=quantile(LOBpct, 0.75, na.rm=TRUE),
         DICE>=quantile(DICE, 0.25, na.rm=TRUE),
         DICE<=quantile(DICE, 0.75, na.rm=TRUE),
         FIP>=quantile(FIP, 0.25, na.rm=TRUE),
         FIP<=quantile(FIP, 0.75, na.rm=TRUE)) %>% as.data.frame()

Pitching.Career.Leaders <- Pitching.Career %>% group_by(PIT_ID, PIT_HAND_CD) %>%
  summarise(PIT.avg = mean(PIT, na.rm=TRUE),
            RA.avg = mean(RA, na.rm=TRUE),
            WHIP.avg = mean(WHIP, na.rm=TRUE),
            LOBpct.avg = mean(LOBpct, na.rm=TRUE),
            DICE.avg = mean(DICE, na.rm=TRUE),
            FIP.avg = mean(FIP, na.rm=TRUE)) %>% as.data.frame()

colPC <- c("BAT_ID", "PIT_HAND_CD", "PIT", "RA", "WHIP", "LOBpct", "DICE", "FIP")
names(Pitching.Career.Leaders) <- colPC
Pitching.Career.Leaders <- merge(Pitching.Career.Leaders, Player, by="BAT_ID")

# Career Leaders by Metric
Pitching.Career.Leaders.WHIP <- Pitching.Career.Leaders %>% arrange(WHIP) %>% head(10)
Pitching.Career.Leaders.LOBpct <- Pitching.Career.Leaders %>% arrange(desc(LOBpct)) %>% head(10)
Pitching.Career.Leaders.DICE <- Pitching.Career.Leaders %>% arrange(DICE) %>% head(10)
Pitching.Career.Leaders.FIP <- Pitching.Career.Leaders %>% arrange(FIP) %>% head(10)

#Active Roster Qualifiers
Pitching.Career.Active <- Pitching.Career %>% group_by(PIT_ID, YEAR_ID, PIT_HAND_CD) %>% filter(YEAR_ID >= "2008-01-01",
                                                                                                YEAR_ID <= "2014-01-01") %>% as.data.frame()

#Average out the multi-appearing players
Pitching.Career.Active.Player <- Pitching.Career.Active %>% group_by(PIT_ID, PIT_HAND_CD) %>%
  summarize(PIT.avg = mean(PIT, na.rm=TRUE),
            RA.avg = mean(RA, na.rm=TRUE),
            WHIP.avg = mean(WHIP, na.rm=TRUE),
            LOBpct.avg = mean(LOBpct, na.rm=TRUE),
            DICE.avg = mean(DICE, na.rm=TRUE),
            FIP.avg = mean(FIP, na.rm=TRUE)) %>% as.data.frame()

#Top 10 Active Pitchers
Pitching.Career.Active.Player.WHIP <- Pitching.Career.Active.Player %>% group_by(PIT_ID) %>% arrange(WHIP.avg) %>% head(10)
Pitching.Career.Active.Player.LOBpct <- Pitching.Career.Active.Player %>% group_by(PIT_ID) %>% arrange(desc(LOBpct.avg)) %>% head(10)
Pitching.Career.Active.Player.FIP <- Pitching.Career.Active.Player %>% group_by(PIT_ID) %>% arrange(FIP.avg) %>% head(10)
Pitching.Career.Active.Player.DICE <- Pitching.Career.Active.Player %>% group_by(PIT_ID) %>% arrange(DICE.avg) %>% head(10)


Pitching.Active.Final <- rbind(Pitching.Career.Active.Player.WHIP, Pitching.Career.Active.Player.LOBpct, Pitching.Career.Active.Player.FIP, Pitching.Career.Active.Player.DICE)
Pitching.Active.Final <- Pitching.Active.Final %>% group_by(PIT_ID, PIT_HAND_CD) %>% summarize(PIT.avg = mean(PIT.avg, na.rm=TRUE),
                                                                                               RA.avg = mean(RA.avg, na.rm=TRUE),
                                                                                               WHIP.avg = mean(WHIP.avg, na.rm=TRUE),
                                                                                               LOBpct.avg = mean(LOBpct.avg, na.rm=TRUE),
                                                                                               DICE.avg = mean(DICE.avg, na.rm=TRUE),
                                                                                               FIP.avg = mean(FIP.avg, na.rm=TRUE)) %>% as.data.frame()


colPitch <- c("BAT_ID", "PIT_HAND_CD", "PIT", "RA", "WHIP", "LOBpct", "DICE", "FIP")
names(Pitching.Active.Final) <- colPitch
Pitching.Active.Final <- merge(Pitching.Active.Final, Player, by="BAT_ID")

# Futures

Pitching.Career.Active.Future <- Pitching.Career %>% group_by(PIT_ID, YEAR_ID, PIT_HAND_CD) %>% filter(YEAR_ID >= "2015-01-01") %>% as.data.frame()

#Average out the multi-appearing players
Pitching.Career.Active.Player.Future <- Pitching.Career.Active %>% group_by(PIT_ID, PIT_HAND_CD) %>%
  summarize(PIT.avg = mean(PIT, na.rm=TRUE),
            RA.avg = mean(RA, na.rm=TRUE),
            WHIP.avg = mean(WHIP, na.rm=TRUE),
            LOBpct.avg = mean(LOBpct, na.rm=TRUE),
            DICE.avg = mean(DICE, na.rm=TRUE),
            FIP.avg = mean(FIP, na.rm=TRUE)) %>% as.data.frame()

colPitch <- c("BAT_ID", "PIT_HAND_CD", "PIT", "RA", "WHIP", "LOBpct", "DICE", "FIP")
names(Pitching.Career.Active.Player.Future) <- colPitch
Pitching.Career.Active.Player.Future <- merge(Pitching.Career.Active.Player.Future, Player, by="BAT_ID")

# Predictions Arranged by WHIP
Betances <- data.frame(Pitching.Active.Final[3,2:7])
Chapman <- data.frame(Pitching.Active.Final[5,2:7])
Davis <- data.frame(Pitching.Active.Final[6,2:7])

Betances.model <- predict(linearModel2, newdata = Betances)
Chapman.model <- predict(linearModel2, newdata = Chapman)
Davis.model <- predict(linearModel2, newdata = Davis)

Betances.Future <- Pitching.Career.Active.Player.Future %>% filter(BAT_ID == "betad001")
Chapman.Future <- Pitching.Career.Active.Player.Future %>% filter(BAT_ID == "chapa001")
Davis.Future <- Pitching.Career.Active.Player.Future %>% filter(BAT_ID == "daviw001")



################################################################################################################################################################
################################################################################################################################################################


# Fielding Statistics
#4~5 outfielders
#1B, 2B, Shortstop, 3B, Right, Center, Left, Catcher 

# select
Fielding.active <- Fielding.file %>% select(BAT_ID, POS, YEAR_ID, InnOuts, Assists) %>% group_by(BAT_ID, POS, YEAR_ID) %>% summarize(InnOuts = sum(InnOuts), Assists = sum(Assists)) %>% as.data.frame()

# Summarize
Fielding.events <- data.events %>% group_by(BAT_ID, YEAR_ID) %>% 
  summarize(Err = sum(ERR_CT), 
            PU = sum(PO1_FLD_CD + PO2_FLD_CD + PO3_FLD_CD),
            DP = sum(gidp), TP = sum(tp), 
            PB = sum(pb), 
            H = sum(h), 
            K = sum(k),
            BB = sum(bb), HR = sum(hr), HBP = sum(hbp), PA = sum(pa), ROE = sum(roe)) %>% as.data.frame()

# Class Declaration
Fielding.events$YEAR_ID <- factor(Fielding.events$YEAR_ID)

# Merge
Fielding.active <- merge(Fielding.active, Fielding.events, by=c("BAT_ID","YEAR_ID"))

# Mutate Variables
Fielding.active <- Fielding.active %>% group_by(BAT_ID, POS, YEAR_ID) %>% filter(InnOuts>0, Assists>0, PU>0, Err>0, PA>0, HR>0, BB>0, HBP>0, K>0) %>%
  mutate(TC = (Assists + PU + Err),
         FP = round((PU + Assists)/(TC),3),
         IP = round((InnOuts/3),3),
         RF = round(((9*(PU + Assists))/IP),3),
         DER = round((1 - ((H + ROE - HR) / (PA - BB - K - HBP - HR))),3)) %>% as.data.frame()

#Class declarations
Fielding.active$BAT_ID <- factor(Fielding.active$BAT_ID)
Fielding.active$POS <- factor(Fielding.active$POS, labels = c("1B", "2B", "3B", "C", "CF", "LF", "OF", "P", "RF", "SS"))

# Scatter Plot : Relatioships
plotFielding <- plot_ly(data = Fielding.active, x=~TC, y=~DER, z=~YEAR_ID,
                        text= ~paste("TC: ", TC, "<br>DER: ", DER, "<br>Year: ", YEAR_ID),
                        color = ~TC, DER= ~TC,
                        colors = c("#1abc9c", "#bdc3c7")) %>% layout(title = 'Deffensive Efficiency Ration by Total Chances (Assists + Errors + Put Outs)', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotFielding

plotFielding2 <- plot_ly(data = Fielding.active, x=~FP, y=~DER, z=~YEAR_ID,
                        text= ~paste("FP: ", FP, "<br>DER: ", DER, "<br>Year: ", YEAR_ID),
                        color = ~FP, DER= ~FP,
                        colors = c("#ffeb3b", "#8c3b0c")) %>% layout(title = 'Deffensive Efficiency Ration by Fielding Percentage', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotFielding2

plotFielding3 <- plot_ly(data = Fielding.active, x=~RF, y=~DER, z=~YEAR_ID,
                        text= ~paste("RF: ", RF, "<br>DER: ", DER, "<br>Year: ", YEAR_ID),
                        color = ~RF, DER= ~RF,
                        colors = c("#f25170", "#000b0d")) %>% layout(title = 'Deffensive Efficiency Ration by Range Factor', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE))
plotFielding3


  #DER and TC have a negative linear relationship, becuase the more chances a team has to create an out on a batted ball, the lower their DER
  # is. Therefore, I want the Interquartile range as my top careerers' and for my roster

# Boxplot : Distribution of each metric over time
plotFielding4 <- ggplot(Fielding.active) + labs(x="Year", y="Total Chances") + geom_boxplot(aes(YEAR_ID, TC, color=factor(YEAR_ID)), outlier.shape = 3) + coord_flip()
plotFielding5 <- ggplot(Fielding.active) + labs(x="Year", y="Field Percentage") + geom_boxplot(aes(YEAR_ID, FP, color=factor(YEAR_ID)), outlier.shape = 3) + coord_flip()
plotFielding6 <- ggplot(Fielding.active) + labs(x="Year", y="Range Factor") + geom_boxplot(aes(YEAR_ID, RF, color=factor(YEAR_ID)), outlier.shape = 3) + coord_flip()
plotFielding7 <- ggplot(Fielding.active) + labs(x="Year", y="Defense Effectiveness Ratio") + geom_boxplot(aes(YEAR_ID, DER, color=factor(YEAR_ID)), outlier.shape = 3) + coord_flip()

Fielding.Active.Quantile <- Fielding.active %>% group_by(YEAR_ID) %>% 
  summarize(Q25.DER = quantile(DER, 0.25, na.rm=TRUE),
            Med.DER = median(DER, na.rm=TRUE),
            Q75.DER = quantile(DER, 0.75, na.rm=TRUE)) %>% as.data.frame()

plot_ly(data=Fielding.Active.Quantile, x=~YEAR_ID, y=~Q75.DER, type = "scatter", mode="lines",
        line = list(color = 'rgb(0,100,80,0.5)'),
        showlegend = TRUE, name="Upper Quartile") %>%
  add_trace(y=~Q25.DER, type = "scatter", mode="lines",
            fill="tonexty", fillcolor="rgba(0,100,80,0.2)", 
            line=list(color = 'rgb(0,100,80,0.5)'),
            showlegend = TRUE, name = "Lower Quartile") %>%
  add_trace(x=~YEAR_ID, y=~Med.DER, type = "scatter", mode="lines",
            line = list(color='rgb(14, 0, 246)'),
            showlegend = TRUE, name="Median") %>%
  layout(title = "Inter Quartile Range of <br>DER",
         xaxis = list(title="Year",
                      zeroline = FALSE),
         yaxis = list(title="Defensive Efficiency Ratio",
                      zeroline = FALSE))

#Career Leaders
Fielding.Career <- Fielding.active %>% group_by(BAT_ID, POS) %>% 
  filter(DER>=quantile(DER, 0.25, na.rm=TRUE),
         DER<=quantile(DER, 0.75, na.rm=TRUE),
         TC>=quantile(TC, 0.25, na.rm=TRUE),
         TC<=quantile(TC, 0.75, na.rm=TRUE),
         FP>=quantile(FP, 0.25, na.rm=TRUE),
         FP<=quantile(FP, 0.75, na.rm=TRUE),
         RF>=quantile(RF, 0.25, na.rm=TRUE),
         RF<=quantile(RF, 0.75, na.rm=TRUE)) %>% as.data.frame()

#Box plot of DER by Position of each year
ggplot(Fielding.Career) + geom_boxplot(aes(POS, DER)) + facet_wrap(~YEAR_ID)

Fielding.Career.Leaders <- Fielding.Career %>% group_by(BAT_ID, POS) %>%
  summarise(DER.avg = mean(DER, na.rm=TRUE),
            TC.avg = mean(TC, na.rm=TRUE),
            FP.avg = mean(FP, na.rm=TRUE),
            RF.avg = mean(RF, na.rm=TRUE)) %>% arrange(desc(DER.avg)) %>% head(25)

# Career Leaders by Metric
Fielding.Career.Leaders.DER <- Fielding.Career.Leaders %>% arrange(desc(DER.avg)) %>% head(10)
Fielding.Career.Leaders.TC <- Fielding.Career.Leaders %>% arrange(desc(TC.avg)) %>% head(10)
Fielding.Career.Leaders.FP <- Fielding.Career.Leaders %>% arrange(desc(FP.avg)) %>% head(10)
Fielding.Career.Leaders.RF <- Fielding.Career.Leaders %>% arrange(desc(RF.avg)) %>% head(10)


# Active Roster Qualifiers
Fielding.Career.Active <- Fielding.Career %>% group_by(BAT_ID, POS) %>% 
          filter(YEAR_ID>="2008-01-01",
                 YEAR_ID<="2014-01-01") %>% arrange(desc(DER)) %>% as.data.frame()

Fielding.Career.Active <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% 
  summarize(DER.avg = mean(DER, na.rm=TRUE),
            FP.avg = mean(FP, na.rm=TRUE),
            IP.avg = mean(IP, na.rm=TRUE),
            TC.avg = mean(TC, na.rm=TRUE),
            Err.avg = mean(Err, na.rm=TRUE),
            PU.avg = mean(PU, na.rm=TRUE),
            PA.avg = mean(PA, na.rm=TRUE),
            H.avg = mean(H, na.rm=TRUE),
            HR.avg = mean(HR, na.rm=TRUE),
            BB.avg = mean(BB, na.rm=TRUE),
            HBP.avg = mean(HBP, na.rm=TRUE),
            K.avg = mean(K, na.rm=TRUE),
            PB.avg = mean(PB, na.rm=TRUE),
            DP.avg = mean(DP, na.rm=TRUE),
            TP.avg = mean(TP, na.rm=TRUE),
            RF.avg = mean(RF, na.rm=TRUE)) %>% as.data.frame()

colFA <- c("BAT_ID", "POS", "DER", "FP", "IP", "TC", "Err", "PU", "PA", "H", "HR", "BB", "HBP", "K", "PB", "DP", "TP", "RF")
names(Fielding.Career.Active) <- colFA

# Math the active roster's actual performance to their expected performance
Fielding.Career.Active.Future <- Fielding.Career %>% group_by(BAT_ID, POS) %>% 
  filter(YEAR_ID>="2015-01-01") %>% arrange(desc(DER)) %>% as.data.frame()

Fielding.Career.Active.Future <- Fielding.Career.Active.Future %>% group_by(BAT_ID, POS) %>% 
  summarize(DER.avg = mean(DER, na.rm=TRUE),
            FP.avg = mean(FP, na.rm=TRUE),
            IP.avg = mean(IP, na.rm=TRUE),
            TC.avg = mean(TC, na.rm=TRUE),
            Err.avg = mean(Err, na.rm=TRUE),
            PU.avg = mean(PU, na.rm=TRUE),
            PA.avg = mean(PA, na.rm=TRUE),
            H.avg = mean(H, na.rm=TRUE),
            HR.avg = mean(HR, na.rm=TRUE),
            BB.avg = mean(BB, na.rm=TRUE),
            HBP.avg = mean(HBP, na.rm=TRUE),
            K.avg = mean(K, na.rm=TRUE),
            PB.avg = mean(PB, na.rm=TRUE),
            DP.avg = mean(DP, na.rm=TRUE),
            TP.avg = mean(TP, na.rm=TRUE),
            RF.avg = mean(RF, na.rm=TRUE)) %>% as.data.frame()

colFA <- c("BAT_ID", "POS", "DER", "FP", "IP", "TC", "Err", "PU", "PA", "H", "HR", "BB", "HBP", "K", "PB", "DP", "TP", "RF")
names(Fielding.Career.Active.Future) <- colFA


# Linear Regression
linearModel3 <- lm(formula = DER ~ FP + IP + TC + Err + PU + PA + H + HR + BB + HBP + K + PB + DP + TP + RF + POS, data = Fielding.active)
summary(linearModel3)

Fielding.Career.Active <- merge(Fielding.Career.Active, Player, by="BAT_ID")

Fielding.Career.Active.POS1B <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "1B") %>% arrange(desc(DER)) %>% head(2)
Fielding.Career.Active.POS2B <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "2B") %>% arrange(desc(DER)) %>% head(2)
Fielding.Career.Active.POS3B <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "3B") %>% arrange(desc(DER)) %>% head(2)
Fielding.Career.Active.POSC <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "C") %>% arrange(desc(DER)) %>% head(2)
Fielding.Career.Active.POSCF <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "CF") %>% arrange(desc(DER)) %>% head(2)
Fielding.Career.Active.POSLF <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "LF") %>% arrange(desc(DER)) %>% head(2)
Fielding.Career.Active.POSOF <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "OF") %>% arrange(desc(DER)) %>% head(5)
Fielding.Career.Active.POSRF <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "RF") %>% arrange(desc(DER)) %>% head(2)
Fielding.Career.Active.POSSS <- Fielding.Career.Active %>% group_by(BAT_ID, POS) %>% filter(POS == "SS") %>% arrange(desc(DER)) %>% head(2)

#List the Field roster excluding pitcher
Fielding.Career.Active.Final <- rbind(Fielding.Career.Active.POS1B, Fielding.Career.Active.POS2B, Fielding.Career.Active.POS3B,
                                      Fielding.Career.Active.POSC, Fielding.Career.Active.POSCF, Fielding.Career.Active.POSLF,
                                      Fielding.Career.Active.POSOF, Fielding.Career.Active.POSRF, Fielding.Career.Active.POSSS)

# Predictions
Laird.1b <- data.frame(Fielding.Career.Active.POS1B[1,2:18])
Arencibia.1b <- data.frame(Fielding.Career.Active.POS1B[2,2:18])
Pena.2B <- data.frame(Fielding.Career.Active.POS2B[1,2:18])
Walters.2b
Pena.3B
Laird.3B
Phillips.C
Kratz.C

Laird.1b.model <- predict(linearModel3, newdata = Laird.1b)

  
################################################################################################################################################################
################################################################################################################################################################


# Average Salary 

#create a budget to pay for the roster.
fileSalary <- "lahmanDB/2015/baseballdatabank-master/core/Salaries.csv"
Salary.data <- fread(fileSalary, data.table = FALSE, showProgress = FALSE, na.strings = "NULL")
Salary.data <- merge(master, Salary.data, by="playerID")

Salary.data <- Salary.data %>% select(yearID, retroID, teamID, lgID, salary) %>% filter(yearID >= 1976) %>% as.data.frame()
colSalary <- c("YEAR_ID", "BAT_ID", "BAT_TEAM_ID", "League", "Salary")
names(Salary.data) <- colSalary

#Class Decalaration
Salary.data$League <- factor(Salary.data$League)
Salary.data$BAT_TEAM_ID <- factor(Salary.data$BAT_TEAM_ID)
Salary.data$BAT_ID <- factor(Salary.data$BAT_ID)
Salary.data$YEAR_ID <- ymd(paste(Salary.data$YEAR_ID, "-01-01", sep=""))
Salary.data$Salary <- as.numeric(Salary.data$Salary)

str(Salary.data, max.level=1)

Salary.League <- Salary.data %>% group_by(League) %>% summarize(Avg.Salary = round(mean(Salary, na.rm=TRUE),2)) %>% as.data.frame()
Salary.Team <- Salary.data %>% group_by(BAT_TEAM_ID) %>% summarize(Avg.Salary = round(mean(Salary, na.rm=TRUE),2)) %>% as.data.frame()
Salary.Player <- Salary.data %>% group_by(BAT_ID) %>% summarize(Avg.Salary = round(mean(Salary, na.rm=TRUE),2)) %>% as.data.frame()


################################################################################################################################################################
################################################################################################################################################################


# How much will each player cost?

Fielding.Salary <- merge(Fielding.Career.Active.Final, Salary.Player, by="BAT_ID")
Pitching.Salary <- merge(Pitching.Active.Final, Salary.Player, by="BAT_ID")
Batting.Salary <- merge(Batting.Career.Active.Player.Final, Salary.Player, by="BAT_ID")


