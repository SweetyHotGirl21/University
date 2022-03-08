######
# Before this script can be run, the packages, variables and functions must be loaded from "BA_BCHerbert_MAIN" into the R session
######
### Premanent and Temporary Components
#In this script the test of Farma and French is prepared and apllied to my data

#### Alphabet
### 5-minute return
Google.ts.lm.5min = lm(Google.ts[,3]~lag(Google.ts[,3]))
Google.ts.lm.5min = lm(Google.ts[1:1579,3]~Google.ts[2:1580,3])
Google.ts.lm.5min$coefficients
summary(Google.ts.lm.5min)

head(lag(Google.ts[,3]))

### 30-minute returns overlapping
Google_Return.30 = c(rep(NA, 6), diff(log(DF[,3]), lag = 6))[80:1659]
Google.ts.lm.30min = lm(Google_Return.30[1:1579]~Google_Return.30[2:1580])
Google.ts.lm.30min$coefficients
summary(Google.ts.lm.30min)

### 60-minute returns overlapping
Google_Return.60 = c(rep(NA, 12), diff(log(DF[,3]), lag = 12))[80:1659]
Google.ts.lm.60min = lm(Google_Return.60[1:1569]~Google_Return.60[12:1580])
Google.ts.lm.60min$coefficients
summary(Google.ts.lm.60min)

### 120-minute returns overlapping
Google_Return.120 = c(rep(NA, 24), diff(log(DF[,3]), lag = 24))[80:1659]
Google.ts.lm.120min = lm(Google_Return.120[1:1557]~Google_Return.120[24:1580]) ####
Google.ts.lm.120min$coefficients
summary(Google.ts.lm.120min)

### one day returns overlapping
Google_Return.1d = c(rep(NA, 80), diff(log(DF[,3]), lag = 80))[80:1659]
Google.ts.lm.1d = lm(Google_Return.1d[1:1501]~Google_Return.1d[80:1580])
Google.ts.lm.1d$coefficients
summary(Google.ts.lm.1d)

### two day returns overlapping
Google_Return.2d = c(rep(NA, 160), diff(log(DF[,3]), lag = 160))[80:1659]
Google.ts.lm.2d = lm(Google_Return.2d[1:1421]~Google_Return.2d[160:1580])
Google.ts.lm.2d$coefficients
summary(Google.ts.lm.2d)

### three day returns overlapping
Google_Return.3d = c(rep(NA, 240), diff(log(DF[,3]), lag = 240))[80:1659]
Google.ts.lm.3d = lm(Google_Return.3d[1:1421]~Google_Return.3d[160:1580])
Google.ts.lm.3d$coefficients
summary(Google.ts.lm.3d)


### four day returns overlapping
Google_Return.4d = c(rep(NA, 320), diff(log(DF[,3]), lag = 320))[80:1659]
Google.ts.lm.4d = lm(Google_Return.4d[1:1261]~Google_Return.4d[320:1580])
Google.ts.lm.4d$coefficients
summary(Google.ts.lm.4d)
plot(Google_Return.4d)


#### Goldman Sachs
### 5-minute return

GS.ts.lm.5min = lm(GS.ts[,3]~lag(GS.ts[,3]))
GS.ts.lm.5min = lm(GS.ts[1:1579,3]~GS.ts[2:1580,3])
GS.ts.lm.5min$coefficients
summary(GS.ts.lm.5min)

head(lag(GS.ts[,3]))

### 30-minute returns overlapping
GS_Return.30 = c(rep(NA, 6), diff(log(DF[,5]), lag = 6))[80:1659]
GS.ts.lm.30min = lm(GS_Return.30[1:1579]~GS_Return.30[2:1580])
GS.ts.lm.30min$coefficients
summary(GS.ts.lm.30min)

### 60-minute returns overlapping
GS_Return.60 = c(rep(NA, 12), diff(log(DF[,5]), lag = 12))[80:1659]
GS.ts.lm.60min = lm(GS_Return.60[1:1569]~GS_Return.60[12:1580])
GS.ts.lm.60min$coefficients
summary(GS.ts.lm.60min)

### 120-minute returns overlapping
GS_Return.120 = c(rep(NA, 24), diff(log(DF[,5]), lag = 24))[80:1659]
GS.ts.lm.120min = lm(GS_Return.120[1:1557]~GS_Return.120[24:1580]) ####
GS.ts.lm.120min$coefficients
summary(GS.ts.lm.120min)

### one day returns overlapping
GS_Return.1d = c(rep(NA, 80), diff(log(DF[,5]), lag = 80))[80:1659]
GS.ts.lm.1d = lm(GS_Return.1d[1:1501]~GS_Return.1d[80:1580])
GS.ts.lm.1d$coefficients
summary(GS.ts.lm.1d)

### two day returns overlapping
GS_Return.2d = c(rep(NA, 160), diff(log(DF[,5]), lag = 160))[80:1659]
GS.ts.lm.2d = lm(GS_Return.2d[1:1421]~GS_Return.2d[160:1580])
GS.ts.lm.2d$coefficients
summary(GS.ts.lm.2d)

### three day returns overlapping
GS_Return.3d = c(rep(NA, 240), diff(log(DF[,5]), lag = 240))[80:1659]
GS.ts.lm.3d = lm(GS_Return.3d[1:1341]~GS_Return.3d[240:1580])
GS.ts.lm.3d$coefficients
summary(GS.ts.lm.3d)

### four day returns overlapping
GS_Return.4d = c(rep(NA, 320), diff(log(DF[,5]), lag = 320))[80:1659]
GS.ts.lm.4d = lm(GS_Return.4d[1:1261]~GS_Return.4d[320:1580])
GS.ts.lm.4d$coefficients
summary(GS.ts.lm.4d)



#### Procter and Gamble
### 5-minute return
PuG.ts.lm.5min = lm(PuG.ts[,3]~lag(PuG.ts[,3]))
PuG.ts.lm.5min = lm(PuG.ts[1:1579,3]~PuG.ts[2:1580,3])
PuG.ts.lm.5min$coefficients
summary(PuG.ts.lm.5min)

head(lag(PuG.ts[,3]))

### 30-minute returns overlapping
PuG_Return.30 = c(rep(NA, 6), diff(log(DF[,7]), lag = 6))[80:1659]
PuG.ts.lm.30min = lm(PuG_Return.30[1:1575]~PuG_Return.30[6:1580])
PuG.ts.lm.30min$coefficients
summary(PuG.ts.lm.30min)

### 60-minute returns overlapping
PuG_Return.60 = c(rep(NA, 12), diff(log(DF[,7]), lag = 12))[80:1659]
PuG.ts.lm.60min = lm(PuG_Return.60[1:1569]~PuG_Return.60[12:1580])
PuG.ts.lm.60min$coefficients
summary(PuG.ts.lm.60min)

### 120-minute returns overlapping
PuG_Return.120 = c(rep(NA, 24), diff(log(DF[,7]), lag = 24))[80:1659]
PuG.ts.lm.120min = lm(PuG_Return.120[1:1557]~PuG_Return.120[24:1580]) ####
PuG.ts.lm.120min$coefficients
summary(PuG.ts.lm.120min)

### one day returns overlapping
PuG_Return.1d = c(rep(NA, 80), diff(log(DF[,7]), lag = 80))[80:1659]
PuG.ts.lm.1d = lm(PuG_Return.1d[1:1501]~PuG_Return.1d[80:1580])
PuG.ts.lm.1d$coefficients
summary(PuG.ts.lm.1d)

### two day returns overlapping
PuG_Return.2d = c(rep(NA, 160), diff(log(DF[,7]), lag = 160))[80:1659]
PuG.ts.lm.2d = lm(PuG_Return.2d[1:1421]~PuG_Return.2d[160:1580])
PuG.ts.lm.2d$coefficients
summary(PuG.ts.lm.2d)

### three day returns overlapping
PuG_Return.3d = c(rep(NA, 240), diff(log(DF[,7]), lag = 240))[80:1659]
PuG.ts.lm.3d = lm(PuG_Return.3d[1:1341]~PuG_Return.3d[240:1580])
PuG.ts.lm.3d$coefficients
summary(PuG.ts.lm.3d)

### four day returns overlapping
PuG_Return.4d = c(rep(NA, 320), diff(log(DF[,7]), lag = 320))[80:1659]
PuG.ts.lm.4d = lm(PuG_Return.4d[1:1261]~PuG_Return.4d[320:1580])
PuG.ts.lm.4d$coefficients
summary(PuG.ts.lm.4d)


#### Netflix
### 5-minute return

NFLX.ts.lm.5min = lm(NFLX.ts[,3]~lag(NFLX.ts[,3]))
NFLX.ts.lm.5min = lm(NFLX.ts[1:1579,3]~NFLX.ts[2:1580,3])
NFLX.ts.lm.5min$coefficients
summary(NFLX.ts.lm.5min)

head(lag(NFLX.ts[,3]))

### 30-minute returns overlapping
NFLX_Return.30 = c(rep(NA, 6), diff(log(DF[,9]), lag = 6))[80:1659]
NFLX.ts.lm.30min = lm(NFLX_Return.30[1:1575]~NFLX_Return.30[6:1580])
NFLX.ts.lm.30min$coefficients
summary(NFLX.ts.lm.30min)

### 60-minute returns overlapping
NFLX_Return.60 = c(rep(NA, 12), diff(log(DF[,9]), lag = 12))[80:1659]
NFLX.ts.lm.60min = lm(NFLX_Return.60[1:1569]~NFLX_Return.60[12:1580])
NFLX.ts.lm.60min$coefficients
summary(NFLX.ts.lm.60min)

### 120-minute returns overlapping
NFLX_Return.120 = c(rep(NA, 24), diff(log(DF[,9]), lag = 24))[80:1659]
NFLX.ts.lm.120min = lm(NFLX_Return.120[1:1557]~NFLX_Return.120[24:1580]) ####
NFLX.ts.lm.120min$coefficients
summary(NFLX.ts.lm.120min)

### one day returns overlapping
NFLX_Return.1d = c(rep(NA, 80), diff(log(DF[,9]), lag = 80))[80:1659]
NFLX.ts.lm.1d = lm(NFLX_Return.1d[1:1501]~NFLX_Return.1d[80:1580])
NFLX.ts.lm.1d$coefficients
summary(NFLX.ts.lm.1d)

### two day returns overlapping
NFLX_Return.2d = c(rep(NA, 160), diff(log(DF[,9]), lag = 160))[80:1659]
NFLX.ts.lm.2d = lm(NFLX_Return.2d[1:1421]~NFLX_Return.2d[160:1580])
NFLX.ts.lm.2d$coefficients
summary(NFLX.ts.lm.2d)

### three day returns overlapping
NFLX_Return.3d = c(rep(NA, 240), diff(log(DF[,9]), lag = 240))[80:1659]
NFLX.ts.lm.3d = lm(NFLX_Return.3d[1:1341]~NFLX_Return.3d[240:1580])
NFLX.ts.lm.3d$coefficients
summary(NFLX.ts.lm.3d)

### four day returns overlapping
NFLX_Return.4d = c(rep(NA, 320), diff(log(DF[,9]), lag = 320))[80:1659]
NFLX.ts.lm.4d = lm(NFLX_Return.4d[1:1261]~NFLX_Return.4d[320:1580])
NFLX.ts.lm.4d$coefficients
summary(NFLX.ts.lm.4d)
