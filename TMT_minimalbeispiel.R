suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(require(verification))


###################################################################
# Input ####
###################################################################
# gesamt ... muss noch auf Temperatur reduziert werden
lokal<-"Europe/Berlin"    # Wenn Zeitzone soll betrachtet werden
tag_start<-6              # Tagesmitteltemperatur (TMT) für tag_start bis tag_start+23
fct_var<-"V1005"
###################################################################
# Reduktion des Input-Table "gesamt" auf "goa" ####
# Meldungskennzahl 5 besitzt nut NA's
###################################################################

devisor<-10
gesamt$V1005[!is.na(eval(parse(text=paste0("gesamt$",fct_var))))] <- 
  gesamt$V1005[!is.na(eval(parse(text=paste0("gesamt$",fct_var))))]/devisor

goa<-gesamt[!MELDUNGSKENNZAHL==5]

sum(goa$LUFTTEMPERATUR>-20,na.rm = TRUE)/12
##################################################################
# Bestimme Tagesmitteltemperatur ####
##################################################################

goa$MESS_DATUM<-with_tz(goa$MESS_DATUM,lokal)         # ändere UTC in Lokalzeit für MESS_DATUM
goa$fimos.zeit<-with_tz(goa$fimos.zeit,lokal)         # ändere UTC in Lokalzeit für fimos.zeit
goa$MESS_DATUM<-goa$MESS_DATUM-hours(tag_start)       # verschiebe um tag_start,
#goa$fimos.zeit<-goa$fimos.zeit-hours(tag_start)       # damit wieder von 0 bis 23 gerechnet werden kann
# DURCH VERSCHIEBEN DER FIMOS ZEIT VERSCHIEBT SICH VORHERSAGEZEIT AUF UNTER EINEN TAG!!!


# Erstelle neues Data.Table mit beobachteten mittleren Tagestemperaturen von tag_start lokal bis tag_start+23 lokal
# duplicated wird benötigt, da Beochbachtungsdaten mehrfach, aber nicht gleich häufig auftreten
# aufgrund der mehrfachen Prognoseläufe.
!duplicated(goa[,c(1,2)])

TMT_Observe<-goa[!duplicated(goa[,.(WMO,MESS_DATUM)]),mean(LUFTTEMPERATUR),by=.(WMO,ymd(format(MESS_DATUM,'%Y-%m-%d')))]
colnames(TMT_Observe)[2:3]<-c("MESS_TAG","MEAN_LUFTTEMPERATUR")

# Erstelle neues Data.Table mit prognostizierten mittleren Tagestemperaturen von 0 bis 23, ab 1. Folgetag
TMT_Prognose<-goa[ymd(format(MESS_DATUM,'%Y-%m-%d'))-ymd(format(fimos.zeit,'%Y-%m-%d'))>0,
                  mean(V1005),by=.(WMO,fimos.zeit,ymd(format(MESS_DATUM,'%Y-%m-%d')))][,c(1,3,2,4)]
setnames(TMT_Prognose,"V1","MEAN_V1005")
setnames(TMT_Prognose,"ymd","MESS_TAG")

# Mergen beider Tables
setkey(TMT_Observe,WMO,MESS_TAG)
setkey(TMT_Prognose,WMO,MESS_TAG)
TMT<-merge(TMT_Observe,TMT_Prognose)[,c(1,2,4,3,5)]

# Anhängen der Differenz und welcher Folgetag
TMT$diff<-TMT$MEAN_LUFTTEMPERATUR-TMT$MEAN_V1005
TMT$FT<-TMT$MESS_TAG-ymd(format(TMT$fimos.zeit,'%Y-%m-%d'))  

# Berechne RMSE
RMSE<-TMT[,sqrt(mean(diff^2,na.rm=TRUE)),by=.(FT)]
setnames(RMSE,"V1","ERROR")



# Plot RMSE
windows(height = 6, width = 8)
par(mar = c(3, 4.1, 1, 0.5) + 0.6) # Damit y-Achsenbezeichnung nicht abgeschnitten wird
par(mfrow = c(1,1)) 
par(cex.lab=1.6)
par(cex.axis=1.2)
par(cex=1.6)
par(mgp=c(2.5,0.9,0))

plot(NA,ylim=c(0,4),xlim=range(1,10),type='l',xlab="Folgetag",ylab="RMSE in °C",lwd=3,
     col="royalblue",frame.plot = FALSE,main="Tagesmitteltemperatur")
lines(RMSE$FT,RMSE$ERROR,col="royalblue",lwd=3)


