

## ---------------------------
rxc_tablo <- table(veri$fakulte, veri$performans_nitel) 
names(dimnames(rxc_tablo)) <- c("Fakülte", "Performans") 
rxc_tablo


## ---------------------------
margin.table(rxc_tablo,1)


## ---------------------------
margin.table(rxc_tablo,2)


## ---------------------------
round(margin.table(rxc_tablo,1)/sum(rxc_tablo),3)


## ---------------------------
round(margin.table(rxc_tablo,2)/sum(rxc_tablo),3)


## ---------------------------
round(prop.table(rxc_tablo),3)


## ---------------------------
round(prop.table(rxc_tablo,1),3)


## ---------------------------
round(prop.table(rxc_tablo,2),3)


## ---------------------------
chisq.test(rxc_tablo)$expected


## ---------------------------
chisq.test(rxc_tablo)


## ---------------------------
chisq.test(rxc_tablo)$stdres


## ---------------------------
KendallTauB(rxc_tablo, conf.level = 0.95)


## ---------------------------
StuartTauC(rxc_tablo, conf.level = 0.95)


## ---------------------------
SomersDelta(rxc_tablo, direction = "column", conf.level = 0.95)


## ---------------------------
cramerV(rxc_tablo,ci=TRUE)


## ---------------------------
rxc_odds <- loddsratio(rxc_tablo, correct = any(rxc_tablo == 0L), log = F) 
rxc_odds


## ---------------------------
confint(rxc_odds,level = 0.95)


## ---------------------------
uyum <- ca(rxc_tablo)
uyum


## ---------------------------
plot.ca(uyum, map = "symmetric", main = "Uyum Grafiği", col = c("#00AFBB", "#FF0000"),mass =c(TRUE, TRUE), pch=c(16,2,17,4), col.lab = c("#00AFBB", "#FF0000"), ylim=c(-0.1, 0.2), xlim=c(-0.6, 0.6)) 
legend("bottomright", c("Fakülte","Performans"), pch=c(16,17), col = c("#00AFBB", "#FF0000"),cex = 1.1)



