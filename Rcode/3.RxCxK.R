## ---------------------------
rxcxk_tablo <- xtabs(~ veri$cinsiyet + veri$fakulte + veri$performans_nitel)
names(dimnames(rxcxk_tablo)) <- c("Cinsiyet", "Fakülte","Performans") 
ftable(rxcxk_tablo)


## ---------------------------
ftable(addmargins(rxcxk_tablo))


## ---------------------------
margin.table(rxcxk_tablo,1)


## ---------------------------
margin.table(rxcxk_tablo,2)


## ---------------------------
margin.table(rxcxk_tablo,3)


## ---------------------------
margin.table(rxcxk_tablo,c(2,3))


## ---------------------------
margin.table(rxcxk_tablo,c(1,2))


## ---------------------------
margin.table(rxcxk_tablo,c(1,3))


## ---------------------------
ftable(prop.table(rxcxk_tablo))


## ---------------------------
round(ftable(prop.table(rxcxk_tablo,1)),2)


## ---------------------------
round(ftable(prop.table(rxcxk_tablo,2)),2)


## ---------------------------
round(ftable(prop.table(rxcxk_tablo,3)),2)


## ---------------------------
round(ftable(prop.table(rxcxk_tablo,c(1,2))),2)


## ---------------------------
round(ftable(prop.table(rxcxk_tablo,c(1,3))),2)


## ---------------------------
round(ftable(prop.table(rxcxk_tablo,c(2,3))),2)


## ---------------------------
veri_freq <- as.data.frame(rxcxk_tablo) 
names(veri_freq) <- c("cinsiyet","fakulte","performans","freq")


## ---------------------------
model0 <- glm(freq~ cinsiyet + fakulte + performans, family = poisson,data = veri_freq)


## ---------------------------
model1 <- glm(freq ~ cinsiyet + fakulte + performans + cinsiyet*performans, family = poisson,data = veri_freq) 
model2 <- glm(freq ~ cinsiyet + fakulte + performans + cinsiyet*fakulte, family = poisson,data = veri_freq) 
model3 <- glm(freq ~ cinsiyet + fakulte + performans + fakulte*performans, family = poisson,data = veri_freq)


## ---------------------------
model4 <- glm(freq ~ cinsiyet + fakulte + performans + cinsiyet*fakulte + cinsiyet*performans, family = poisson,data = veri_freq) 
model5 <- glm(freq ~ cinsiyet + fakulte + performans + cinsiyet*performans + fakulte*performans, family = poisson,data = veri_freq) 
model6 <- glm(freq ~ cinsiyet + fakulte + performans + cinsiyet*fakulte + fakulte*performans, family = poisson,data = veri_freq)


## ---------------------------
model7 <- glm(freq ~ cinsiyet + fakulte + performans + cinsiyet*fakulte + cinsiyet*performans + fakulte*performans, family = poisson,data = veri_freq)


## ---------------------------
LRstats(model0, model1, model2, model3, model4, model5, model6, model7)


## ---------------------------
AIC_2 <- model2$deviance-2*model2$df.residual 
AIC_2


## ---------------------------
AIC_4 <- model4$deviance-2*model4$df.residual 
AIC_4


## ---------------------------
AIC_6 <- model6$deviance-2*model6$df.residual 
AIC_6


## ---------------------------
AIC_7 <- model7$deviance-2*model7$df.residual 
AIC_7



