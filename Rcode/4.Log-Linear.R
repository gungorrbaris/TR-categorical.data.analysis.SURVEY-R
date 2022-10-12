

## ---------------------------
log_tablo <- xtabs(~ veri$cinsiyet + veri$fakulte + veri$performans_nitel)
names(dimnames(log_tablo)) <- c("Cinsiyet", "Fakülte","Performans")

ftable(log_tablo)


## ---------------------------
veri_freq_log <- as.data.frame(log_tablo)

names(veri_freq_log) <- c("cinsiyet","fakulte","performans","freq")

veri_freq_log$cinsiyet <- relevel(as.factor(veri_freq_log$cinsiyet), ref = "Kadın")

veri_freq_log$fakulte <- relevel(as.factor(veri_freq_log$fakulte), ref = "İletişim Fakültesi")

veri_freq_log$performans <- relevel(as.factor(veri_freq_log$performans), ref = "iyi")


## ---------------------------
doygun_model <- glm(freq ~ cinsiyet + fakulte + performans + cinsiyet*fakulte+ cinsiyet*performans + fakulte*performans+ cinsiyet*fakulte*performans, family = poisson, data = veri_freq_log)

model_doygun_log <- step(doygun_model, direction = "backward")


## ---------------------------
LRstats(model_doygun_log)


## ---------------------------
summary(model_doygun_log)



