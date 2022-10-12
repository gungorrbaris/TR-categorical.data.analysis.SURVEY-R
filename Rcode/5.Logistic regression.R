

## ---------------------------
veri_loj <- veri[, c(1,2,4,5,6,32,35)]


## ---------------------------
veri_loj$cinsiyet <- as.factor(veri_loj$cinsiyet)

veri_loj$fakulte <- as.factor(veri_loj$fakulte)

veri_loj$performans_nitel <- as.factor(veri_loj$performans_nitel)

veri_loj$ders <- as.factor(veri_loj$ders)

veri_loj$egitim <- as.factor(veri_loj$egitim)

veri_loj$platform <- as.factor(veri_loj$platform)

veri_loj$arastirma <- factor(veri_loj$arastirma, labels = c("Araştırmam","Nadiren","Bazen","Araştırırım","Her zaman"))


## ---------------------------
knitr::kable(head(veri_loj,n=10), align = "c")


## ---------------------------
veri_loj$platform <- relevel(as.factor(veri_loj$platform), ref = "Instagram")

veri_loj$performans_nitel <- relevel(as.factor(veri_loj$performans_nitel), ref = "orta")


## ---------------------------
model_lojistik <- polr(data= veri_loj, performans_nitel~cinsiyet+fakulte+ders+egitim+platform+arastirma, Hess=TRUE)

summary(model_lojistik)


## ---------------------------
Anova(model_lojistik, type="II", test="Wald")


## ---------------------------
back_lr <- step(model_lojistik)


## ---------------------------
uygun_model <- polr(data= veri_loj, performans_nitel ~ platform, Hess=TRUE)

summary(uygun_model)


## ---------------------------
Anova(uygun_model, type="II", test="Wald")


## ---------------------------
logitgof(performans_nitel, fitted(uygun_model), ord = TRUE)


## ---------------------------
exp(cbind(OR = coef(uygun_model), confint(uygun_model,level = 0.95)))


## ---------------------------
probabilities <- predict(uygun_model, veri_loj)

head(probabilities,n=200)


## ---------------------------
table(Orijinal = veri_loj$performans_nitel,Tahmin = probabilities)


## ---------------------------
mean(probabilities == veri_loj$performans_nitel)

