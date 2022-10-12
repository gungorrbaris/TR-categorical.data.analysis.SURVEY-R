

## ---------------------------
veri <- read.csv("https://raw.githubusercontent.com/gungorrbaris/TR-categorical.data.analysis.SURVEY-R/main/data/veri-anket.csv",header=TRUE,sep=";")


knitr::kable(head(veri,n=10), align = "c")


## ---------------------------
performans_nitel <- ifelse(veri$performans > 17, "iyi", ifelse(veri$performans <13 , "kötü","orta"))
veri$performans_nitel = performans_nitel
veri$performans_nitel <- as.factor(performans_nitel)


## ---------------------------

veri$cinsiyet <- as.factor(veri$cinsiyet)

veri$fakulte <- as.factor(veri$fakulte)

veri$bolum <- as.factor(veri$bolum)

veri$ders <- as.factor(veri$ders)

veri$egitim <- as.factor(veri$egitim)

veri$platform <- as.factor(veri$platform)

veri$arastirma <- factor(veri$arastirma,
                         labels = c("Hiçbir Zaman","Nadiren","Bazen","Genellikle","Her zaman"))

veri$ders_acilsin_mi <- as.factor(veri$ders_acilsin_mi)


## ---------------------------
knitr::kable(head(veri[, c(1,2,4,5,6,7,32,33,35)],n=10), align = "c")


## ---------------------------
str(veri[, c(1,2,4,5,6,7,32,33,35)])


##
knitr::kable(head(summary(veri[, c(1,2,4,5,6,7,32,33,35)])), align = "c")



