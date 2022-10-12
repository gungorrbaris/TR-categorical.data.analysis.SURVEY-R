knitr::kable(head(summary(veri[, c(1,2,4,5,6,7,32,33,35)])), align = "c")


## ---------------------------
etiket1 <- c("Erkek","Kadın") 

h1<- table(veri$cinsiyet)
yuzdelik1<- paste("%",round(100*h1/sum(h1),1))

par(mar=c(0,12,0,0), xpd=TRUE)
pie3D(h1,labels = yuzdelik1, start=1, radius = 0.9, height = 0.1, theta = 1, explode = 0.09, shade=0.7, mar=c(5,5,5,5), main = "Cinsiyet Dağılımı Pasta Grafiği", col = c("steelblue1","indianred3"), border = "black", labelcol="black",labelcex=1.5)
legend("bottomright", etiket1, inset=c(-0.3,0.02), cex=1, text.font = 3, title = "Cinsiyet", y.intersp = 1.2, fill = c("steelblue1","indianred3"))


## ---------------------------
etiket2 <- c("Eğitim","Fen","İletişim","Mühendislik")
h2<- table(veri$fakulte)
yuzdelik2<- paste("%",round(100*h2/sum(h2),1))

par(mar=c(0,12,0,0), xpd=TRUE)
pie3D(h2,labels = yuzdelik2, start=1, radius = 0.9, height = 0.1, theta = 1, explode = 0.09, shade=0.7, main = "Fakülte Dağılımı Pasta Grafiği", col = hcl.colors(length(h2), "Spectral"), border = "black", labelcol="black",labelcex=1.5)
legend("bottomright", etiket2, inset=c(-0.3,0.02), cex=1, text.font = 3, title = "Fakülteler", y.intersp = 1.2, fill = hcl.colors(length(h2), "Spectral"))


## ---------------------------
etiket3 <- c("Evet aldım","Hayır almadım")
h3<- table(veri$ders)
yuzdelik3<- paste("%",round(100*h3/sum(h3),1))

par(mar=c(0,12,0,0), xpd=TRUE)
pie3D(h3,labels = yuzdelik3, start=3.1, radius = 0.9, height = 0.1, theta = 1, explode = 0.09, shade=0.7, main = "Medya Okuryazarlığı Dersini Alanların Pasta Grafiği", col = c("lightseagreen","lightsteelblue3"), border = "black", labelcol="black",labelcex=1.5)
legend("bottomright", etiket3, inset=c(-0.3,0.02), cex=1, text.font = 3, title = "Medya Okuryazarlığı Dersini", fill = c("lightseagreen","lightsteelblue3"))


## ---------------------------
etiket4 <- c("Evet katıldım","Hayır katılmadım")
h4<- table(veri$egitim)
yuzdelik4<- paste("%",round(100*h4/sum(h4),1))

par(mar=c(0,12,0,0), xpd=TRUE)
pie3D(h4,labels = yuzdelik4, start=3.1, radius = 0.9, height = 0.1, theta = 1, explode = 0.2, shade=0.7, main = "Medya Okuryazarlığı Eğitimini/Seminerini Alanların Pasta Grafiği", col = c("lightseagreen","lightsteelblue3"), border = "black", labelcol="black",labelcex=1.5)
legend("bottomright", etiket4, inset=c(-0.37,0.02), cex=1, text.font = 3, title = "Medya O. Eğitimine/Seminerine", fill = c("lightseagreen","lightsteelblue3"))


## ---------------------------
etiket5 <- c("Diğer","Haber Uygulamaları","Haber Web Siteleri","Instagram","TV","Twitter")
h5<- table(veri$platform)
yuzdelik5<- paste("%",round(100*h5/sum(h5),1))

par(mar=c(0,12,0,0), xpd=TRUE)
pie3D(h5,labels = yuzdelik5, start=1.5, radius = 0.9, height = 0.1, theta = 1, explode = 0.08, shade=0.7, main = "Öğrencilerin Takip Ettikleri Platformların Pasta Grafiği", col = c("#9C0824","#ED620F","#F6796A","#BBC5CC","#4F98C4","#1C5A99"), border = "black", labelcol="black",labelcex=1.5)
legend("bottomright", etiket5, inset=c(-0.35,-0.07), cex=1, y.intersp = 1.16, text.font = 3, title = "Haber Platformları", fill = c("#9C0824","#ED620F","#F6796A","#BBC5CC","#4F98C4","#1C5A99"))


## ---------------------------
etiket6 <- c("Hiçbir Zaman (1)","Nadiren (2)","Bazen (3)","Genellikle (4)","Her zaman (5)")
h6<- table(veri$arastirma)
yuzdelik6<- paste("%",round(100*h6/sum(h6),1))

par(mar=c(0,12,0,0), xpd=TRUE)
pie3D(h6,labels = yuzdelik6, start=0.9, radius = 0.9, height = 0.1, theta = 1, explode = 0.08, shade=0.7, main = "Öğrencilerin Haberleri Araştırma Dereceleri Pasta Grafiği", col = hcl.colors(length(h6), "pubugn"), border = "black", labelcol="black",labelcex=1.5)
legend("bottomright", etiket6, inset=c(-0.35,0.004), cex=1, y.intersp = 1.16, text.font = 3, title = "Doğruluğunu araştırırım", fill = hcl.colors(length(h6), "pubugn"))


## ---------------------------
etiket7 <- c("Evet isterim","Hayır istemem")
h7<- table(veri$ders_acilsin_mi)
yuzdelik7<- paste("%",round(100*h7/sum(h7),1))
par(mar=c(0,12,0,0), xpd=TRUE)

pie3D(h7,labels = yuzdelik7, start=1.4, radius = 0.9, height = 0.1, theta = 1, explode = 0.08, shade=0.7, main = "Ders Talebi Pasta Grafiği", col = c("lightseagreen","lightsteelblue3"), border = "black", labelcol="black",labelcex=1.5)
legend("bottomright", etiket7, inset=c(-0.3,0.08), cex=1, text.font = 3, title = "Dersin Açılmasını", fill = c("lightseagreen","lightsteelblue3"))


## ---------------------------
etiket8 <- c("İyi","Kötü","Orta")
h8<- table(veri$performans_nitel)
yuzdelik8<- paste("%",round(100*h8/sum(h8),1))

par(mar=c(0,12,0,0), xpd=TRUE)
pie3D(h8,labels = yuzdelik8, start=1, radius = 0.9, height = 0.1, theta = 1, explode = 0.09, shade=0.7, main = "Performans Dağılımı Pasta Grafiği", col = c("#56B4E9", "brown","#E69F00"), border = "black", labelcol="black",labelcex=1.5)
legend("bottomright", etiket8, inset=c(-0.3,0.02), cex=1, text.font = 3, title = "Fakülteler", y.intersp = 1.2, fill = c("#56B4E9", "brown","#E69F00"))


## ---------------------------
ggplot(veri, aes(performans)) + geom_bar(fill="steelblue")+ scale_y_continuous(limits=c(0,30),breaks = c(0,5,10,15,20,25,30))+ scale_x_continuous(limits=c(7,24),breaks = c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))+ theme_minimal()+ ggtitle("Araştırmadaki Bütün Öğrencilerin Performans Dağılımı")+ labs(x="Performans", y = "Sıklık")+ theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(fakulte, fill = cinsiyet)) + labs(x="Fakülteler", y = "Sıklık")+ geom_bar(position=position_dodge(),colour="black")+ scale_y_continuous(limits=c(0,36),breaks = c(0,3,6,9,12,15,18,21,24,27,30,33,36))+ guides(fill=guide_legend(title="Cinsiyet"))+ scale_fill_manual(values=c("#56B4E9","#E69F00"))+ ggtitle("Fakültelere Göre Cinsiyetin Dağılımı")+ theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(performans, fill = cinsiyet)) + labs(x="Yirmi Dört Soru İçerisinde Bilinen Doğru Sayısı", y = "Sıklık")+ geom_bar(position=position_dodge(),width=0.8,colour="black")+ scale_x_continuous(limits=c(7,23),breaks = c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))+ scale_y_continuous(limits=c(0,20),breaks = c(0,2,4,6,8,10,12,14,16,18,20,22))+ guides(fill=guide_legend(title="Cinsiyet"))+ scale_fill_manual(values=c("#56B4E9","#E69F00"))+ ggtitle("Performanslara Göre Cinsiyetin Dağılımı")+ theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(x= performans_nitel, group=cinsiyet)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + labs(x="Performans",y = "Yüzdelik %", fill="Performans") + facet_grid(~cinsiyet)+ scale_fill_manual(values=c("#56B4E9", "brown","#E69F00"),labels=c("İyi", "Kötü", "Orta"))+ scale_y_continuous(labels = scales::percent)+ ggtitle("Cinsiyetlere Göre Performans Oranları")+ theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(performans_nitel, fill = cinsiyet)) + geom_bar(position = "fill")+ labs(x="Performans", y = "Yüzdelik %")+ scale_y_continuous(limits=c(0,1),breaks = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+ guides(fill=guide_legend(title="Cinsiyet"))+ scale_fill_manual(values=c("#56B4E9","#E69F00"))+ ggtitle("Performans Seviyelerinde Cinsiyetlerin Oranı")+ theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(x= performans_nitel, group=fakulte)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + labs(x="Performans",y = "Yüzdelik %", fill="Performans") + facet_grid(~fakulte)+ scale_fill_manual(values=c("#56B4E9", "brown","#E69F00"),labels=c("İyi", "Kötü", "Orta"))+ scale_y_continuous(labels = scales::percent)+ ggtitle("Fakültelere Göre Performans Oranları")+ theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(performans_nitel, fill = fakulte)) + geom_bar(position = "fill")+ labs(x="Performans", y = "Yüzdelik %")+ guides(fill=guide_legend(title="Fakülte"))+ scale_color_npg()+ scale_fill_npg()+ ggtitle("Performans Seviyelerinde Fakültelerin Oranı")+ theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(fakulte, fill = platform)) + geom_bar(position=position_dodge(),colour="black")+ labs(x="Fakülte", y = "Sıklık")+ guides(fill=guide_legend(title="Haber Platformları"))+ scale_fill_manual(values=hcl.colors(length(table(veri$platform)), "rdbu"))+ scale_y_continuous(limits=c(0,30),breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))+ ggtitle("Fakültelere Göre Haber Kaynağı Dağılımları")+ theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(fakulte, fill = arastirma)) + geom_bar(position=position_dodge(),colour="black")+ labs(x="Araştırma Derecesi", y = "Sıklık")+ guides(fill=guide_legend(title="Fakülteler"))+ scale_fill_manual(values=hcl.colors(length(table(veri$arastirma)), "Reds"))+ scale_y_continuous(limits=c(0,26),breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26))+ ggtitle("Fakültere Göre Haber Kaynağı Araştırma Düzeyleri")+ theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(fakulte, fill = arastirma)) + geom_bar(position = "fill",colour="black")+ labs(x="Fakülteler", y = "Yüzdelik %")+ guides(fill=guide_legend(title="Fakülteler"))+ scale_fill_manual(values=hcl.colors(length(table(veri$arastirma)), "Reds"))+ ggtitle("Fakültere Göre Haber Kaynağı Araştırma Düzeyi Oranları")+ theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(performans_nitel, fill = platform)) + geom_bar(position = "fill", colour="black")+ labs(x="Performans Düzeyleri", y = "Yüzdelik %")+ guides(fill=guide_legend(title="Haber Kaynakları"))+ scale_fill_manual(values=hcl.colors(length(table(veri$platform)), "Rdbu"))+ ggtitle("Haber Kaynaklarına Göre Performans Oranları")+ theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri, aes(performans, fill = fakulte)) + labs(x="Yirmi Dört Soru İçerisinde Bilinen Doğru Sayısı", y = "Sıklık")+ geom_bar(width=0.8,colour="black")+ scale_x_continuous(limits=c(7,23),breaks = c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))+ scale_y_continuous(limits=c(0,33),breaks = c(3,6,9,12,15,18,21,24,27,30,33))+ guides(fill=guide_legend(title="Fakülte"))+ scale_fill_manual(values=pal_jco()(10))+ ggtitle("Fakültelere Göre Doğruluk Performansının Dağılımları")+ theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


## ----fig.height=8, fig.width=10----
ggplot(veri, aes(x= performans_nitel, group=platform)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + labs(x="Performans",y = "Yüzdelik %", fill="Performans") + facet_grid(~platform)+ scale_fill_manual(values=c("#56B4E9", "brown","#E69F00"),labels=c("İyi", "Kötü", "Orta"))+ scale_y_continuous(labels = scales::percent)+ ggtitle("Haber Kaynaklarına Göre Performans Oranları")+ theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(data=veri, aes(x=fakulte, y=performans,fill=cinsiyet)) + geom_bar(stat="identity",position=position_dodge(),width = 0.8)+ guides(fill=guide_legend(title="Cinsiyet"))+ labs(x="Fakülte", y = "Performans")+ ggtitle("Fakültelerdeki Kadın ve Erkek Öğrencilerin Performansları")+ scale_fill_manual(values=c("#56B4E9","#E69F00"))+ scale_y_continuous(limits=c(0,24),breaks = c(0,3,6,9,12,15,18,21,24))+ theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


## ---------------------------
ggplot(veri,fill=veri$fakulte)+ aes(x = fakulte , y = performans )+ geom_boxplot(fill="#E69F00")+ theme_minimal()+labs(y="Performans", x = "Fakülteler")+ ggtitle("Fakültelere Göre Performansın Kutu Grafikleri")+ theme(plot.title = element_text(hjust = 0.5))+stat_summary(fun.y=mean,geom="point", shape=18, size=3, color="black")


## ---------------------------
ggplot(veri, aes(x=fakulte, y=performans, fill=cinsiyet)) + geom_boxplot()+ theme_minimal()+ scale_fill_manual(values=c("#56B4E9","#E69F00"))+ labs(y="Performans", x = "Fakülteler")+ ggtitle("Fakülte ve Cinsiyete Göre Performansın Kutu Grafikleri")+ theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(ncol=1,reverse = TRUE,title="Cinsiyet"))+stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black",position = position_dodge2(width = 0.75,preserve = "single"))


## ---------------------------
ggplot(veri, aes(x=fakulte, y=performans, fill=platform)) + geom_boxplot()+ theme_minimal()+ scale_fill_brewer(palette="Dark2")+ ggtitle("Fakülte ve Haber Kaynağına Göre Performansın Kutu Grafikleri")+ theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(ncol=1,reverse = TRUE,title="Platform"))+ labs(y="Performans", x = "Fakülteler")+stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black",position = position_dodge2(width = 0.75,preserve = "single"))


## ---------------------------
box1 <- ggplot(veri, aes(x=fakulte, y=performans, fill=ders)) + geom_boxplot()+ facet_wrap(~fakulte, scale="free")+ theme_minimal()+ scale_fill_brewer(palette="Dark2")+ guides(fill=guide_legend(ncol=1,title="Dersi\naldınız mı?"))+ labs(y="Performans", x = "Fakülteler")+ggtitle("Dersi Alıp Almama Durumu ve Fakültelere Göre Performansın Kutu Grafikleri")+ scale_x_discrete(labels = c("",""))+stat_summary(fun.y=mean, geom="point", shape=18, size=3.2, color="black",position = position_dodge2(width = 0.75,preserve = "single"))

box2 <- ggplot(veri, aes(x=fakulte, y=performans, fill=egitim)) + geom_boxplot()+ facet_wrap(~fakulte, scale="free")+ scale_x_discrete(labels = c("",""))+ theme_minimal()+ scale_fill_brewer(palette="Dark2")+ guides(fill=guide_legend(ncol=1,title="Eğitimi\naldınız mı?"))+ggtitle("Eğitimi Alıp Almama Durumu ve Fakültelere Göre Performansın Kutu Grafikleri")+labs(y="Performans", x = "Fakülteler")+stat_summary(fun.y=mean, geom="point", shape=18, size=3.2, color="black",position = position_dodge2(width = 0.75,preserve = "single"))
ggarrange(box1, box2, ncol = 1)


## ---------------------------
ggplot(veri, aes(x=platform, y=performans, fill=cinsiyet)) + geom_boxplot()+ facet_wrap(~platform, scale="free")+ scale_x_discrete(labels = c("",""))+ theme_minimal()+ scale_fill_manual(values=c("#56B4E9","#E69F00"))+guides(fill=guide_legend(ncol=1,reverse = TRUE,title="Cinsiyet"))+ labs(y="Performans", x = "Haber Kaynakları")+ggtitle("Cinsiyet ve Haber Kaynağına Göre Performansın Kutu Grafikleri")+theme(plot.title = element_text(hjust = 0.5))+stat_summary(fun.y=mean, geom="point", shape=18, size=3.2, color="black",position = position_dodge2(width = 0.75,preserve = "single"))


## ---------------------------
ggplot(veri,fill=arastirma) + aes(x = arastirma , y = performans) + geom_boxplot(fill="#5F9EA0")+ facet_wrap(~arastirma, scale="free")+ scale_x_discrete(labels = c("",""))+theme_minimal()+ggtitle("Araştırma Düzeyine Göre Performansın Kutu Grafikleri")+ theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(ncol=1,reverse = TRUE,title="Platform"))+ labs(y="Performans", x = "Araştırma Düzeyleri")+stat_summary(fun.y=mean, geom="point", shape=18, size=3.2, color="black",position = position_dodge2(width = 0.75,preserve = "single"))


## ---------------------------
ggplot(veri, aes(x=arastirma, y=performans, fill=ders)) + geom_boxplot()+ facet_wrap(~arastirma, scale="free")+ scale_x_discrete(labels = c("",""))+ theme_minimal()+scale_fill_manual(values=c("lightseagreen","lightsteelblue3"))+guides(fill=guide_legend(ncol=1,title="Dersi Aldım"))+ labs(y="Performans", x = "Araştırma Düzeyleri")+ggtitle("Dersi Alıp Almama Durumu ve Araştırma Düzeylerine Göre Performansın Kutu Grafikleri")+stat_summary(fun.y=mean, geom="point", shape=18, size=3.2, color="black",position = position_dodge2(width = 0.75,preserve = "single"))


## ---------------------------
ggplot(veri,fill=veri$fakulte) + aes(x = fakulte , y = performans, fill=fakulte) + geom_violin() + geom_violin(trim=FALSE)+ geom_boxplot(width=0.1, fill="lightblue")+ scale_fill_brewer(palette = "Dark2")+ labs(x="Fakülteler", y = "Performans")+ ggtitle("Fakültelere Göre Performansın Keman Grafiği")+ theme_minimal()+ theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(title="Fakülteler"))+stat_summary(fun="mean")


## ---------------------------
ggplot(veri, aes(x = performans, y =fakulte, fill = cinsiyet)) + geom_density_ridges_gradient(scale = 1.2, rel_min_height = 0.0005)+ labs(x="Performans", y = "Fakülteler")+ ggtitle("Fakülte ve Cinsiyete Göre Performansın Ridgeline Grafiği")+ theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values=c("#56B4E9","#E69F00"))+ scale_x_continuous(limits=c(5,25),breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))+ guides(fill=guide_legend(title="Cinsiyet"))


## ---------------------------
arastirma_sum <- c(6,20,71,84,19)
a <- data.frame(veri$arastirma,veri$performans)
colnames(a) <- c("arastirma_duzeyi","performans")
perf_arastir <- a %>% group_by(arastirma_duzeyi) %>% summarise_each(funs(sum))
perf_arastir$kisi_sayisi_arastirma <- arastirma_sum
oran1 <- perf_arastir[,2]/perf_arastir[,3]
colnames(oran1) <- "oran"
perf_arastir <- cbind(perf_arastir, oran1)


## ---------------------------
perf_arastir


## ---------------------------
platform_sum <- c(15,19,29,44,7,86)
b <- data.frame(veri$platform,veri$performans)
colnames(b) <- c("haber_kaynaklari","performans")
perf_platform <- b %>% group_by(haber_kaynaklari) %>% summarise_each(funs(sum))
perf_platform$kisi_sayisi_platform <- platform_sum
oran2 <- perf_platform[,2]/perf_platform[,3]
colnames(oran2) <- "oran"
perf_platform <- cbind(perf_platform, oran2)


## ---------------------------
perf_platform


## ---------------------------
parlamento1 <- data.frame(Araştırma_Düzeyi = factor(c("Hiçbir Zaman","Nadiren","Genellikle","Bazen","Her zaman"), levels = c("Hiçbir Zaman","Nadiren","Genellikle","Bazen","Her zaman")), seats = c(150,143,153,158,157), colors=c("red3","#34854C","dodgerblue1","blue4","orange2"),stringsAsFactors=FALSE)

ggplot(parlamento1) + ggtitle("Araştırma Düzeylerine Göre Performansların Parlamento Grafiği") + geom_parliament(aes(seats = seats, fill = Araştırma_Düzeyi), color = NA) + scale_fill_manual(values = parlamento1$colors, labels = parlamento1$Araştırma_Düzeyi) + coord_fixed() + theme_void()+ theme(plot.title = element_text(hjust = 0.5),legend.position="bottom",legend.direction = "vertical")+ guides(fill=guide_legend(nrow=2,byrow=TRUE,reverse = TRUE,title="Araştırma Düzeyleri"))


## ---------------------------
parlamento2 <- data.frame(Araştırma_Düzeyi=factor(c("TV","Haber Web Siteleri","Instagram","Twitter","Haber Uygulamaları","Diğer"),levels=c("TV","Haber Web Siteleri","Instagram","Twitter","Haber Uygulamaları","Diğer")),seats=c(143,152,152,153,161,164),colors =c("#0071A6","#EF9708","#815122","#DFCEBA","#1EB2B5","#142D4A"),stringsAsFactors=FALSE)

ggplot(parlamento2) + ggtitle("Haber Kaynaklarına Göre Performansların Parlamento Grafiği") +geom_parliament(aes(seats = seats, fill = Araştırma_Düzeyi), color = NA) + scale_fill_manual(values = parlamento2$colors, labels = parlamento2$Araştırma_Düzeyi) +coord_fixed() +theme_void()+ theme(plot.title = element_text(hjust = 0.5),legend.position="bottom",legend.direction = "vertical")+guides(fill=guide_legend(nrow=2,byrow=TRUE,reverse = TRUE,title="Haber Kaynakları"))


## ---------------------------
ggplot(veri, aes(x = arastirma, y = fakulte, fill = performans)) + ggtitle("Fakültelerde Haber Kaynağı Araştırma Düzeylerine Göre Isı Grafiği")+ theme(plot.title = element_text(hjust = 0.5),legend.position="right",legend.direction = "vertical")+ labs(y = "Fakülteler", x = "Araştırma Düzeyleri")+ geom_tile(color = "azure2", lwd = 1.5, linetype = 1) + guides(fill=guide_legend(ncol=1,reverse = TRUE,title="Doğru\nPerformansı"))+ coord_fixed()+ scale_fill_viridis(discrete=FALSE)


## ---------------------------
library(dplyr)
library(MASS)


## ----fig.height=6, fig.width=8.5----
heat_veri <- veri %>% dplyr::select(2,8:31)

colnames(heat_veri) <- c("fakulte","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","h12",
                    "h13","h14","h15","h16","h17","h18","h19","h20","h21","h22","h23","h24")

haber_dogru_toplam <- heat_veri %>% group_by(fakulte) %>% summarise_each(funs(sum))

haber_dogru_toplam


heatmaply(normalize(haber_dogru_toplam), xlab = "Sorular", ylab = "Fakülteler", scale = "column", main = "Fakültelerin Doğru Cevap Verdiği Soru Sayısı İçin Isı Grafiği", Colv = NA, Rowv = NA,row_dend_left = T, plot_method = "plotly")



