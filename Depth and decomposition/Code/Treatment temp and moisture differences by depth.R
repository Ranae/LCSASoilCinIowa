library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

header.cc <- scan("Data/CCRafa.out", skip = 2, nlines = 1, what = character(), sep="")
cc<-read.table("Data/CCRafa.out", skip = 4, header=FALSE, na.strings = "?")
names(cc) <- header.cc
cc$trt<-"cc"
cc<-select(cc, trt, Date, sw_5, sw_10, sw_15, sw_35, sw_75, sw_100, st_5, st_10, st_15, st_35, st_75, st_100)

#cc$Date<-as.Date(cc.biomass$Date, "%d/%m/%Y")

header.cs <- scan("Data/CS.out", skip = 2, nlines = 1, what = character(), sep="")
cs<-read.table("Data/CS.out", skip = 4, header=FALSE, na.strings = "?")
names(cs) <- header.cs
cs$trt<-"cs"
cs<-select(cs, trt, Date, sw_5, sw_10, sw_15, sw_35, sw_75, sw_100, st_5, st_10, st_15, st_35, st_75, st_100)


header.sc <- scan("Data/SC15.out", skip = 2, nlines = 1, what = character(), sep="")
sc<-read.table("Data/SC15.out", skip = 4, header=FALSE, na.strings = "?")
names(sc) <- header.sc
sc$trt<-"sc"
sc<-select(sc, trt, Date, sw_5, sw_10, sw_15, sw_35, sw_75, sw_100, st_5, st_10, st_15, st_35, st_75, st_100)


header.pf <- scan("Data/PF.out", skip = 2, nlines = 1, what = character(), sep="")
pf<-read.table("Data/PF.out", skip = 4, header=FALSE, na.strings = "?")
names(pf) <- header.pf
pf$trt<-"pf"
pf<-select(pf, trt, Date, sw_5, sw_10, sw_15, sw_35, sw_75, sw_100, st_5, st_10, st_15, st_35, st_75, st_100)


header.p <- scan("Data/P.out", skip = 2, nlines = 1, what = character(), sep="")
p<-read.table("Data/P.out", skip = 4, header=FALSE, na.strings = "?")
names(p) <- header.p
p$trt<-"p"
p<-select(p, trt, Date, sw_5, sw_10, sw_15, sw_35, sw_75, sw_100, st_5, st_10, st_15, st_35, st_75, st_100)

tempandwater<-rbind(cc, pf, p)

temp<-tempandwater%>%
  select(trt, Date, starts_with("st"))%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y"))%>%
  mutate(year= year(Date))%>%
  group_by(trt)%>%
  gather(key = depth, value = value, st_5:st_100)

ggplot(filter(temp, year == "2012" & depth == "st_5"), aes(x = Date, y=value))+
  geom_line(aes(group=trt, color=trt))+
  facet_wrap(~depth)

water<-tempandwater%>%
  select(trt, Date, starts_with("sw"))%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y"))%>%
  mutate(year= year(Date))%>%
  group_by(trt)%>%
  gather(key = depth, value = value, sw_5:sw_100)

ggplot(filter(water, year == "2013" & depth == "sw_15"), aes(x = Date, y=value))+
  geom_line(aes(group=trt, color=trt))+
  facet_wrap(~depth)

corn<-gather(cc, key = depth, value = valuecc, sw_5:st_100)
ferprairie<-gather(pf, key = depth, value = valuepf, sw_5:st_100)
prairie<-gather(p, key = depth, value = valuep, sw_5:st_100)

now<-full_join(corn, ferprairie, by = c("Date", "depth"))
then<-full_join(now, prairie, by = c("Date", "depth"))

dif<-then%>%
  select(Date, depth, valuecc, valuepf, valuep)%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y"))%>%
  mutate(ccVSpf = valuecc-valuepf)%>%
  mutate(ccVSp = valuecc-valuep)%>%
  mutate(pfVSp = valuepf - valuep)%>%
  mutate(year = year(Date))

ggplot(waterdif, aes(x = Date, y = ccVSp.x))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = ccVSpf.y), color = "red")+
  facet_wrap(~depth, ncol=1)

ggplot(tempdif, aes(x = Date, y = ccVSpf))+
  geom_point()+
  geom_hline(yintercept = 0)+
  facet_wrap(~depth)

waterdif<-dif[dif$depth %in% c("sw_5", "sw_10", "sw_15", "sw_35", "sw_75", "sw_100"),]
watermeans<-waterdif%>%
  group_by(depth)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  select(depth, ccVSpf, ccVSp, pfVSp)
waterdif<-full_join(waterdif, watermeans, by = "depth")
  

tempdif<-dif[dif$depth %in% c("st_5", "st_10", "st_15", "st_35", "st_75", "st_100"),]
tempdif<-filter(tempdif, ccVSpf<7)
tempmeans<-tempdif%>%
  group_by(depth)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  select(depth, ccVSpf, ccVSp, pfVSp)
tempdif<-full_join(tempdif, tempmeans, by = "depth")

yf_theme<-theme_bw()+
  theme(#panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    legend.position='none', legend.title=element_blank(),
    legend.text = element_text(size=12),
    strip.text.x = element_text(size = 14),
    axis.title.x = element_text(size=16,vjust=-0.5),
    axis.title.y = element_text(size=16,angle=90, vjust=1.2),
    axis.text.x = element_text(colour="black", size=14),
    axis.text.y = element_text(colour="black", size=18))
theme_set(yf_theme)

Wccvspfig<-ggplot(waterdif, aes(x = Date, y = ccVSp.x))+
  geom_point()+
  ggtitle("Continuous Corn - Prairie Water Content")+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = ccVSp.y), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Volumetric water content")

Wccvspffig<-ggplot(waterdif, aes(x = Date, y = ccVSpf.x))+
  geom_point()+
  ggtitle("Continuous Corn - Fertilized Prairie Water Content")+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = ccVSpf.y), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Volumetric water content")

Wpfvspfig<-ggplot(waterdif, aes(x = Date, y = pfVSp.x))+
  geom_point()+
  ggtitle("Fertilized Prairie - Prairie Water Content")+
  ylim(-0.1, 0.25)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = pfVSp.y), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Volumetric water content")

Tccvspfig<-ggplot(tempdif, aes(x = Date, y = ccVSp.x))+
  geom_point()+
  ggtitle("Continuous Corn - Prairie Temperatures")+
  ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = ccVSp.y), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Temp (C)")

Tccvspffig<-ggplot(tempdif, aes(x = Date, y = ccVSpf.x))+
  geom_point()+
  ggtitle("Continuous Corn - Fertilized Prairie Temperatures")+
  ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = ccVSpf.y), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Temp (C)")

Tpfvspfig<-ggplot(tempdif, aes(x = Date, y = pfVSp.x))+
  geom_point()+
  ggtitle("Fertilized Prairie - Prairie Temperatures")+
  ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = pfVSp.y), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Temp(C)")

library(grid)
vplayout<- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)

pdf("Figures/COBS water content and temperature differences between treatments at depth.pdf", width = 16, height = 16)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3, heights = unit(c(15), "null"), widths = unit(c(5,5,5), "null"))))
print(Wccvspffig, vp=vplayout(1,1))
print(Wccvspfig, vp=vplayout(1,2))
print(Wpfvspfig, vp=vplayout(1,3))

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3, heights = unit(c(15), "null"), widths = unit(c(5,5,5), "null"))))
print(Tccvspffig, vp=vplayout(1,1))
print(Tccvspfig, vp=vplayout(1,2))
print(Tpfvspfig, vp=vplayout(1,3))
dev.off()

####Differences from 10 cm (depth comparisons)
##CC
Tdepthcc<-cc%>%
 select(trt, Date, st_5:st_100)%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y" ))%>%
  mutate(dif5 = st_10-st_5)%>%
  mutate(dif15 = st_5-st_15)%>%
  mutate(dif35 = st_5-st_35)%>%
  mutate(dif75 = st_5-st_75)%>%
  mutate(dif100 = st_5-st_100)%>%
  select(trt, Date, dif10:dif100)%>%
  gather(key = depth, value=temp, dif10:dif100)

Tdepthccmeans<-Tdepthcc%>%
  group_by(depth)%>%
  summarise(mean(temp))
Tdepthcc<-full_join(Tdepthcc, Tdepthccmeans, by = "depth")

cctempdif<-ggplot(Tdepthcc, aes(x = Date, y = temp))+
  geom_point()+
  ggtitle("Continuous Corn difference from 10 cm")+
  #ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = mean(temp)), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Temp(C)")

Wdepthcc<-cc%>%
  select(trt, Date, sw_5:sw_100)%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y" ))%>%
  mutate(dif5 = sw_10-sw_5)%>%
  mutate(dif15 = sw_10-sw_15)%>%
  mutate(dif35 = sw_10-sw_35)%>%
  mutate(dif75 = sw_10-sw_75)%>%
  mutate(dif100 = sw_10-sw_100)%>%
  select(trt, Date, dif5:dif100)%>%
  gather(key = depth, value=water, dif5:dif100)

Wdepthccmeans<-Wdepthcc%>%
  group_by(depth)%>%
  summarise(mean(water))
ccwaterdif<-full_join(Wdepthcc, Wdepthccmeans, by = "depth")

ccwaterdif<-ggplot(Wdepthcc, aes(x = Date, y = water))+
  geom_point()+
  ggtitle("Continuous Corn difference from 10 cm")+
  #ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = mean(water)), color = "red")+
  facet_wrap(~depth, ncol=1)

tempdif<-dif[dif$depth %in% c("st_5", "st_10", "st_15", "st_35", "st_75", "st_100"),]
tempdif<-filter(tempdif, ccVSpf<7)
tempmeans<-tempdif%>%
  group_by(depth)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  select(depth, ccVSpf, ccVSp, pfVSp)
tempdif<-full_join(tempdif, tempmeans, by = "depth")

##pf
Tdepthp<-pf%>%
  select(trt, Date, st_5:st_100)%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y" ))%>%
  mutate(dif5 = st_10-st_5)%>%
  mutate(dif15 = st_5-st_15)%>%
  mutate(dif35 = st_5-st_35)%>%
  mutate(dif75 = st_5-st_75)%>%
  mutate(dif100 = st_5-st_100)%>%
  select(trt, Date, dif5:dif100)%>%
  gather(key = depth, value=temp, dif5:dif100)

Tdepthpfmeans<-Tdepthpf%>%
  group_by(depth)%>%
  summarise(mean(temp))
Tdepthpf<-full_join(Tdepthpf, Tdepthpfmeans, by = "depth")

pftempdif<-ggplot(Tdepthpf, aes(x = Date, y = temp))+
  geom_point()+
  ggtitle("Fertilized prairie difference from 10 cm")+
  #ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = mean(temp)), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Temp(C)")

Wdepthpf<-pf%>%
  select(trt, Date, sw_5:sw_100)%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y" ))%>%
  mutate(dif5 = sw_10-sw_5)%>%
  mutate(dif15 = sw_10-sw_15)%>%
  mutate(dif35 = sw_10-sw_35)%>%
  mutate(dif75 = sw_10-sw_75)%>%
  mutate(dif100 = sw_10-sw_100)%>%
  select(trt, Date, dif5:dif100)%>%
  gather(key = depth, value=water, dif5:dif100)

Wdepthpfmeans<-Wdepthpf%>%
  group_by(depth)%>%
  summarise(mean(water))
pfwaterdif<-full_join(Wdepthpf, Wdepthpfmeans, by = "depth")

pfwaterdif<-ggplot(Wdepthpf, aes(x = Date, y = water))+
  geom_point()+
  ggtitle("Fertilized prairie difference from 10 cm")+
  #ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = mean(water)), color = "red")+
  facet_wrap(~depth, ncol=1)

##p
Tdepthp<-p%>%
  select(trt, Date, st_5:st_100)%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y" ))%>%
  mutate(dif5 = st_10-st_5)%>%
  mutate(dif15 = st_5-st_15)%>%
  mutate(dif35 = st_5-st_35)%>%
  mutate(dif75 = st_5-st_75)%>%
  mutate(dif100 = st_5-st_100)%>%
  select(trt, Date, dif5:dif100)%>%
  gather(key = depth, value=temp, dif5:dif100)

Tdepthpmeans<-Tdepthp%>%
  group_by(depth)%>%
  summarise(mean(temp))
Tdepthp<-full_join(Tdepthp, Tdepthpmeans, by = "depth")

ptempdif<-ggplot(Tdepthp, aes(x = Date, y = temp))+
  geom_point()+
  ggtitle("Prairie difference from 10 cm")+
  #ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = mean(temp)), color = "red")+
  facet_wrap(~depth, ncol=1)+
  ylab("Temp(C)")

Wdepthp<-p%>%
  select(trt, Date, sw_5:sw_100)%>%
  mutate(Date = as.Date(Date, "%d/%m/%Y" ))%>%
  mutate(dif5 = sw_10-sw_5)%>%
  mutate(dif15 = sw_10-sw_15)%>%
  mutate(dif35 = sw_10-sw_35)%>%
  mutate(dif75 = sw_10-sw_75)%>%
  mutate(dif100 = sw_10-sw_100)%>%
  select(trt, Date, dif5:dif100)%>%
  gather(key = depth, value=water, dif5:dif100)

Wdepthpmeans<-Wdepthp%>%
  group_by(depth)%>%
  summarise(mean(water))
pwaterdif<-full_join(Wdepthp, Wdepthpmeans, by = "depth")

pwaterdif<-ggplot(Wdepthp, aes(x = Date, y = water))+
  geom_point()+
  ggtitle("Prairie difference from 10 cm")+
  #ylim(-2.5, 7.5)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = mean(water)), color = "red")+
  facet_wrap(~depth, ncol=1)

pdf("Figures/COBS water content and temperature differences between depths by treatments.pdf", width = 16, height = 16)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3, heights = unit(c(15), "null"), widths = unit(c(5,5,5), "null"))))
print(ccwaterdif, vp=vplayout(1,1))
print(pwaterdif, vp=vplayout(1,2))
print(pfwaterdif, vp=vplayout(1,3))

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3, heights = unit(c(15), "null"), widths = unit(c(5,5,5), "null"))))
print(cctempdif, vp=vplayout(1,1))
print(ptempdif, vp=vplayout(1,2))
print(pftempdif, vp=vplayout(1,3))
dev.off()