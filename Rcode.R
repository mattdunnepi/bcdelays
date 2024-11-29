###########################################################################################################
  #Author: Matthew Dunn, MPH, PhD
#Code for article "A latent class assessment of healthcare access factors and disparities in breast cancer care timeliness" 
#published in PLOS medicine 

#* Datasets used in this analysis are available upon submission of a letter of intent and approval from 
#the Carolina Breast Study Steering Committee (https://ciphr.unc.edu/cbcs-loi-form.php) and IRB approval. 
#The data are not publicly available to protect privacy of study participants. 
###########################################################################################################
*////////////////////////////////////////////////////////////////////////

  
  ###LOAD PACKAGES###########

library(vtable)
library(rstatix)
library(gtsummary)
library(flextable)
library(ggplot2)
library(cowplot)
library(scales)
library(VennDiagram)
library(eulerr)
library(formattable)
library(ggrepel) 
library(ggalluvial)
library(dplyr)
library(RColorBrewer)
library(corrplot)
library(hrbrthemes)



###Supplemental Table 1####

newdestable <- read.csv("C:/Users/matth/Dissertation/newdestable.csv", header=T, na.strings=c(""," ", "NA"))
newdestable[sapply(newdestable, is.numeric)] <- lapply(newdestable[sapply(newdestable, is.numeric)], as.factor)

newdestable %>% 
  select(agecat, ESTSIZE, Stage, er_status, her2_status, incomecat2, educat, foreign, employtype,
         marital2, nowinsx, RUCA2Ax, financex, transx, regularx, screen, detect, diagcat, surgcat, race) %>%  # keep only the columns of interest
  tbl_summary(by = race) %>% 
  add_overall() %>%       
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Participant race**") %>%
  as_gt() %>%
  gt::gtsave(filename = "C:/Users/matth/Dissertation/supptable1.html")


####TABLE 1##########


classes %>% 
  select(ses3classfinal, barriers2class, util5class2, latestage, delay, PRO_TREAT, race) %>%  # keep only the columns of interest
  tbl_summary(by = race) %>% 
  add_overall() %>%       
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Participant race**") %>%
  as_gt() %>%
  gt::gtsave(filename = "C:/Users/matth/Dissertation/table1.html")

###FIGURE 1, Flow Chart ###########

flow <- read.csv("C:/Users/matth/Dissertation/flowdiag.csv")
flow$strata = factor(flow$strata, levels = unique(flow$strata))
flow$delay = factor(flow$delay, levels = unique(flow$delay))
flow$numdelays = factor(flow$numdelays, levels = unique(flow$numdelays))



flowb <- read.csv("C:/Users/matth/Dissertation/flowdiag_b.csv")
flowb$strata = factor(flowb$strata, levels = unique(flowb$strata))
flowb$delay = factor(flowb$delay, levels = unique(flowb$delay))
flowb$numdelays = factor(flowb$numdelays, levels = unique(flowb$numdelays))


flownb <- read.csv("C:/Users/matth/Dissertation/flowdiag_nb.csv")
flownb$strata = factor(flownb$strata, levels = unique(flownb$strata))
flownb$delay = factor(flownb$delay, levels = unique(flownb$delay))
flownb$numdelays = factor(flownb$numdelays, levels = unique(flownb$numdelays))

colors <- hcl.colors(4, "red-purple",rev = "TRUE")



colors2 <- c("lightpink","purple","red", "darkred")


f1a<- ggplot(data = flow,
             aes(axis1 = latestage, axis2 = delay, axis3= PRO_TREAT, y = Frequency )) +
  geom_flow(aes(fill = numdelays)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("latestage", "delay", "Response"),
                   expand = c(0.15, 0.05)) + 
  scale_fill_manual(values = colors, name="Number of delays") + ggtitle("A) Overall\n   " ) +
  theme(legend.text=element_text(size=14),legend.key.size = unit(4, 'in')) + theme_void()

f1b <- ggplot(data = flowb,
              aes(axis1 = latestage, axis2 = delay, axis3= PRO_TREAT, y = Frequency )) +
  geom_flow(aes(fill = numdelays)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("latestage", "delay", "Response"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = colors, name="Number of delays") + ggtitle("B) Black women" ) +
  theme(legend.text=element_text(size=14),legend.key.size = unit(4, 'in')) + theme_void()

f1c <- ggplot(data = flownb,
              aes(axis1 = latestage, axis2 = delay, axis3= PRO_TREAT, y = Frequency )) +
  geom_flow(aes(fill = numdelays)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("latestage", "delay", "Response"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = colors, name="Number of delays") + ggtitle("C) non-Black women" ) +
  theme(legend.text=element_text(size=14),legend.key.size = unit(4, 'in')) + theme_void()

tiff("C:/Users/matth/Dissertation/fig1.tiff", units="in", width=14, height=8, res=300)
ggdraw() +
  draw_plot(f1a,  x = 0, y = 0.6, width = 0.7, height = 0.4) +
  draw_plot(f1b, x = 0, y = 0.3, width = 0.7, height = 0.3) +
  draw_plot(f1c, x = 0, y = 0, width = 0.7, height = 0.3) 
dev.off()



######FIGURE 2, Forest Plots, Delayed Dx Models ######################


stage_overall <- read.csv("C:/Users/matth/Dissertation/Fig2A.csv")
stage_overall$lcat = factor(stage_overall$lcat, levels = unique(stage_overall$lcat))

f2a <- ggplot(stage_overall, aes(x=lcat, y=rfd, ymin=ll, ymax=ul,fill=group,color=group)) + 
  #specify position here
  geom_linerange(linewidth=1,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_color_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c( -10, 0, 10, 20,30), n.breaks = 5, limits=c(-10,30)) +
  coord_flip() + theme_bw() + ggtitle("A)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12)) +
  theme(axis.text.y = element_text(hjust = 0))
f2a

stage_race <- read.csv("C:/Users/matth/Dissertation/Fig2B.csv")
stage_race$lcat = factor(stage_race$lcat, levels = unique(stage_race$lcat))
stage_race$group = factor(stage_race$group, levels = unique(stage_race$group))

f2b <- ggplot(stage_race, aes(x=lcat, y=rfd, ymin=ll, ymax=ul, fill=group, color=group)) + 
  #specify position here
  geom_linerange(linewidth=1,position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.7)) +
  scale_fill_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_color_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c( -10, 0, 10, 20,30), n.breaks = 5, limits=c(-10,30)) +
  coord_flip() + theme_bw() + ggtitle("B)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12),axis.text.y = element_text(hjust = 0))
f2b

tiff("C:/Users/matth/Dissertation/fig2.tiff", units="in", width=14, height=8, res=300)
ggdraw() +
  draw_plot(f2a,  x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(f2b, x = 0, y = 0, width = 0.5, height = 0.5) 
dev.off()

######FIGURE 3, DELAYED TREATMENT MODELS, FOREST PLOTS ######################


delay_overall <- read.csv("C:/Users/matth/Dissertation/Fig3A.csv")
delay_overall$lcat = factor(delay_overall$lcat, levels = unique(delay_overall$lcat))


f3a <- ggplot(delay_overall, aes(x=lcat, y=rfd, ymin=ll, ymax=ul,fill=group,color=group)) + 
  #specify position here
  geom_linerange(linewidth=1,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_color_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-20, -10, 0, 10, 20,30), n.breaks = 6, limits=c(-20,30)) +
  coord_flip() + theme_bw() + ggtitle("A)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12)) +
  theme(axis.text.y = element_text(hjust = 0))
f3a

delay_race <- read.csv("C:/Users/matth/Dissertation/Fig3B.csv")
delay_race$lcat = factor(delay_race$lcat, levels = unique(delay_race$lcat))
delay_race$group = factor(delay_race$group, levels = unique(delay_race$group))

f3b <- ggplot(delay_race, aes(x=lcat, y=rfd, ymin=ll, ymax=ul, fill=group, color=group)) + 
  #specify position here
  geom_linerange(size=1,position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.7)) +
  scale_fill_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_color_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-20, -10, 0, 10, 20,30), n.breaks = 6, limits=c(-20,30)) +
  coord_flip() + theme_bw() + ggtitle("B)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12),axis.text.y = element_text(hjust = 0))
f3b

tiff("C:/Users/matth/Dissertation/fig3.tiff", units="in", width=14, height=8, res=300)
ggdraw() +
  draw_plot(f3a,  x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(f3b, x = 0, y = 0, width = 0.5, height = 0.5) 
dev.off()

######FIGURE S2, DELAYED TREATMENT SENSITIVITY ANALYSIS ######################

delay_sen <- read.csv("C:/Users/matth/Dissertation/delay45sens.csv")
delay_sen$lcat = factor(delay_sen$lcat, levels = unique(delay_sen$lcat))


f4sena <- ggplot(delay_sen, aes(x=lcat, y=rfd2, ymin=ll2, ymax=ul2,fill=group,color=group)) + 
  #specify position here
  geom_linerange(linewidth=1,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_color_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-20, -10, 0, 10, 20,30), n.breaks = 6, limits=c(-20,30)) +
  coord_flip() + theme_bw() + ggtitle("A)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12)) +
  theme(axis.text.y = element_text(hjust = 0))
f4sena

delay_senrace <- read.csv("C:/Users/matth/Dissertation/delay45sensrace.csv")
delay_senrace$lcat = factor(delay_senrace$lcat, levels = unique(delay_senrace$lcat))
delay_senrace$group = factor(delay_senrace$group, levels = unique(delay_senrace$group))

f4senb <- ggplot(delay_senrace, aes(x=lcat, y=rfd2, ymin=ll2, ymax=ul2, fill=group, color=group)) + 
  #specify position here
  geom_linerange(size=1,position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.7)) +
  scale_fill_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_color_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-20, -10, 0, 10, 20,30), n.breaks = 6, limits=c(-20,30)) +
  coord_flip() + theme_bw() + ggtitle("B)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12),axis.text.y = element_text(hjust = 0))
f4senb

tiff("C:/Users/matth/Dissertation/delaysens.tiff", units="in", width=14, height=8, res=300)
ggdraw() +
  draw_plot(f4sena,  x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(f4senb, x = 0, y = 0, width = 0.5, height = 0.5) 
dev.off()

######FIGURE 4, Prolonged Treatment models, Forest plots ######################

pro_overall <- read.csv("C:/Users/matth/Dissertation/Fig5A.csv")
pro_overall$lcat = factor(pro_overall$lcat, levels = unique(pro_overall$lcat))

f5a <- ggplot(pro_overall, aes(x=lcat, y=rfd2, ymin=ll2, ymax=ul2,fill=group,color=group)) + 
  #specify position here
  geom_linerange(linewidth=1,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_color_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-20, -10, 0, 10, 20), n.breaks = 5, limits=c(-20,20)) +
  coord_flip() + theme_bw() + ggtitle("A)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12)) +
  theme(axis.text.y = element_text(hjust = 0))
f5a

prolonged_race <- read.csv("C:/Users/matth/Dissertation/Fig5B.csv")
prolonged_race$lcat = factor(prolonged_race$lcat, levels = unique(prolonged_race$lcat))
prolonged_race$group = factor(prolonged_race$group, levels = unique(prolonged_race$group))

f5b <- ggplot(prolonged_race, aes(x=lcat, y=rfd2, ymin=ll2, ymax=ul2, fill=group, color=group)) + 
  #specify position here
  geom_linerange(size=1,position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.7)) +
  scale_fill_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_color_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-20, -10, 0, 10, 20), n.breaks = 5, limits=c(-20,20)) +
  coord_flip() + theme_bw() + ggtitle("B)" )  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12),axis.text.y = element_text(hjust = 0))
f5b

tiff("C:/Users/matth/Dissertation/fig5.tiff", units="in", width=14, height=8, res=300)
ggdraw() +
  draw_plot(f5a,  x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(f5b, x = 0, y = 0, width = 0.5, height = 0.5) 
dev.off()


######FIGURE S3, Prolonged Treatment models, sensitivity analyses by ER and HER2 status ######################

prolonged_er <- read.csv("C:/Users/matth/Dissertation/F5A_er.csv")
prolonged_er$lcat = factor(prolonged_er$lcat, levels = unique(prolonged_er$lcat))
prolonged_er$group = factor(prolonged_er$group, levels = unique(prolonged_er$group))

f5_er <- ggplot(prolonged_er, aes(x=lcat, y=rfd, ymin=ll, ymax=ul, fill=group, color=group)) + 
  #specify position here
  geom_linerange(linewidth=1,position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.7)) +
  scale_fill_manual(breaks=c("pos", "neg"), values=c("purple", "green"), labels=c("Positive","Negative")) +
  scale_color_manual(breaks=c("pos", "neg"), values=c("purple", "green"), labels=c("Positive","Negative")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), n.breaks = 10, limits=c(-40,50)) +
  coord_flip() + theme_bw() + ggtitle("Prolonged treatment by ER staus" )  + 
  theme(legend.position="bottom", legend.title = element_blank(),axis.text=element_text(size=12),axis.text.y = element_text(hjust = 0))
f5_er

prolonged_her <- read.csv("C:/Users/matth/Dissertation/F5A_her2.csv")
prolonged_her$lcat = factor(prolonged_er$lcat, levels = unique(prolonged_er$lcat))
prolonged_her$group = factor(prolonged_er$group, levels = unique(prolonged_er$group))

f5_her <- ggplot(prolonged_her, aes(x=lcat, y=rfd, ymin=ll, ymax=ul, fill=group, color=group)) + 
  #specify position here
  geom_linerange(linewidth=1,position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.7)) +
  scale_fill_manual(breaks=c("pos", "neg"), values=c("purple", "green"), labels=c("Positive","Negative")) +
  scale_color_manual(breaks=c("pos", "neg"), values=c("purple", "green"), labels=c("Positive","Negative")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), n.breaks = 10, limits=c(-40,50)) +
  coord_flip() + theme_bw() + ggtitle("Prolonged treatment by HER2 staus" )  + 
  theme(legend.position="bottom", legend.title = element_blank(),axis.text=element_text(size=12),axis.text.y = element_text(hjust = 0))
f5_her




tiff("C:/Users/matth/Dissertation/fig5er_her.tiff", units="in", width=14, height=8, res=300)
ggdraw() +
  draw_plot(f5_er,  x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(f5_her, x = 0, y = 0, width = 0.5, height = 0.5) 
dev.off()



######FIGURE S4, OncotypeDx models, forest plots ######################

oncodx_overall <- read.csv("C:/Users/matth/Dissertation/Fig4A.csv")
oncodx_overall$lcat = factor(oncodx_overall$lcat, levels = unique(oncodx_overall$lcat))

f4a <- ggplot(oncodx_overall, aes(x=lcat, y=rfd, ymin=ll, ymax=ul,fill=group,color=group)) + 
  #specify position here
  geom_linerange(linewidth=1,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_color_manual(values=c("black"), labels=c("No usual source of office-based care")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-30,-20, -10, 0, 10, 20,30), n.breaks = 7, limits=c(-30,30)) +
  coord_flip() + theme_bw() + ggtitle("A)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12)) +
  theme(axis.text.y = element_text(hjust = 0))
f4a

odx_race <- read.csv("C:/Users/matth/Dissertation/Fig4B.csv")
odx_race$lcat = factor(odx_race$lcat, levels = unique(odx_race$lcat))
odx_race$group = factor(odx_race$group, levels = unique(odx_race$group))

f4b <- ggplot(odx_race, aes(x=lcat, y=rfd, ymin=ll, ymax=ul, fill=group, color=group)) + 
  #specify position here
  geom_linerange(size=1,position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.7)) +
  scale_fill_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_color_manual(breaks=c("b", "nb"), values=c("red", "blue"), labels=c("Black","non-Black")) +
  scale_x_discrete(name="", limits=rev ) +
  scale_y_continuous(name="Relative Frequency Difference (95% Confidence Interval)", breaks = c(-30,-20, -10, 0, 10, 20,30), n.breaks = 7, limits=c(-30,30)) +
  coord_flip() + theme_bw() + ggtitle("B)")  + 
  theme(legend.position="none", legend.title = element_blank(),axis.text=element_text(size=12),axis.text.y = element_text(hjust = 0))
f4b


tiff("C:/Users/matth/Dissertation/fig4.tiff", units="in", width=14, height=8, res=300)
ggdraw() +
  draw_plot(f4a,  x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(f4b, x = 0, y = 0, width = 0.5, height = 0.5) 
dev.off()



#####FIGURE 5, Heat Map     ##############################

crossclassoutcomes <- read.csv("C:/Users/matth/Dissertation/newfigure5.csv")
crossclassoutcomes$xval = factor(crossclassoutcomes$xval, levels = unique(crossclassoutcomes$xval))
crossclassoutcomes$yval = factor(crossclassoutcomes$yval, levels = unique(crossclassoutcomes$yval))

f5a <- ggplot(crossclassoutcomes, aes(x = xval, y = yval, fill = prev1)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", limits=c(0, 70), breaks=seq(0,70,by=10)) +
  coord_fixed() +
  guides(fill = guide_colourbar(title="Delayed diagnosis (%)")) +
  theme_minimal() + theme(axis.title = element_blank(),axis.text.x=element_blank())

f5b <- ggplot(crossclassoutcomes, aes(x = xval, y = yval, fill = prev2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue",na.value = "white", limits=c(0, 70), breaks=seq(0,70,by=10)) +
  coord_fixed() +
  guides(fill = guide_colourbar(title="Delayed treatment (%)")) + 
  theme_minimal() + theme(axis.title = element_blank(),axis.text.x=element_blank())

f5c <- ggplot(crossclassoutcomes, aes(x = xval, y = yval, fill = prev3)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", limits=c(0, 70), breaks=seq(0,70,by=10)) +
  coord_fixed() +
  guides(fill = guide_colourbar(title="Prolonged treatment (%)")) + 
  theme_minimal() + theme(axis.title = element_blank(),axis.text.x=element_blank())


tiff("C:/Users/matth/Dissertation/newfig5.tiff", units="in", width=12, height=10, res=300)
ggdraw() +
  draw_plot(f5a,  x = 0, y = 0.6, width = 1.5, height = 0.3) +
  draw_plot(f5b, x = 0, y = 0.3, width = 1.5, height = 0.3) +
  draw_plot(f5c, x = 0, y = 0, width = 1.5, height = 0.3) 
dev.off()




