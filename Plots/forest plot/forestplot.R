library("readxl")

# all
str_all <- read_excel("/Users/yuchen/Desktop/UKBiobank/Tables/all.xlsx")
str_all$percent = round(str_all$percent*100,2)
str_all = str_all[1:29, ]
str_all$`No of \r\nparticipants` <- format(str_all$`No of \r\nparticipants`, big.mark=",") 
str_all$`No of \r\nparticipants` [str_all$`No of \r\nparticipants` == "    NA"] = ""
str_all$`No of \r\ndeath (%)` <- format(str_all$`No of \r\ndeath (%)`, big.mark=",") 
str_all$`No of \r\ndeath (%)` [str_all$`No of \r\ndeath (%)` == "   NA"] = ""
## Labels defining subgroups are a little indented!
subgps <- c(4,5,8,9,12,13,16,17,20,21,24,25,28,29)
str_all$Subgroup[subgps] <- paste("  ",str_all$Subgroup[subgps]) 
## Combine the count and percent column
np_all <- ifelse(str_all$`No of \r\ndeath (%)` != "", paste(str_all$`No of \r\ndeath (%)`," (",str_all$percent,")",sep=""), "")


## The rest of the columns in the table. 
tabletext <- cbind(c("Subgroup","\n",str_all$Subgroup), 
                   c("No. of Participants","\n",str_all$`No of \r\nparticipants`), 
                   c("No. of Death (%)","\n",np_all),
                   c("Hazard Ratio\n (95% CI)","\n",str_all$`Hazard ratio \r\n(95% CI)`), 
                   c("P value for\n interaction","\n",str_all$`P value for \r\ninteraction`))
library(forestplot)

# draw the plot
#tiff(file.path("/Users/yuchen/Desktop/all.tiff"), units="in", width=15, height=10, res=300)
forestplot(labeltext=tabletext, graph.pos=4, 
           mean=c(NA,NA,str_all$estimate), 
           lower=c(NA,NA,str_all$low), upper=c(NA,NA,str_all$high),
           hrzl_lines=list("3" = gpar(lwd=2, col="#99999922"), 
                           "7" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922"),
                           "15" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922"),
                           "23" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922")),
           txt_gp=fpTxtGp(label=gpar(fontfamily="Times", cex=1),
                          ticks=gpar(cex=1),
                          xlab=gpar(cex = 1)),
           shapes_gp = styles,
           is.summary = c(T,T,F,F,F,F,F,F,T,T,F,F,F,F,F,F,T,T,F,F,F,F,F,F,T,T,F,F,F,F,F),
           #col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=1, ci.vertices=FALSE, ci.vertices.height = 0.2
           )
#dev.off()

styles <- fpShapesGp(
  lines = list(
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "black")
  ),
  box = list(
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black")
  ) 
)


# neop
str_neop <- read_excel("/Users/yuchen/Desktop/UKBiobank/Tables/cancer.xlsx")
str_neop$percent = round(str_neop$percent*100,2)
str_neop$`No of \r\nparticipants` <- format(str_neop$`No of \r\nparticipants`, big.mark=",") 
str_neop$`No of \r\nparticipants` [str_neop$`No of \r\nparticipants` == "    NA"] = ""
str_neop$`No of \r\ndeath (%)` <- format(str_neop$`No of \r\ndeath (%)`, big.mark=",") 
str_neop$`No of \r\ndeath (%)` [str_neop$`No of \r\ndeath (%)` == " NA"] = ""
str_neop = str_neop[1:29, ]
str_neop$Subgroup[subgps] <- paste("  ",str_neop$Subgroup[subgps]) 
## Combine the count and percent column
np_neop <- ifelse(str_neop$`No of \r\ndeath (%)` != "", paste(str_neop$`No of \r\ndeath (%)`," (",str_neop$percent,")",sep=""), "")

## The rest of the columns in the table. 
tabletext2 <- cbind(c("Subgroup","\n",str_neop$Subgroup), 
                   c("No. of Participants","\n",str_neop$`No of \r\nparticipants`), 
                   c("No. of Death (%)","\n",np_neop),
                   c("Hazard Ratio\n (95% CI)","\n",str_neop$`Hazard ratio \r\n(95% CI)`), 
                   c("P value for\n interaction","\n",str_neop$`P value for \r\ninteraction`))

# draw the plot
tiff(file.path("/Users/yuchen/Desktop/cancer.tiff"), units="in", width=15, height=10, res=300)
forestplot(labeltext=tabletext2, graph.pos=4, 
           mean=c(NA,NA,str_neop$estimate), 
           lower=c(NA,NA,str_neop$low), upper=c(NA,NA,str_neop$high),
           hrzl_lines=list("3" = gpar(lwd=2, col="#99999922"), 
                           "7" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922"),
                           "15" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922"),
                           "23" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922")),
           txt_gp=fpTxtGp(label=gpar(cex=1.5),
                          ticks=gpar(cex=1.2),
                          xlab=gpar(cex = 1.3)),
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)
dev.off()


# circ
str_circ <- read_excel("/Users/yuchen/Desktop/UKBiobank/Tables/cvd.xlsx")
str_circ$percent = round(str_circ$percent*100,2)
str_circ$`No of \r\nparticipants` <- format(str_circ$`No of \r\nparticipants`, big.mark=",") 
str_circ$`No of \r\nparticipants` [str_circ$`No of \r\nparticipants` == "    NA"] = ""
str_circ$`No of \r\ndeath (%)` <- format(str_circ$`No of \r\ndeath (%)`, big.mark=",") 
str_circ$`No of \r\ndeath (%)` [str_circ$`No of \r\ndeath (%)` == " NA"] = ""
str_circ = str_circ[1:29, ]
str_circ$Subgroup[subgps] <- paste("  ",str_circ$Subgroup[subgps]) 
## Combine the count and percent column
np_circ <- ifelse(str_circ$`No of \r\ndeath (%)` != "", paste(str_circ$`No of \r\ndeath (%)`," (",str_circ$percent,")",sep=""), "")

## The rest of the columns in the table. 
tabletext3 <- cbind(c("Subgroup","\n",str_circ$Subgroup), 
                   c("No. of Participants","\n",str_circ$`No of \r\nparticipants`), 
                   c("No. of Death (%)","\n",np_circ),
                   c("Hazard Ratio\n (95% CI)","\n",str_circ$`Hazard ratio \r\n(95% CI)`), 
                   c("P value for\n interaction","\n",str_circ$`P value for \r\ninteraction`))

# draw the plot
tiff(file.path("/Users/yuchen/Desktop/cvd.tiff"), units="in", width=15, height=10, res=300)
forestplot(labeltext=tabletext3, graph.pos=4, 
           mean=c(NA,NA,str_circ$estimate), 
           lower=c(NA,NA,str_circ$low), upper=c(NA,NA,str_circ$high),
           hrzl_lines=list("3" = gpar(lwd=2, col="#99999922"), 
                           "7" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922"),
                           "15" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922"),
                           "23" = gpar(lwd=122, lineend="butt", columns=c(2:6), col="#99999922")),
           txt_gp=fpTxtGp(label=gpar(cex=1.5),
                          ticks=gpar(cex=1.2),
                          xlab=gpar(cex = 1.3)),
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)
dev.off()
