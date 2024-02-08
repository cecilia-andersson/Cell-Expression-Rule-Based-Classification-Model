# Running ROSETTA

# Load ROSETTA
set.seed(1)
library(R.ROSETTA)

# load project dataset: 'Project4'

dim(Project4)[2]-1

# top from mcfs
selected <- c(1251, 1001, 501, 251, 751, 1161, 911, 836, 318, 4, 437, 602, 678, 856, 797, 1153, 178, 68, 1086, 274, 1106, 1087, 1259)

selected2 <- c(1251, 1001, 501, 251, 751, 1161, 911, 836, 318, 4, 437, 602, 678, 856, 797, 1153, 178, 68, 1086, 274, 1106, 1087, 896, 1087, 428, 102, 1259)

selected3 <- c(1251, 1001, 501, 251, 751, 1161, 911, 836, 318, 4, 437, 602, 678, 856, 797, 1153, 178, 68, 1086, 274, 1106, 1087, 896, 1087, 428, 102, 2, 820, 568, 814, 411, 5, 161, 330, 1030, 818, 645, 1259)


only_major <- Project4[selected]
only_major

only_major2 <- Project4[selected2]

only_major3 <- Project4[selected3]

table(only_major$ethnicity)
table(only_major2$ethnicity)
table(Project4$ethnicity)

African <- rosetta(only_major, roc = TRUE, clroc = "Afro")
?rosetta
Afrules <- African$main
Afqual <- African$quality

African2 <- rosetta(only_major2, roc = TRUE, clroc = "Afro")
?rosetta
Afrules2 <- African2$main
Afqual2 <- African2$quality
Afrules2

African3 <- rosetta(only_major3, roc = TRUE, clroc = "Afro")
Afrules3 <- African3$main
Afqual3 <- African3$quality
Afqual3

Asian <- rosetta(only_major, roc= TRUE, clroc = "Asian")
Asrules <- Asian$main
Asqual <- Asian$quality

Asian3 <- rosetta(only_major3, roc = TRUE, clroc = "Asian")
Asrules3 <- Asian3$main
Asqual3 <- Asian3$quality

Asian$main %>%  dim()
Caucasian$main %>% dim()
African$main %>% dim()

Caucasian <- rosetta(only_major, roc= TRUE, clroc = "Cau")
Caurules <- Caucasian$main
Cauqual <- Caucasian$quality

Cauqual3
Caurules3

 Caucasian3 <- rosetta(only_major3, roc = TRUE, clroc = "Cau")
Caurules3 <- Caucasian3$main
Cauqual3 <- Caucasian3$quality



#
viewRules()
rulesAfro <- viewRules(rules[rules$ethnicity=="Afro",], setLabels=TRUE, labels=c("low", "medium", "high"))
print(rulesAfro[1:12,])

rulesAfro3 <- viewRules(Afrules3[Afrules3$decision=="Afro",], setLabels=TRUE, labels=c("low", "medium", "high"))

Afrule<- rulesAfro3[1:10,]

rulesAs3 <- viewRules(Asrules3[Asrules3$decision=="Asian",], setLabels=TRUE, labels=c("low", "medium", "high"))
rulesCau3 <- viewRules(Caurules3[Caurules3$decision=="Cau",], setLabels=TRUE, labels=c("low", "medium", "high"))

Asrule<- rulesAs3[1:10,]
Caurule<- rulesCau3[1:10,]

tops <- viewRules(Afrules3, setLabels=TRUE, labels=c("low", "medium", "high"))

top5 <- tops[1:5,]


recalc <- recalculateRules(Afrules3, tops)

features_rules <-viewRules(Afrules3[Afrules3$features = c("MXRA7_Activated_4","MXRA7_Activated_48","age","bmi", "GNLY_Activated_4", "IFIT2_IFNb_4" , "MXRA7_Activated_48" , "MXRA7_Unstim_4","IFIT2_Unstim_4", "CYBB_Activated_48", "FADS2_Activated_48", "UTS2_Activated_48", "weight.kg." , "CD19_Th17_48",  "IFITM3_Activated_48", "NPCDR1_Unstim_4", "UTS2_IFNb_4", "UTS2_Th17_48")])



#Visunet
library(VisuNet)

Afvis <- visunet(African$main)
Afvis2 <-visunet(African2$main)
Afvis3 <- visunet(African3$main)
Asvis3 <- visunet(Asian3$main)

feats <- getFeatures(Afrules3, filter = T, filterType = "pvalue", thr = 0.05)
Af_feat <- feats$features$Afro
Cau_feat <- feats$features$Cau
As_feat <- feats$features$Asian

AfCau<- intersect(Af_feat, Cau_feat)
AfCauAs<- intersect(AfCau, As_feat)

AfCauAs


plotMeanROC(African3)
plotMeanROC(Caucasian3)
plotMeanROC(Asian3)

?plotMeanROC
