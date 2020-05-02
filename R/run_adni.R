## loadpackages ----
options(digits = 3)
source("R/adnimerge_tools.R")
library(ADNIMERGE)
theme_set(theme_bw())

## ----data--------
dd <- adnimerge

# add CDMEMORY to dd, but with screen converted to baseline ----
cdr.bl <- cdr
cdr.bl$VISCODE <- with(cdr.bl, ifelse(VISCODE == "sc", "bl", VISCODE))
dd <- merge(dd, cdr.bl[, c("RID", "VISCODE", "CDMEMORY")], by = c("RID", "VISCODE"), all.x = TRUE)
dd$AGEC <- cut(dd$AGE, breaks = seq(39, 110, by = 10))

cdr$CDRSB <- apply(cdr[, c("CDMEMORY", "CDORIENT", "CDJUDGE", "CDCOMMUN", "CDHOME", "CDCARE")], 1, sum)
adas$ADAS11 <- adas[, "TOTSCORE"]
adas$ADAS13 <- adas[, "TOTAL13"]
mmse$MMSE <- mmse[, "MMSCORE"]

dd.sc <- merge(
  subset(registry, ORIGPROT == COLPROT & VISCODE %in% c("sc", "f"), c("RID", "VISCODE", "EXAMDATE")),
  subset(arm, ORIGPROT == COLPROT, c("RID", "DX")), by = "RID", all.x = TRUE)
dd.sc <- merge(dd.sc,
               subset(ptdemog, ORIGPROT == COLPROT, c("RID", "VISCODE", "AGE", "PTGENDER", "PTEDUCAT", "PTMARRY", "PTETHCAT", "PTRACCAT")),
               by = c("RID", "VISCODE"), all.x = TRUE)
dd.sc <- merge(dd.sc,
               subset(cdr, ORIGPROT == COLPROT, c("RID", "VISCODE", "CDRSB", "CDMEMORY")),
               by = c("RID", "VISCODE"), all.x = TRUE)
dd.sc <- merge(dd.sc,
               subset(adas, ORIGPROT == COLPROT & VISCODE %in% c('f', 'bl'), c("RID", "ADAS13")),
               by = "RID", all.x = TRUE)
dd.sc <- merge(dd.sc,
               subset(mmse, ORIGPROT == COLPROT, c("RID", "VISCODE", "MMSE")),
               by = c("RID", "VISCODE"), all.x = TRUE)
dd.sc <- merge(dd.sc,
               subset(roster, ORIGPROT == COLPROT, c("RID", "PTID")),
               by = "RID", all.x = TRUE)
dd.sc$SITE <- as.factor(unlist(lapply(strsplit(dd.sc$PTID, "_S_"), function(x) x[1])))
dd.sc$DX.bl <- factor(dd.sc$DX,
                      levels = c('NL', 'SMC', 'EMCI', 'LMCI', 'AD'),
                      labels = c('CN', 'SMC', 'EMCI', 'LMCI', 'AD'))
dd.sc$AGEC <- cut(dd.sc$AGE, breaks = seq(39, 110, by = 10))

label(dd$PTEDUCAT) <- "Education"
label(dd$AGE) <- "Age"
label(dd$AGEC) <- "Age"
label(dd$PTGENDER) <- "Sex"
label(dd$PTETHCAT) <- "Ethnicity"
label(dd$PTRACCAT) <- "Race"
label(dd$PTMARRY) <- "Marital"
label(dd$CDRSB) <- "CDR-SB"
label(dd$CDMEMORY) <- "CDR Memory"
label(dd$ADAS11) <- "ADAS 11"
label(dd$ADAS13) <- "ADAS 13"
label(dd$MMSE) <- "MMSE"

# copy labels from dd/adnimerge to dd.sc
for(cc in intersect(colnames(dd), colnames(dd.sc))) label(dd.sc[, cc]) <- label(dd[, cc])

# remove records with NA examdates
dd <- dd[!is.na(dd$EXAMDATE), ]

# indicate the bl withdrawals:
BLTDS <- subset(treatdis, VISCODE == "bl" & WDRAWTYPE == "Full")$RID
dd$VISCODE <- with(dd, ifelse(VISCODE == "bl" & RID %in% BLTDS, "blwd", VISCODE))


f <- DX.bl ~ AGE + AGEC + PTGENDER + PTEDUCAT + PTMARRY + PTETHCAT + PTRACCAT + CDRSB + CDMEMORY + ADAS13 + MMSE
tab <- summary(f, data = dd.sc,
               subset = !is.na(DX.bl) & !is.na(AGE) & VISCODE %in% c('sc', 'f'),
               method = "reverse", test = TRUE, overall = TRUE)
rcmd <- rep("", nrow(summary2matrix(tab)) - 1)
rcmd[seq(1, length(rcmd), by = 2)] <- "rowcolor[gray]{.9}"
latex(tab, round = 3, file = "",
      size = "scriptsize", outer.size = 'tiny', where = 'h!',
      rownamesTexCmd = rcmd, label = "tab1",
      caption = 'Descriptive Statistics by Screen Dx')

## ----results = 'asis'---------------------------------------------------------
tab <- summary(f, data = dd,
               subset = VISCODE == 'bl',
               method = "reverse", test = TRUE, overall = TRUE,
               continuous = 9)
rcmd <- rep("", nrow(summary2matrix(tab)) - 1)
rcmd[seq(1, length(rcmd), by = 2)] <- "rowcolor[gray]{.9}"
latex(tab, file = "", size = "scriptsize", outer.size = 'tiny', where = 'h!', rownamesTexCmd = rcmd, label = "tab2")

## ----results = 'hide'---------------------------------------------------------
print(tab, round = 3, hsep=", ", header.vsep=FALSE, file="bl_summary_DX.bl.csv")

## ----adasmmsebp, fig.show='hold'----------------------------------------------
dd1 <- subset(dd, VISCODE == "bl" & !is.na(DX.bl),
              c("RID", "DX.bl", "ADAS13", "CDRSB", "CDMEMORY", "MMSE", "EcogPtMem", "EcogSPMem", "EcogPtTotal", "EcogSPTotal"))
ggplot(dd1, aes(x = DX.bl, y = ADAS13)) +
  geom_jitter(size = 1, position = position_jitter(h=0)) +
  geom_boxplot(aes(fill = DX.bl), outlier.shape = NA) +
  theme(legend.position = "none")
ggplot(dd1, aes(DX.bl, MMSE)) +
  geom_jitter(size = 1, position = position_jitter(h=0)) +
  geom_boxplot(aes(fill = DX.bl), outlier.shape = NA) +
  theme(legend.position = "none")

## ----cdrbp, fig.show='hold'---------------------------------------------------
ggplot(dd1, aes(DX.bl, CDRSB)) +
  geom_jitter(size = 1, position = position_jitter(h=0)) +
  geom_boxplot(aes(fill = DX.bl), outlier.shape = NA) +
  theme(legend.position = "none")
ggplot(dd1, aes(DX.bl, CDMEMORY)) +
  geom_jitter(size = 1, position = position_jitter(h=0)) +
  geom_boxplot(aes(fill = DX.bl), outlier.shape = NA) +
  theme(legend.position = "none") +
  ylab("CDR-Memory")

## ----ecogmembp, fig.show='hold'-----------------------------------------------
ggplot(dd1, aes(DX.bl, EcogPtMem)) +
  geom_jitter(size = 1, position = position_jitter(h=0)) +
  geom_boxplot(aes(fill = DX.bl), outlier.shape = NA) +
  theme(legend.position = "none") +
  ylab("ECog Self Memory")
ggplot(dd1, aes(DX.bl, EcogSPMem)) +
  geom_jitter(size = 1, position = position_jitter(h=0)) +
  geom_boxplot(aes(fill = DX.bl), outlier.shape = NA) +
  theme(legend.position = "none") +
  ylab("ECog Partner Memory")

## ----ecogtotbp, fig.show='hold'-----------------------------------------------
ggplot(dd1, aes(DX.bl, EcogPtTotal)) +
  geom_jitter(size = 1, position = position_jitter(h=0)) +
  geom_boxplot(aes(fill = DX.bl), outlier.shape = NA) +
  theme(legend.position = "none") +
  ylab("ECog Self Total")
ggplot(dd1, aes(DX.bl, EcogSPTotal)) +
  geom_jitter(size = 1, position = position_jitter(h=0)) +
  geom_boxplot(aes(fill = DX.bl), outlier.shape = NA) +
  theme(legend.position = "none") +
  ylab("ECog Partner Total")

## ----results = 'asis'---------------------------------------------------------
tab <- rbind(
  with(subset(dd1, !is.na(ADAS13)), Hmisc::summarize(ADAS13, by = DX.bl, FUN = summary.default, stat.name = "N")),
  with(subset(dd1, !is.na(MMSE)), Hmisc::summarize(MMSE, by = DX.bl, FUN = summary.default, stat.name = "N")),
  with(subset(dd1, !is.na(CDRSB)), Hmisc::summarize(CDRSB, by = DX.bl, FUN = summary.default, stat.name = "N")),
  with(subset(dd1, !is.na(CDMEMORY)), Hmisc::summarize(CDMEMORY, by = DX.bl, FUN = summary.default, stat.name = "N")),
  with(subset(dd1, !is.na(EcogPtMem)), Hmisc::summarize(EcogPtMem, by = DX.bl, FUN = summary.default, stat.name = "N")),
  with(subset(dd1, !is.na(EcogSPMem)), Hmisc::summarize(EcogSPMem, by = DX.bl, FUN = summary.default, stat.name = "N")),
  with(subset(dd1, !is.na(EcogPtTotal)), Hmisc::summarize(EcogPtTotal, by = DX.bl, FUN = summary.default, stat.name = "N")),
  with(subset(dd1, !is.na(EcogSPTotal)), Hmisc::summarize(EcogSPTotal, by = DX.bl, FUN = summary.default, stat.name = "N")))
rcmd <- rep("", nrow(tab))
rcmd[seq(1, length(rcmd), by = 2)] <- "rowcolor[gray]{.9}"

latex(tab[, -1], file = "", rowlabel = "", rowname = tab[, 1],
      rgroup = c("ADAS13", "MMSE", "CDRSB", "CDR-Memory",
                 "ECog Self Mem", "Ecog Partner Mem", "ECog Self Total", "Ecog Partner Total"),
      digits = 3, where = 'h!', rownamesTexCmd = rcmd, size = "small", outer.size = 'scriptsize', label = "tab5")

## ----longsums, fig.keep='all', results = 'asis', fig.width=8, fig.height=8/1.6, out.width='\\linewidth'----
label(adas$ADAS13) <- "ADAS 13"
longsums(adas, "ADAS13")
label(mmse$MMSE) <- "MMSE"
longsums(mmse, "MMSE")
label(cdr$CDRSB) <- "CDR-SB"
longsums(cdr, "CDRSB")
label(cdr$CDMEMORY) <- "CDR-Memory"
longsums(cdr, "CDMEMORY")

## ----results = "asis"---------------------------------------------------------
toLatex(sessionInfo(), locale=FALSE)
