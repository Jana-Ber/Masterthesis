# packages laden
library(haven)
library(sandwich)
library(lmtest)
library(ggplot2)

#Daten einlesen
df <- read_sav("/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Dataprep_01_03.sav")

# ------ Hypothesis 1.1 --------
df_clean <- na.omit(df[c("IPV_phy", "Loneliness_sum", "Dep_sum")]) # mir stehen 8683 zur Verfügung, weshalb ich 4341 für die Berechnung verwende

set.seed(124) # Für Reproduzierbarkeit (mein Geburtstag)
n <- 4341 # Größe jedes Subsamples

# Zufällige Indizes für die Aufteilung
indizes <- sample(1:(2*n), 2*n, replace = FALSE)
subsample1 <- df_clean[indizes[1:n], ]
table(subsample1$IPV_phy)
subsample2 <- df_clean[indizes[(n+1):(2*n)], ]
table(subsample2$IPV_phy)

# Modell für Subsample 1
modell1 <- lm(Dep_sum ~ IPV_phy * Loneliness_sum, data = subsample1)
summary(modell1)

coefs1 <- coefficients(summary(modell1))
mod.df <- modell1$df.residual # model degrees of freedom = 4337
t025 <- qt(p=.975, df=mod.df) # critical t value at 2.5%
# Formula to compute lower and upper 95% CI of the estimate.
7.7867802 - t025*0.20378522; 7.7867802 + t025*0.20378522 # Intercept
2.0453201 - t025*0.38140552; 2.0453201 + t025*0.38140552 # IPV_phy
0.3621737 - t025*0.03478640; 0.3621737 + t025*0.03478640 # Loneliness_sum
0.0183535 - t025*0.03478640; 0.0183535 + t025*0.03478640 # IPV_phy:Loneliness_sum
# Manually compute t value
7.7867802/0.20378522 # Intercept
2.0453201/0.38140552 # IPV_phy
0.3621737/0.01999487 # Loneliness_sum
0.0183535/0.03478640 # IPV_phy:Loneliness_sum
# Manually compute 2-sided p value
pt(q=38.2107220, df = mod.df, lower.tail = FALSE)*2 # Intercept
pt(q=5.3625864, df = mod.df, lower.tail = FALSE)*2 # IPV_phy
pt(q=18.1133315, df = mod.df, lower.tail = FALSE)*2 # Loneliness_sum
pt(q=0.5276056, df = mod.df, lower.tail = FALSE)*2 # IPV_phy:Loneliness_sum
# - - - - - - - - - - - - - - - - - - -
ci1 <- confint(modell1) # Konfidenz-Intervall enthält 0 für Interaktion, 95% CI [-0.049, 0.09]
# sandwich (sw) methode ausführen
coefs1sw <- lmtest::coeftest(modell1, vcov=sandwich) #sandwich und summary nicht signifikant
# Compute lower and upper 95% CI of the estimate, use robust standard error.
coefs1sw["(Intercept)","Estimate"] - t025*coefs1sw["(Intercept)","Std. Error"]
coefs1sw["(Intercept)","Estimate"] + t025*coefs1sw["(Intercept)","Std. Error"]
# Try whether confint also works with this sandwich output.
ci1sw <- confint(object = coefs1sw) # Nice.


# Modell für Subsample 2
modell2 <- lm(Dep_sum ~ IPV_phy * Loneliness_sum, data = subsample2)
coefs2 <- coefficients(summary(modell2))
ci2 <- confint(modell2) # Konfidenzintervall enthält die null nicht 95% CI [0.02, 0.15]
coefs2sw <- lmtest::coeftest(modell2, vcov=sandwich) #sandwich und summary/confint sind signifikant
ci2sw <- confint(coefs2sw)

# calculation of the effect size (Formel aus Selya et al., 2012)
modell0 <- lm(Dep_sum ~ IPV_phy, data = df_clean)
R2_modell0 <- summary(modell0)$r.squared
R2_modell1 <- summary(modell1)$r.squared
R2_modell2 <- summary(modell2)$r.squared
(R2_modell1 - R2_modell0) / (1 - R2_modell1)#effect size f2 = 0.12
(R2_modell2 - R2_modell0) / (1 - R2_modell2)#effect size f2 = 0.10




# ------ Hypothesis 1.2 --------

df_clean1 <- na.omit(df[c("IPV_sx", "Loneliness_sum", "Dep_sum")]) 
set.seed(124) # Für Reproduzierbarkeit (mein Geburtstag)
n <- 4691 # Größe jedes Subsamples

# Zufällige Indizes für die Aufteilung
indizes <- sample(1:(2*n), 2*n, replace = FALSE)
subsample3 <- df_clean1[indizes[1:n], ]
table(subsample3$IPV_sx)
subsample4 <- df_clean1[indizes[(n+1):(2*n)], ]
table(subsample4$IPV_sx)

# Modell für Subsample 1
modell3 <- lm(Dep_sum ~ IPV_sx * Loneliness_sum, data = subsample3)
coefs3 <- coefficients(summary(modell3))
ci3 <- confint(modell3) # Konfidenz-Intervall enthält 0 für Interaktion, 95% CI [-0.049, 0.09]
coefs3sw <- lmtest::coeftest(modell3, vcov=sandwich) #beide nicht signifikant, aber der P-wert unterscheidet sich trotzdem sehr stark
ci3sw <- confint(coefs3sw)

# Modell für Subsample 2
modell4 <- lm(Dep_sum ~ IPV_sx * Loneliness_sum, data = subsample4)
coefs4 <- coefficients(summary(modell4))
ci4 <- confint(modell4) # Konfidenzintervall enthält die null nicht 95% CI [0.02, 0.15]
coefs4sw <- lmtest::coeftest(modell4, vcov=sandwich) #beide nicht sig.
ci4sw <- confint(coefs4sw)

#computing effect size
modell0 <- lm(Dep_sum ~ IPV_sx, data = df_clean1)
R2_modell0 <- summary(modell0)$r.squared
R2_modell3 <- summary(modell3)$r.squared
R2_modell4 <- summary(modell4)$r.squared
(R2_modell3 - R2_modell0) / (1 - R2_modell3) # f2 = 0.11
(R2_modell4 - R2_modell0) / (1 - R2_modell4) #f2 = 0.12




# ------ Hypothesis 1.3 ------
df_clean2 <- na.omit(df[c("IPV_psy", "Loneliness_sum", "Dep_sum")]) 

set.seed(124) # Für Reproduzierbarkeit (mein Geburtstag)
n <- 3622 # Größe jedes Subsamples

# Zufällige Indizes für die Aufteilung
indizes <- sample(1:(2*n), 2*n, replace = FALSE)
subsample5 <- df_clean2[indizes[1:n], ]
table(subsample5$IPV_psy)
subsample6 <- df_clean2[indizes[(n+1):(2*n)], ]
table(subsample6$IPV_psy)

# Modell für Subsample 5
modell5 <- lm(Dep_sum ~ IPV_psy * Loneliness_sum, data = subsample5)
coefs5 <- coefficients(summary(modell5))
ci5 <- confint(modell5) 
coefs5sw <- lmtest::coeftest(modell5, vcov=sandwich) # beides signifikant
ci5sw <- confint(coefs5sw)

# Modell für Subsample 6
modell6 <- lm(Dep_sum ~ IPV_psy * Loneliness_sum, data = subsample6)
coefs6 <- coefficients(summary(modell6))
ci6 <- confint(modell6) # Konfidenzintervall enthält die null nicht 95% CI [0.02, 0.15]
coefs6sw <- lmtest::coeftest(modell6, vcov=sandwich) #beides nicht signifikant
ci6sw <- confint(coefs6sw)
# computing effect size
modell0 <- lm(Dep_sum ~ IPV_psy, data = df_clean2)
R2_modell0 <- summary(modell0)$r.squared
R2_modell5 <- summary(modell5)$r.squared
R2_modell6 <- summary(modell6)$r.squared
(R2_modell5 - R2_modell0) / (1 - R2_modell5) # f2 = 0.10
(R2_modell6 - R2_modell0) / (1 - R2_modell6) #f2 = 0.11



# ------  Hypothesis 2.1 --------
df_clean3 <- na.omit(df[c("IPV_phy", "ses_cat", "Dep_sum")])

df_clean3$ses_cat <- factor(df_clean3$ses_cat,
                           levels = c(1, 2, 3),
                           labels = c("low", "medium", "high"))
df_clean3$ses_cat <- factor(df_clean3$ses_cat)

# Modell
modell7 <- lm(Dep_sum ~ IPV_phy * ses_cat, data = df_clean3)
table(df_clean3$IPV_phy)
coefs7 <- coefficients(summary(modell7))
ci7 <- confint(modell7)
coefs7sw <- lmtest::coeftest(modell7, vcov=sandwich) #bei sandwich und summary, beide interaktionsterme nicht signifikant
ci7sw <- confint(coefs7sw)
# effect size
modell0 <- lm(Dep_sum ~ IPV_phy, data = df_clean3)
R2_modell0 <- summary(modell0)$r.squared
R2_modell7 <- summary(modell7)$r.squared
(R2_modell7 - R2_modell0) / (1 - R2_modell7)




# ------ Hypothesis 2.2 -------
df_clean4 <- na.omit(df[c("IPV_sx", "ses_cat", "Dep_sum")]) # mir stehen 6543 zur Verfügung
df_clean4$ses_cat <- factor(df_clean4$ses_cat, levels = c(1, 2, 3), labels = c("low", "medium", "high"))
df_clean4$ses_cat <- factor(df_clean4$ses_cat)

modell8 <- lm(Dep_sum ~ IPV_sx * ses_cat, data = df_clean4)
table(df_clean4$IPV_sx)
coefs8 <- coefficients(summary(modell8))
ci8 <- confint(modell8)
coefs8sw <- lmtest::coeftest(modell8, vcov=sandwich) # ein interaktionsterm (IPV_sex:ses_catmedium)ist nicht mehr signifikant (mit der summary funktion war er es noch)
ci8sw <- confint(coefs8sw)

# effect size
modell0 <- lm(Dep_sum ~ IPV_sx, data = df_clean4)
R2_modell0 <- summary(modell0)$r.squared
R2_modell8 <- summary(modell8)$r.squared
(R2_modell8 - R2_modell0) / (1 - R2_modell8)



# ----- Hypothesis 2.3 ------
df_clean5 <- na.omit(df[c("IPV_psy", "ses_cat", "Dep_sum")])
df_clean5$ses_cat <- factor(df_clean5$ses_cat, levels = c(1, 2, 3), labels = c("low", "medium", "high"))
df_clean5$ses_cat <- factor(df_clean5$ses_cat)

modell9 <- lm(Dep_sum ~ IPV_psy * ses_cat, data = df_clean5)
table(df_clean5$IPV_psy)
coefs9 <- coefficients(summary(modell9))
ci9 <- confint(modell9)
coefs9sw <- lmtest::coeftest(modell9, vcov=sandwich) # sig. stimmt bei beiden überein (ein interaktionsterm ist sig. der andere nicht)
ci9sw <- confint(coefs9sw)
# effect size
modell0 <- lm(Dep_sum ~ IPV_psy, data = df_clean5)
R2_modell0 <- summary(modell0)$r.squared
R2_modell9 <- summary(modell9)$r.squared
(R2_modell9 - R2_modell0) / (1 - R2_modell9)

#Collect all 9 non-robust models in a list.
coefLs <- list(m1=cbind(coefs1, ci1),
               m2=cbind(coefs2, ci2),
               m3=cbind(coefs3, ci3),
               m4=cbind(coefs4, ci4),
               m5=cbind(coefs5, ci5),
               m6=cbind(coefs6, ci6),
               m7=cbind(coefs7, ci7),
               m8=cbind(coefs8, ci8),
               m9=cbind(coefs9, ci9))
# Display in console
coefLs

#MM Collect all 9 robust (sw = sandwich) models in a list.
coefswLs <- list(m1sw=cbind(coefs1sw, ci1sw),
                 m2sw=cbind(coefs2sw, ci2sw),
                 m3sw=cbind(coefs3sw, ci3sw),
                 m4sw=cbind(coefs4sw, ci4sw),
                 m5sw=cbind(coefs5sw, ci5sw),
                 m6sw=cbind(coefs6sw, ci6sw),
                 m7sw=cbind(coefs7sw, ci7sw),
                 m8sw=cbind(coefs8sw, ci8sw),
                 m9sw=cbind(coefs9sw, ci9sw))
# Display in console
coefswLs

# Own custom function, used to format the table according to how the table is presented in the thesis document.
reformat <- function(coefList=NULL, tblColnames = c("Estimate", "SE", "LL", "UL", "t", "p")) {
    for(i in 1:length(coefList)) {
        coefList[[i]] <- coefList[[i]][,c(1,2,5,6,3,4)]
        colnames(coefList[[i]]) <- tblColnames
    }
    return(coefList)
}

# Apply own custom function and display in console
(coefLsNew <- reformat(coefList=coefLs))
(coefswLsNew <- reformat(coefList=coefswLs))


# ------- creating plots (forest plots for hypotheses 1.1 - 1.3) -------

#forest plot for hypothesis 1.1
n <- 4331 # Größe jedes Subsamples
set.seed(1)
# Make 100 random different seeds (but each time the same random seeds, see set.seed(1))
seeds <- sample(124:198273645, size=100)
# Empty vectors with which to collect the results.
pval1 <- pval2 <- estimate1 <- estimate2 <- ci1 <- ci2 <- c()
# s <- seeds[1]
for(s in seeds) {
  set.seed(s)
  # Zufällige Indizes für die Aufteilung
  indizes <- sample(1:(2*n), 2*n, replace = FALSE)
  subsample1 <- df_clean[indizes[1:n], ]
  subsample2 <- df_clean[indizes[(n+1):(2*n)], ]
  # Run models
  modell1 <- lm(Dep_sum ~ IPV_phy * Loneliness_sum, data = subsample1)
  # coefs1 <- coefficients(summary(modell3))
  # Replace with robust model
  coefs1 <- lmtest::coeftest(modell1, vcov=sandwich)
  modell2 <- lm(Dep_sum ~ IPV_phy * Loneliness_sum, data = subsample2)
  # coefs2 <- coefficients(summary(modell3))
  # Replace with robust model
  coefs2 <- lmtest::coeftest(modell2, vcov=sandwich)
  # Take p value of model 1 and 2
  pval1 <- c(pval1,
             coefs1["IPV_phy:Loneliness_sum","Pr(>|t|)"])
  pval2 <- c(pval2,
             coefs2["IPV_phy:Loneliness_sum","Pr(>|t|)"])
  # Collect the point estimates of the interaction effect
  estimate1 <- c(estimate1,
                 coefs1["IPV_phy:Loneliness_sum","Estimate"])
  estimate2 <- c(estimate2,
                 coefs2["IPV_phy:Loneliness_sum","Estimate"])
  # Collect 95% CI of the interaction effect
  # ci1 <- c(ci1, as.numeric(confint(modell3)["IPV_sx:Loneliness_sum",]))
  # ci2 <- c(ci2, as.numeric(confint(modell4)["IPV_sx:Loneliness_sum",]))
  # Replace with confint for robust model
  ci1 <- c(ci1, as.numeric(confint(coefs1)["IPV_phy:Loneliness_sum",]))
  ci2 <- c(ci2, as.numeric(confint(coefs2)["IPV_phy:Loneliness_sum",]))
}
alphaSigLevel <- .05
length(which(pval1 <= alphaSigLevel)) # für subsample 1 sind 23% der Interaktionen signifikant
length(which(pval2 <= alphaSigLevel)) # für subsample 2 sinf 17% der Interaktionen signifikant


# Forest plot for point estimates and 95CI for modell1
cis1 <- data.frame(matrix(data=ci1, ncol=2, byrow = TRUE))
colnames(cis1) <- c("l95ci", "u95ci")
mod1Df <- cbind(data.frame(estimate1, pval1), cis1)
mod1Df <- mod1Df[order(-mod1Df$estimate1, mod1Df$pval1),]
mod1Df$rep <- factor(1:nrow(mod1Df))
mod1Plot <- 
  ggplot(mod1Df, aes(x=estimate1, y=rep)) +
  geom_errorbar(width=.15, aes(xmin=l95ci, xmax=u95ci), linewidth=1, color="grey") +
  geom_point(shape=124, size=5) +
  geom_vline(xintercept = 0, linetype = "dashed", color="black", linewidth=1) +
  xlab(label="Interaction effect in linear regression model") +
  ylab(label="100 repeated 50/50 split of the total sample") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.ticks.y = element_blank(),
    axis.text.y=element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    legend.position = "none")

ggsave(filename="forestPlotModell1.1a.png", plot = mod1Plot, path = "/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Plots", device = "png", width=8, height=7, units="in", dpi=300)

# Forest plot for point estimates and 95CI for modell2
cis2 <- data.frame(matrix(data=ci2, ncol=2, byrow = TRUE))
colnames(cis2) <- c("l95ci", "u95ci")
mod2Df <- cbind(data.frame(estimate2, pval2), cis2)
mod2Df <- mod2Df[order(-mod2Df$estimate2, mod2Df$pval2),]
mod2Df$rep <- factor(1:nrow(mod2Df))
mod2Plot <- 
  ggplot(mod2Df, aes(x=estimate2, y=rep)) +
  geom_errorbar(width=.15, aes(xmin=l95ci, xmax=u95ci), linewidth=1, color="grey") +
  geom_point(shape=124, size=5) +
  geom_vline(xintercept = 0, linetype = "dashed", color="black", linewidth=1) +
  xlab(label="Interaction effect in linear regression model") +
  ylab(label="100 repeated 50/50 split of the total sample") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.ticks.y = element_blank(),
    axis.text.y=element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    legend.position = "none")

ggsave(filename="forestPlotModell1.1b.png", plot = mod2Plot, path = "/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Plots", device = "png", width=8, height=7, units="in", dpi=300)

# Forest plot for hypothesis 1.2 
n <- 4691 # Größe jedes Subsamples
set.seed(1)
# Make 100 random different seeds (but each time the same random seeds, see set.seed(1))
seeds <- sample(124:198273645, size=100)
# Empty vectors with which to collect the results.
pval3 <- pval4 <- estimate3 <- estimate4 <- ci3 <- ci4 <- c()
# s <- seeds[1]
for(s in seeds) {
  set.seed(s)
  # Zufällige Indizes für die Aufteilung
  indizes <- sample(1:(2*n), 2*n, replace = FALSE)
  subsample3 <- df_clean1[indizes[1:n], ]
  subsample4 <- df_clean1[indizes[(n+1):(2*n)], ]
  # Run models
  modell3 <- lm(Dep_sum ~ IPV_sx * Loneliness_sum, data = subsample3)
  # coefs1 <- coefficients(summary(modell3))
  # Replace with robust model
  coefs3 <- lmtest::coeftest(modell3, vcov=sandwich)
  modell4 <- lm(Dep_sum ~ IPV_sx * Loneliness_sum, data = subsample4)
  # coefs2 <- coefficients(summary(modell3))
  # Replace with robust model
  coefs4 <- lmtest::coeftest(modell4, vcov=sandwich)
  # Take p value of model 1 and 2
  pval3 <- c(pval3,
             coefs3["IPV_sx:Loneliness_sum","Pr(>|t|)"])
  pval4 <- c(pval4,
             coefs4["IPV_sx:Loneliness_sum","Pr(>|t|)"])
  # Collect the point estimates of the interaction effect
  estimate3 <- c(estimate3,
                 coefs3["IPV_sx:Loneliness_sum","Estimate"])
  estimate4 <- c(estimate4,
                 coefs4["IPV_sx:Loneliness_sum","Estimate"])
  # Collect 95% CI of the interaction effect
  # ci1 <- c(ci1, as.numeric(confint(modell3)["IPV_sx:Loneliness_sum",]))
  # ci2 <- c(ci2, as.numeric(confint(modell4)["IPV_sx:Loneliness_sum",]))
  # Replace with confint for robust model
  ci3 <- c(ci3, as.numeric(confint(coefs3)["IPV_sx:Loneliness_sum",]))
  ci4 <- c(ci4, as.numeric(confint(coefs4)["IPV_sx:Loneliness_sum",]))
}
alphaSigLevel <- .05
length(which(pval3 <= alphaSigLevel)) 
length(which(pval4 <= alphaSigLevel)) 

# Forest plot for point estimates and 95CI for modell3
cis3 <- data.frame(matrix(data=ci3, ncol=2, byrow = TRUE))
colnames(cis3) <- c("l95ci", "u95ci")
mod3Df <- cbind(data.frame(estimate3, pval3), cis3)
mod3Df <- mod3Df[order(-mod3Df$estimate3, mod3Df$pval3),]
mod3Df$rep <- factor(1:nrow(mod3Df))
mod3Plot <- 
  ggplot(mod3Df, aes(x=estimate3, y=rep)) +
  geom_errorbar(width=.15, aes(xmin=l95ci, xmax=u95ci), linewidth=1, color="grey") +
  geom_point(shape=124, size=5) +
  geom_vline(xintercept = 0, linetype = "dashed", color="black", linewidth=1) +
  xlab(label="Interaction effect in linear regression model") +
  ylab(label="100 repeated 50/50 split of the total sample") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.ticks.y = element_blank(),
    axis.text.y=element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    legend.position = "none")

ggsave(filename="forestPlotModell1.2a.png", plot = mod3Plot, path = "/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Plots", device = "png", width=8, height=7, units="in", dpi=300)

# Forest plot for point estimates and 95CI for modell4
cis4 <- data.frame(matrix(data=ci4, ncol=2, byrow = TRUE))
colnames(cis4) <- c("l95ci", "u95ci")
mod4Df <- cbind(data.frame(estimate4, pval4), cis4)
mod4Df <- mod4Df[order(-mod4Df$estimate4, mod4Df$pval4),]
mod4Df$rep <- factor(1:nrow(mod4Df))
mod4Plot <- 
  ggplot(mod4Df, aes(x=estimate4, y=rep)) +
  geom_errorbar(width=.15, aes(xmin=l95ci, xmax=u95ci), linewidth=1, color="grey") +
  geom_point(shape=124, size=5) +
  geom_vline(xintercept = 0, linetype = "dashed", color="black", linewidth=1) +
  xlab(label="Interaction effect in linear regression model") +
  ylab(label="100 repeated 50/50 split of the total sample") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.ticks.y = element_blank(),
    axis.text.y=element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    legend.position = "none")

ggsave(filename="forestPlotModell1.2b.png", plot = mod4Plot, path = "/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Plots", device = "png", width=8, height=7, units="in", dpi=300)

# forest plot for hypothesis 1.3
n <- 3622 # Größe jedes Subsamples
set.seed(1)
# Make 100 random different seeds (but each time the same random seeds, see set.seed(1))
seeds <- sample(124:198273645, size=100)
# Empty vectors with which to collect the results.
pval5 <- pval6 <- estimate5 <- estimate6 <- ci5 <- ci6 <- c()
# s <- seeds[1]
for(s in seeds) {
  set.seed(s)
  # Zufällige Indizes für die Aufteilung
  indizes <- sample(1:(2*n), 2*n, replace = FALSE)
  subsample5 <- df_clean2[indizes[1:n], ]
  subsample6 <- df_clean2[indizes[(n+1):(2*n)], ]
  # Run models
  modell5 <- lm(Dep_sum ~ IPV_psy * Loneliness_sum, data = subsample5)
  # coefs1 <- coefficients(summary(modell3))
  # Replace with robust model
  coefs5 <- lmtest::coeftest(modell5, vcov=sandwich)
  modell6 <- lm(Dep_sum ~ IPV_psy * Loneliness_sum, data = subsample6)
  # coefs2 <- coefficients(summary(modell3))
  # Replace with robust model
  coefs6 <- lmtest::coeftest(modell6, vcov=sandwich)
  # Take p value of model 1 and 2
  pval5 <- c(pval5,
             coefs6["IPV_psy:Loneliness_sum","Pr(>|t|)"])
  pval6 <- c(pval6,
             coefs6["IPV_psy:Loneliness_sum","Pr(>|t|)"])
  # Collect the point estimates of the interaction effect
  estimate5 <- c(estimate5,
                 coefs5["IPV_psy:Loneliness_sum","Estimate"])
  estimate6 <- c(estimate6,
                 coefs6["IPV_psy:Loneliness_sum","Estimate"])
  # Collect 95% CI of the interaction effect
  # ci1 <- c(ci1, as.numeric(confint(modell3)["IPV_psy:Loneliness_sum",]))
  # ci2 <- c(ci2, as.numeric(confint(modell4)["IPV_psy:Loneliness_sum",]))
  # Replace with confint for robust model
  ci5 <- c(ci5, as.numeric(confint(coefs5)["IPV_psy:Loneliness_sum",]))
  ci6 <- c(ci6, as.numeric(confint(coefs6)["IPV_psy:Loneliness_sum",]))
}
alphaSigLevel <- .05
length(which(pval5 <= alphaSigLevel)) 
length(which(pval6 <= alphaSigLevel)) 

# Forest plot for point estimates and 95CI for modell5
cis5 <- data.frame(matrix(data=ci5, ncol=2, byrow = TRUE))
colnames(cis5) <- c("l95ci", "u95ci")
mod5Df <- cbind(data.frame(estimate5, pval5), cis5)
mod5Df <- mod5Df[order(-mod5Df$estimate5, mod5Df$pval5),]
mod5Df$rep <- factor(1:nrow(mod5Df))
mod5Plot <- 
  ggplot(mod5Df, aes(x=estimate5, y=rep)) +
  geom_errorbar(width=.15, aes(xmin=l95ci, xmax=u95ci), linewidth=1, color="grey") +
  geom_point(shape=124, size=5) +
  geom_vline(xintercept = 0, linetype = "dashed", color="black", linewidth=1) +
  xlab(label="Interaction effect in linear regression model") +
  ylab(label="100 repeated 50/50 split of the total sample") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.ticks.y = element_blank(),
    axis.text.y=element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    legend.position = "none")

ggsave(filename="forestPlotModell1.3a.png", plot = mod5Plot, path = "/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Plots", device = "png", width=8, height=7, units="in", dpi=300)

# Forest plot for point estimates and 95CI for modell6
cis6 <- data.frame(matrix(data=ci6, ncol=2, byrow = TRUE))
colnames(cis6) <- c("l95ci", "u95ci")
mod6Df <- cbind(data.frame(estimate6, pval6), cis6)
mod6Df <- mod6Df[order(-mod6Df$estimate6, mod6Df$pval6),]
mod6Df$rep <- factor(1:nrow(mod6Df))
mod6Plot <- 
  ggplot(mod6Df, aes(x=estimate6, y=rep)) +
  geom_errorbar(width=.15, aes(xmin=l95ci, xmax=u95ci), linewidth=1, color="grey") +
  geom_point(shape=124, size=5) +
  geom_vline(xintercept = 0, linetype = "dashed", color="black", linewidth=1) +
  xlab(label="Interaction effect in linear regression model") +
  ylab(label="100 repeated 50/50 split of the total sample") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.ticks.y = element_blank(),
    axis.text.y=element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    legend.position = "none")

ggsave(filename="forestPlotModell1.3b.png", plot = mod6Plot, path = "/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Plots", device = "png", width=8, height=7, units="in", dpi=300)

