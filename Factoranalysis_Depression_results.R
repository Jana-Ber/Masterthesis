# Results from Factor analysis for the items "F610_2", "F610_3", "F610_4", "F610_8", "F610_9", "F610_11", "F610_12" (psych. Belastung)

# > summary(efa_psych)
# 
# Factor analysis with Call: psych::fa(r = dat[, ctbItems], nfactors = nFactors, rotate = "quartimin", 
#                                      scores = "tenBerge", fm = "pa")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is 77  and the objective function was  0.86 
# The number of observations was  10264  with Chi Square =  8869.21  with prob <  0 
# 
# The root mean square of the residuals (RMSA) is  0.07 
# The df corrected root mean square of the residuals is  0.08 
# 
# Tucker Lewis Index of factoring reliability =  0.791
# RMSEA index =  0.105  and the 10 % confidence intervals are  0.104 0.107
# BIC =  8158
# -----------------------------------------------------------

# Meine Ergebnisse, basierend auf Stichprobe, nachdem alle fehlenden Werte in den relevanten Variablen entfernt wurden: Sample size verringert sich von 10246 auf 10190.

# Explorative FA deutet auf eine Dimension hin, wenn man konventionelle Regel akzeptiert, wonach Eigenwerte > 1 als Kriterium gelten, die Anzahl Dimensionen abzuschätzen. Ein Eigenwert > 1 deutet gemäss dieser 'Regel' auf eine Dimension hin.
# Hierfür verwendeter Befehl:
# efa_psych.parallel <- psych::fa.parallel(dat1[,ctbItems], fa="fa", fm="pa", sim=FALSE, plot = TRUE)

# psych::VSS(dat[,ctbItems], plot = FALSE)
# In dieser Übersicht zeigt sich der geringste MAP Wert für eine Dimension (weil erste Zeile der Tabelle).
# Very Simple Structure
# Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
#           n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
# VSS complexity 1 achieves a maximimum of 0.79  with  1  factors
# VSS complexity 2 achieves a maximimum of 0.86  with  2  factors
# 
# The Velicer MAP achieves a minimum of 0.05  with  1  factors 
# BIC achieves a minimum of  -16.02  with  3  factors
# Sample Size adjusted BIC achieves a minimum of  -6.48  with  3  factors
# 
# Statistics by number of factors 
# vss1 vss2   map dof   chisq     prob sqresid  fit RMSEA  BIC  SABIC complex  eChisq    SRMR eCRMS eBIC
# 1 0.79 0.00 0.047  14 2.8e+03  0.0e+00     2.9 0.79 0.139 2654 2698.0     1.0 2.4e+03 7.4e-02 0.091 2257
# 2 0.64 0.86 0.076   8 6.2e+02 3.1e-129     2.0 0.86 0.087  549  574.2     1.3 3.3e+02 2.7e-02 0.045  251
# 3 0.53 0.77 0.156   3 1.2e+01  8.5e-03     1.7 0.88 0.017  -16   -6.5     1.6 6.9e+00 4.0e-03 0.011  -21
# 4 0.48 0.72 0.235  -1 7.1e-05       NA     1.6 0.88    NA   NA     NA     1.9 2.8e-05 8.0e-06    NA   NA
# 5 0.47 0.72 0.466  -4 1.6e-07       NA     1.5 0.89    NA   NA     NA     1.9 6.6e-08 3.9e-07    NA   NA
# 6 0.48 0.72 1.000  -6 2.5e-08       NA     1.5 0.89    NA   NA     NA     1.9 1.1e-08 1.6e-07    NA   NA
# 7 0.48 0.72    NA  -7 2.5e-08       NA     1.5 0.89    NA   NA     NA     1.9 1.1e-08 1.6e-07    NA   NA

# 'Konfirmatorische' FA mit Annahme einer Dimension, siehe diese Befehle:
# nFactors <- 1
# 
# # Rotation method: direct oblimin rotation with a delta of zero (quartimin)
# # scores = "tenBerge" = pattern matrix for factor loading inspection.
# efa_psych <- psych::fa(r=dat[,ctbItems], nfactors = nFactors, fm="pa", rotate = "quartimin", scores = "tenBerge")
# summary(efa_psych)
# 
# # Display factor loadings:
# loadings(efa_psych)

# ergibt diese Ausgabe:
# Loadings:
#     PA1  
# F610_2  0.573
# F610_3  0.609
# F610_4  0.516
# F610_8  0.757
# F610_9  0.728
# F610_11 0.556
# F610_12 0.618
# 
# PA1
# SS loadings    2.760
# Proportion Var 0.394

# Anwendung der .4-.3-.2 Regel von Howard (2016), d.h. diese Funktion ausführen:
# ctbItemsNew <- itemRemoval(loadings(efa_psych), ctbItems = ctbItems)

# ergibt diese Ausgabe:

# $ruleDf
# ruleA ruleB ruleC violation
# F610_2   TRUE  TRUE  TRUE     FALSE
# F610_3   TRUE  TRUE  TRUE     FALSE
# F610_4   TRUE  TRUE  TRUE     FALSE
# F610_8   TRUE  TRUE  TRUE     FALSE
# F610_9   TRUE  TRUE  TRUE     FALSE
# F610_11  TRUE  TRUE  TRUE     FALSE
# F610_12  TRUE  TRUE  TRUE     FALSE
# 
# $efaLoadingDf
# PA1
# F610_2  0.5728081
# F610_3  0.6089703
# F610_4  0.5161508
# F610_8  0.7568664
# F610_9  0.7280924
# F610_11 0.5562990
# F610_12 0.6178913
# 
# [1] "N O  r e m o v a l"
