# Exploratory and cross-validated confirmatory factor analysis.

library(tidyverse)
library(psych)
library(parameters)
library(lavaan)

# Own function (see Howard, 2016; in order to check 0.4-0.3-0.2 rule)
rule432 <- function(loadingsObject) {
    # Convert loadings() output to data.frame:
    efaLoadingDf <- data.frame(
        matrix(as.numeric(loadingsObject),
               attributes(loadingsObject)$dim,
               dimnames=attributes(loadingsObject)$dimnames))
    # See loadings function (stats pkg: ?loadings):
    # The signs of the loadings vectors are arbitrary for both factor analysis and PCA
    efaLoadingDf <- abs(efaLoadingDf)
    # Check a (0.4), b (0.3), and c (0.2) rule successively:
    ruleA <- ruleB <- ruleC <- c()
    for(i in 1:nrow(efaLoadingDf)) {
        # Rule a) load on primary factor with > .4
        idxMaxLoad <- which(efaLoadingDf[i,] == max(efaLoadingDf[i,]))
        ruleA <- c(ruleA, efaLoadingDf[i,idxMaxLoad]>.4)
        # Rule b) loading on non-primary factor be <.3
        ruleB <- c(ruleB, all(efaLoadingDf[i,-idxMaxLoad]<.3))
        # Rule c) Diff between primary factor loading and alternative factor loadings >.2
        dominantFacLoad <- efaLoadingDf[i,idxMaxLoad]
        # Absolute difference between dominant factor loading and loading on other factors.
        absDiffToOtherFacLoad <- as.numeric(abs(efaLoadingDf[i,-idxMaxLoad] - dominantFacLoad))
        ruleC <- c(ruleC, all(absDiffToOtherFacLoad>.2))
    }
    ruleDf <- data.frame(ruleA, ruleB, ruleC)
    ruleDf$violation <- apply(ruleDf, 1, function(x) any(x==FALSE))
    rownames(ruleDf) <- rownames(efaLoadingDf)
    list(ruleDf=ruleDf, efaLoadingDf=efaLoadingDf)
}

# Own function to remove items, if required.
itemRemoval <- function(efaLoading, ctbItems) {
    # Which items shall be retained? Apply .4-.3-.2 rule:
    efa_psychLoadLs <- rule432(efaLoading)
    print(efa_psychLoadLs)  # Print to console
    # Items that violate the rule
    violItems <- rownames(efa_psychLoadLs[["ruleDf"]][efa_psychLoadLs[["ruleDf"]]$violation,])
    if(length(violItems)==0) {
        print("N O  r e m o v a l")
        return(ctbItems)
    } else {
        print("R e m o v a l")
        print(ctbItems[match(violItems, ctbItems)])
        return(ctbItems[-match(violItems, ctbItems)])
    }
}

# Read the data (data must be in the directory that you are currently in)
getwd() # Directory that you are currently in.
dat <- haven::read_sav("/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Dataprep_01_03.sav")
dim(dat)

ctbItems <- c("f610_2", "f610_3", "f610_4", "f610_8", "f610_9", "f610_11", "f610_12")

dat <- as.data.frame(dat)

# str(dat[,ctbItems])

head(dat[,ctbItems])

dat[,ctbItems][dat[,ctbItems]>=6] <- NA

summary(dat[,ctbItems])

idxNA <- apply(dat[,ctbItems], 1, function(x) {
    if(any(is.na(x))) {
        TRUE
    } else {
        FALSE
    }
})

dat1 <- dat[!idxNA,]
dim(dat1)

summary(dat1[,ctbItems])


# # Temporarily outcommented ---------------------------------------------
# # OPTIONS 1, 2, OR 3 (select one)
# # ------------------
# # 1 = No transformation, keep all 15 items
# # 2 = Log transformation, remove item generic_ct_2 a priori
# # 3 = Log transformation, keep all 15 items
# SELECTEDOPTION <- 3
# 
# ctbItemsCovid <- c("covid_scepticism_1", "covid_scepticism_2", "covid_scepticism_3", "covid_cause_2", "covid_reason_3", "covid_specific_10", "covid_specific_15", "covid_specific_17", "covid_specific_22", "covid_specific_29")
# # 
# ctbItemsGeneric <- c("generic_ct_1", "generic_ct_2", "generic_ct_3", "generic_ct_4", "generic_ct_5")
# ctbItems <- c(ctbItemsCovid, ctbItemsGeneric)
# 
# # Add one to all outcome values, so that original value 0 equals 0 after log-trans.:
# dat[,ctbItems] <- dat[,ctbItems]+1
# 
# # log-transform 
# dat <- dat %>% mutate(across(all_of(ctbItems), list(log=log)), .keep="all")
# names(dat)
# 
# # If OPTION 1 is going to be selected, this makes sure that the raw items will be used.
# ctbItems0 <- paste0(ctbItems, "_log")
# 
# # ------------------
# if(SELECTEDOPTION == 1) {
#     # 1 = No transformation, keep all 15 items
# } else if(SELECTEDOPTION == 2) {
#     # A priori: Remove generic_ct_2_log
#     # # 2 = Log transformation, remove item generic_ct_2 a priori
#     ctbItems <- ctbItems0[!grepl("generic_ct_2_log", ctbItems0)]
# } else if(SELECTEDOPTION == 3) {
#     # Take all
#     # 3 = Log transformation, keep all 15 items
#     ctbItems <- ctbItems0
# }
# 
# # Check
# cbind(ctbItems)
# # Temporarily outcommented ---------------------------------------------

# Correlation matrix of ctbItems
# - - - - - - - - - - - - - - - - - - - - -
cormat <- cor(dat1[,rev(ctbItems)])
cormat[upper.tri(cormat, diag = TRUE)] <- NA
cormat[,7:1]

# Use only k_split function (thanks to unknown function author out there in the www):
# Create 'randomly' (pseudo-randomly) drawn subsamples
k_split <- function(df, k) {
    folds <- split(sample(nrow(df), nrow(df), replace=FALSE), as.factor(1:k))
    lapply(folds, function(idxs) df[idxs, ])
}

# 3-split:
set.seed(3)
datSplit3 <- k_split(dat1[,ctbItems], 3)
lapply(datSplit3, dim)
names(datSplit3) <- c("efa", "cfa", "cfa1")

# Check factor structure again for each split in the 3-split scenario:
check_factorstructure(datSplit3$efa[,ctbItems])
check_factorstructure(datSplit3$cfa[,ctbItems])
check_factorstructure(datSplit3$cfa1[,ctbItems])

# Step 2a and 2b:
# - - - - - - - -
# VSP ("elbow") combined with parallel analysis and MAP.
# MAP = Minimum Average Partial correlation [lower value is better].

# Full N:
# fm="pa" for principal axis factor solution.
efa_psych.parallel <- psych::fa.parallel(dat1[,ctbItems], fa="fa", fm="pa", sim=FALSE, plot = TRUE)

psych::VSS(dat[,ctbItems], plot = FALSE) # "The Velicer MAP achieves a minimum of 0.03

# Steps 3 - 5:
# - - - - - -
# Function for item removal (obeying .4-.3-.2 rule by Howard (2016):

# Round no.1

nFactors <- 1

# Rotation method: direct oblimin rotation with a delta of zero (quartimin)
# scores = "tenBerge" = pattern matrix for factor loading inspection.
efa_psych <- psych::fa(r=dat[,ctbItems], nfactors = nFactors, fm="pa", rotate = "quartimin", scores = "tenBerge")
summary(efa_psych)

# Display factor loadings:
loadings(efa_psych)

ctbItemsNew <- itemRemoval(loadings(efa_psych), ctbItems = ctbItems)

efa_psychLoadLs <- rule432(loadings(efa_psych))

# # Round no.2
# 
# # Run EFA procedure again, with reduced number of items:
# efa_psychNew <- psych::fa(r=dat[,ctbItemsNew], nfactors = nFactors, fm="pa", rotate = "quartimin", scores = "tenBerge")
# summary(efa_psychNew)
# 
# # Check whether Howard's (2016) .4-.3-.2 rule suggests green light:
# (efa_psychLoadLsNew <- rule432(loadings(efa_psychNew)))
# ctbItemsNew1 <- itemRemoval(loadings(efa_psychNew), ctbItems = ctbItemsNew)
# 
# # Round no.3
# 
# efa_psychNew1 <- psych::fa(r=dat[,ctbItemsNew1], nfactors = nFactors, fm="pa", rotate = "quartimin", scores = "tenBerge")
# summary(efa_psychNew1)
# # Check whether Howard's (2016) .4-.3-.2 rule suggests green light:
# (efa_psychLoadLsNew1 <- rule432(loadings(efa_psychNew1)))
# ctbItemsNew2 <- itemRemoval(loadings(efa_psychNew1), ctbItems = ctbItemsNew1)


# Step 6:
# - - - -
# Cross-validate EFA with CFA

# Set number of factors to 2.
# ---------------------
setNumFacStep6 <- 1

# 3-split:
(trainingSplit3 <- psych::fa(r=datSplit3$efa[,ctbItemsNew], nfactors=setNumFacStep6, fm="pa", rotate = "quartimin", scores = "tenBerge") %>% parameters::efa_to_cfa())
# Important: cross-validation data is the a priori separated "cfa" subset of full dataset.
# 1st cross-validation:
cfaSplit3.1 <- lavaan::cfa(trainingSplit3, data=datSplit3$cfa[,ctbItemsNew])
# summary(cfaSplit3.1, standardized = TRUE)
# 2nd cross-validation:
cfaSplit3.2 <- lavaan::cfa(trainingSplit3, data=datSplit3$cfa1[,ctbItemsNew])
# summary(cfaSplit3.2, standardized = TRUE)

# Results overview:
compare <- performance::compare_performance(cfaSplit3.1, cfaSplit3.2)
compareDf <- as.data.frame(compare)
(compareDf1 <- compareDf %>% mutate(across(where(is.numeric), round, digits=2)))
