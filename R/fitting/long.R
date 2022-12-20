library(tidyverse)
library(pROC)


#### LONG COVID ########

## Importing data... org is Moncucco cohort.
org <- read.delim("Moncucco_longCOVID_table.txt",header = TRUE,sep = "\t")
org <- org %>% filter(Group_code<3)
org <- org %>% arrange(Group_code)
org_c <- org %>% filter(Group_code==0)


## Short vs Long

# Subtract medians from chemokines CCL21 CXCL13 CXCL16

org_CCL21 = median(org_c$CCL21, na.rm = TRUE)
org_CXCL13 = median(org_c$CXCL13, na.rm = TRUE)
org_CXCL16 = median(org_c$CXCL16, na.rm = TRUE)


org_sh <- org
org_sh$CCL21 <- org$CCL21 - org_CCL21
org_sh$CXCL13 <- org$CXCL13 - org_CXCL13
org_sh$CXCL16 <- org$CXCL16 - org_CXCL16


## Logistic fit

org_fit <- org_sh
org_fit <- org_fit %>% filter(Group_code>0)
org_fit$Group_code <- ifelse(org_fit$Group_code==1,0,org_fit$Group_code)
org_fit$Group_code <- ifelse(org_fit$Group_code==2,1,org_fit$Group_code)
res <- glm(Group_code~CCL21+CXCL13+CXCL16, family = binomial(link = "logit"),org_fit)
summary(res)

probs <- predict(res,type = "response")
fitted.results <- ifelse(probs > 0.5,1,0)

#fitted results
misClasificError <- mean(fitted.results != org_fit$Group_code)
print(paste0("Misclassification Error: ",round(misClasificError,5)))
print(paste0("AIC: ",round(res$aic,5)))
print(auc(fitted.results,org_fit$Group_code))

plot(probs,main='long COVID - Org')
lines(org_fit$Group_code,col= 'red')
