library(tidyverse)
library(pROC)


#### healthy patients vs. COVID infection ########

## Importing data... org is Moncucco cohort.
org <- read.delim("Moncucco_table.txt",header = TRUE,sep = "\t")
org <- org %>% filter(Group_code<3)
org_c <- org %>% filter(Group_code==0)


## Controls vs Covid19

# Subtract medians from chemokines CCL19 CCL22 CXCL17

org_CCL19 = median(org_c$CCL19, na.rm = TRUE)
org_CCL22 = median(org_c$CCL22, na.rm = TRUE)
org_CXCL17 = median(org_c$CXCL17, na.rm = TRUE)

org_sh <- org
org_sh$CCL19 <- org$CCL19 - org_CCL19
org_sh$CCL22 <- org$CCL22 - org_CCL22
org_sh$CXCL17 <- org$CXCL17 - org_CXCL17


## Logistic fit

org_fit <- org_sh
org_fit$Group_code <- ifelse(org_fit$Group_code==2,1,org_fit$Group_code)
res <- glm(Group_code~CCL19+CCL22+CXCL17, family = binomial(link = "logit"),org_fit)

probs <- predict(res,type = "response")
fitted.results <- ifelse(probs > 0.5,1,0)

#fitted results
misClasificError <- mean(fitted.results != org_fit$Group_code)
print(paste0("Misclassification Error: ",round(misClasificError,5)))
print(paste0("AIC: ",round(res$aic,5)))
print(auc(fitted.results,org_fit$Group_code))

plot(probs,main='controls_COVID - Org')
lines(org_fit$Group_code,col= 'red')
