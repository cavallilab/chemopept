library(tidyverse)
library(pROC)


#### SEVERITY ########

## Importing data... org is Moncucco cohort.
org <- read.delim("Moncucco_table.txt",header = TRUE,sep = "\t")
org <- org %>% filter(Group_code<3)
org_c <- org %>% filter(Group_code==0)


## Mild vs severe

# Subtract medians from chemokines CXCL8 CXCL5 CCL25

org_CXCL8 = median(org_c$CXCL8, na.rm = TRUE)
org_CXCL5 = median(org_c$CXCL5, na.rm = TRUE)
org_CCL25 = median(org_c$CCL25, na.rm = TRUE)

org_sh <- org
org_sh$CXCL8 <- org$CXCL8 - org_CXCL8
org_sh$CXCL5 <- org$CXCL5 - org_CXCL5
org_sh$CCL25 <- org$CCL25 - org_CCL25


## Logistic fit

org_fit <- org_sh
org_fit <- org_fit %>% filter(Group_code>0)
org_fit$Group_code <- ifelse(org_fit$Group_code==1,0,org_fit$Group_code)
org_fit$Group_code <- ifelse(org_fit$Group_code==2,1,org_fit$Group_code)
res <- glm(Group_code~CXCL8+CXCL5+CCL25+CCL2, family = binomial(link = "logit"),org_fit)

probs <- predict(res,type = "response")
fitted.results <- ifelse(probs > 0.5,1,0)

#fitted results
misClasificError <- mean(fitted.results != org_fit$Group_code)
print(paste0("Misclassification Error: ",round(misClasificError,5)))
print(paste0("AIC: ",round(res$aic,5)))
print(auc(fitted.results,org_fit$Group_code))

plot(probs,main = 'Severity - Org')
lines(org_fit$Group_code,col= 'red')

