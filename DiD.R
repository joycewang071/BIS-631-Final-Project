#install.packages("did")
library(tidyverse)
library(xtable)
library(did)
out1 <- att_gt(yname="proportion",
               tname="week",
               idname="IP_May_numeric",
               gname="first.treat",
               xformla=~1,
               data=weibo_week)
summary(out1)
ggdid(out1)

out2 <- att_gt(yname="proportion",
               tname="week",
               idname="IP_May_numeric",
               gname="first.treat",
               xformla=~female+verified,
               data=weibo_week)

summary(out2)
ggdid(out2)

agg.simple <- aggte(out2, type = "simple")
summary(agg.simple)

