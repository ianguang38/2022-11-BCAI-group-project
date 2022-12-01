library(tidyverse)
library(lmerTest)

# glance at the data
rawdt <- read.csv("week2/MNS_data_full.csv")
hist(rawdt$change_view)
# drop na
nona <- rawdt[complete.cases(rawdt),]
features <- nona["change_view"]
dt <- nona %>% mutate(across(c("correctness":"confidence",
                         "clarity":"number_of_article"), scale))
write.csv(dt,"./week2/mns_changeview.csv",row.names = F)

hist(dt$change_view)
table(dt$change_view)
## optional: keep only int value
dt <- dt[!dt$change_view%%1,]
hist(dt$change_view)

null.model <- lmer(change_view ~ (1|article_id)+(1|user_id),data = dt,
                   REML = F)
view.model <- lmer(change_view ~ correctness+likability+usefulness+accuracy+
                     confidence+clarity+(1|article_id)+(1|user_id),data = dt,
                   REML = F)
full.model <- lmer(change_view ~ correctness+likability+usefulness+accuracy+
                     confidence+clarity+Excited+Motivated+Depression+Depression+
                     Anxiety+
                     (1|article_id)+(1|user_id),data = dt,REML = F)
anova(view.model,null.model)
anova(full.model,view.model)
summary(full.model)

## check assumption
a.model = full.model
plot(a.model)
qqnorm(resid(a.model))

# linearity
plot(fitted(a.model),residuals(a.model))
# collinearity
# homoskedasticity
# normality of residuals
qqnorm(resid(a.model))
hist(resid(a.model))

# check result
library(stargazer)
class(a.model) <- "lmerMod"
stargazer(a.model, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")



