##OLS model
ols <- lm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = df2)
summary(ols)
##Fixed model 
fe <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = df2, index = c("country", "year"), model = "within")
summary(fe)
##Random effect
re <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = df2, index = c("country", "year"), model = "random")
summary(re)

#new RE
re <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises,
          data = df2,
          index = c("country", "year"),
          model = "random",
          random.method = "walhus")

###Assumptions 
##Hausman Test (FE vs RE) → Tests for exogeneity of time-invariant regressors 
phtest(fe,re)
#If p-value < 0.05 → reject H₀ → regressors are endogenous → RE is inconsistent → prefer FE.

##heteroskedasticity
bptest(fe)
#If p-value < 0.05 → reject H₀ → heteroskedasticity present

##Serial Correlation 
pwartest(fe)
#If p-value < 0.05 → reject H₀ → autocorrelation is present

##cross correlation
pcdtest(fe, test=("lm")) #cross corrlation

##cross sectional independence Pesaran CD test 
pcdtest(fe, test=("cd"))

#USe robust standard errors
#Driscoll-Kraay SE for t >20
coeftest(fe, vcovSCC(fe, type = "HC1"))

##Multicolinaiorty 
pooled <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises,
              data = df2, model = "pooling")
vif_model <- lm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = df2)
vif(vif_model)

##Strict Exogeneity 
df2 <- df2 %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    lead_ln_cti   = lead(ln_cti),
    lead_ln_gdpc  = lead(ln_gdpc),
    lead_distance = lead(distance),  # may not vary over time — will drop out in FE
    lead_crises   = lead(crises)
  )
fe_exog_test <- plm(
  ln_trade ~ ln_cti + ln_gdpc + crises +
    lead_ln_cti + lead_ln_gdpc + lead_crises,
  data = df2,
  model = "within"
)

summary(fe_exog_test)
