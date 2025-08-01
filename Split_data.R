##Reading data
df <- read_excel("C://Users//malek//Desktop//Center of Applied Statistics//Qatar Sanctions//V 1.0//Qatar_project//Qatar_v1.0.xlsx", sheet = "final")
##Preprocess data & transform to panel
df$year <- as.numeric(df$year)
df$crises <- as.numeric(df$crises)
pdata <- pdata.frame(df, index = c("country", "year"))
df

df1 <- pdata %>%
  filter(country %in% c("Saudi", "UAE", "Bahrain", "Egypt"))

df2 <- pdata %>%
  filter(!country %in% c("Saudi", "UAE", "Bahrain", "Egypt"))
View(df1)
View(df2)


##unobserved heterogeneity
##DF1
plotmeans(
  ln_trade ~ country,
  data = df1,
  main = "Heterogeneity Across Group A Countries ",
  xlab = "Country",
  ylab = "Log of Trade",
  col = "steelblue",
  pch = 19,                      # Solid circle
  barcol = "steelblue",          # Color for error bars
  ccol = "black",                # Line color
  mean.labels = FALSE,          # Hide "n=" labels
  connect = TRUE                # Keep line connecting points
)
plotmeans(
  ln_trade ~ year,
  data = df1,
  main = "Heterogeneity Across Time",
  xlab = "Year",
  ylab = "Log of Trade",
  col = "steelblue",
  pch = 19,                      # Solid circle
  barcol = "steelblue",          # Color for error bars
  ccol = "black",                # Line color
  mean.labels = TRUE,          # Hide "n=" labels
  connect = TRUE                # Keep line connecting points
)
#DF2
##unobserved heterogeneity
plotmeans(
  ln_trade ~ country,
  data = df2,
  main = "Heterogeneity Across Group C Countries",
  xlab = "Country",
  ylab = "Log of Trade",
  col = "steelblue",
  pch = 19,                      # Solid circle
  barcol = "steelblue",          # Color for error bars
  ccol = "black",                # Line color
  mean.labels = FALSE,          # Hide "n=" labels
  connect = TRUE                # Keep line connecting points
)
plotmeans(
  ln_trade ~ year,
  data = df2,
  main = "Heterogeneity Across Time Across Group C Countries",
  xlab = "Year",
  ylab = "Log of Trade",
  col = "steelblue",
  pch = 19,                      # Solid circle
  barcol = "steelblue",          # Color for error bars
  ccol = "black",                # Line color
  mean.labels = FALSE,          # Hide "n=" labels
  n.label = FALSE,
  connect = TRUE                # Keep line connecting points
)



##OLS model
ols <- lm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = df1)
summary(ols)
##Fixed model 
fe <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = df2, index = c("country", "year"), model = "within")
summary(fe)
##Random effect
re <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = df1, index = c("country", "year"), model = "random")
summary(re)

#new RE
re <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises,
          data = df1,
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
              data = df1, model = "pooling")
vif_model <- lm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = df2)
vif(vif_model)

##Strict Exogeneity 
df1 <- df1 %>%
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
  data = df1,
  model = "within"
)

summary(fe_exog_test)
