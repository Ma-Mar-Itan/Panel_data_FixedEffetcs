##Reading data
df <- read_excel("C://Users//malek//Desktop//Center of Applied Statistics//Qatar Sanctions//V 1.0//Qatar_v1.0.xlsx", sheet = "final")
View(df)

##Preprocess data & transform to panel
df$year <- as.numeric(df$year)
df$crises <- as.numeric(df$crises)
pdata <- pdata.frame(df, index = c("country", "year"))
View(pdata)

##unobserved heterogeneity
plotmeans(
  ln_trade ~ country,
  data = pdata,
  main = "Heterogeneity Across Countries",
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
  data = pdata,
  main = "Heterogeneity Across Time",
  xlab = "Year",
  ylab = "Log of Trade",
  col = "steelblue",
  pch = 19,                      # Solid circle
  barcol = "steelblue",          # Color for error bars
  ccol = "black",                # Line color
  mean.labels = FALSE,          # Hide "n=" labels
  connect = TRUE                # Keep line connecting points
)

##Desrciptive
vars <- pdata[, c("ln_trade", "ln_cti", "ln_gdpc", "distance", "crises")]
describeBy(vars, group = pdata$country)
ars <- pdata[, c("ln_trade", "ln_cti", "ln_gdpc", "distance", "crises")]
desc_list <- describeBy(vars, group = pdata$country, mat = TRUE)
desc_df <- as.data.frame(desc_list)
desc_df <- desc_df %>%
  rename(
    country = group1,
    variable = vars
  ) %>%
  select(country, variable, n, mean, sd, median, min, max, range, skew, kurtosis, se)

desc_df
write.xlsx(desc_df, "descriptive_stats_by_country.xlsx")

##plots
#CTI
pdata %>%
  filter(!is.na(country)) %>%
  ggplot(aes(x = ln_cti, y = ln_trade, color = country)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Trade vs. CTI",
    subtitle = "Colored by Country",
    x = "Log of CTI ",
    y = "Log of Trade ",
    color = "Country"
  )
#GDPC
pdata %>%
  filter(!is.na(country)) %>%
  ggplot(aes(x = ln_gdpc, y = ln_trade, color = country)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Trade vs. GDPC",
    subtitle = "Colored by Country",
    x = "Log of GDPC ",
    y = "Log of Trade ",
    color = "Country"
  )

#distance
pdata %>%
  filter(!is.na(country)) %>%
  ggplot(aes(x = distance, y = ln_trade, color = country)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Trade vs. GDPC",
    subtitle = "Colored by Country",
    x = "Distance ",
    y = "Log of Trade ",
    color = "Country"
  )
#crises
pdata %>%
  filter(!is.na(country)) %>%
  ggplot(aes(x = crises, y = ln_trade, color = country)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Trade vs. GDPC",
    subtitle = "Colored by Country",
    x = "Crises",
    y = "Log of Trade ",
    color = "Country"
  )

##OLS model
ols <- lm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = pdata)
summary(ols)
##Fixed model 
fe <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = pdata, index = c("country", "year"), model = "within")
summary(fe)
##Random effect
re <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = pdata, index = c("country", "year"), model = "random")
summary(re)


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
              data = pdata, model = "pooling")
vif_model <- lm(ln_trade ~ ln_cti + ln_gdpc + distance + crises, data = pdata)
vif(vif_model)

##Strict Exogeneity 
pdata <- pdata %>%
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
  data = pdata,
  model = "within"
)

summary(fe_exog_test)
