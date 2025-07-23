###Chamberlain-Mundlak (CRE) Model
pdata$mean_ln_cti <- ave(pdata$ln_cti, pdata$country)
pdata$mean_ln_gdpc <- ave(pdata$ln_gdpc, pdata$country)
pdata$mean_distance <- ave(pdata$distance, pdata$country)
pdata$mean_crises <- ave(pdata$crises, pdata$country)

model_cre <- plm(ln_trade ~ ln_cti + ln_gdpc + distance + crises +
                           mean_ln_cti + mean_ln_gdpc + mean_distance + mean_crises,
                 data = pdata, model = "random")
summary(model_cre)

#Robust SE incase heteroskedasticity and cross correlation assumptions violated
coeftest(model_cre, vcovSCC(fe, type = "HC1"))
