plotmeans(
  ln_trade ~ year,
  data = df1,
  main = "Heterogeneity Across Time Across Group A Countries",
  xlab = "Year",
  ylab = "Log of Trade",
  col = "steelblue",
  pch = 19,                      # Solid circle
  barcol = "steelblue",          # Color for error bars
  ccol = "black",                # Line color
  mean.labels = FALSE,           # Hide "mean=" labels
  n.label = FALSE,               # Hide "n=" labels
  connect = TRUE                 # Keep line connecting points
)
