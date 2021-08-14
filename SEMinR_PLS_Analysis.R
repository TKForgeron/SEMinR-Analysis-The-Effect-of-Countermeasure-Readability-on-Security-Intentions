library(seminr)

# import data
all_indicators <- read.csv("../Data/all_indicators.csv", header = TRUE)

# remove variables that will not be used for this analysis
all_indicators = subset(all_indicators, select=-c(english_level,language,progress,duration,age))
# take reading_level as logical/boolean
all_indicators$crx6 <- as.integer(as.logical(all_indicators$crx6))
# order columns alphabetically
all_indicators = all_indicators[ , order(names(all_indicators))]


# build measurement part of model
# composite construct = formative construct
measurements <- constructs(
  composite("CRD", multi_items("rd", 1:2), weights = mode_B),
  composite("TA", multi_items("ta", 1:4), weights = mode_B),
  composite("CR", multi_items("cr", 1:8), weights = mode_B),
  composite("CA", multi_items("crx", 1:4), weights = mode_B),
  composite("PS", multi_items("ps", 1:4), weights = mode_B),
  composite("PV", multi_items("pv", 1:4), weights = mode_B),
  composite("SE", multi_items("se", 1:8), weights = mode_B),
  composite("RC", multi_items("rc", 1:4), weights = mode_B),
  composite("RE", multi_items("re", 1:4), weights = mode_B),
  composite("SN", multi_items("sn", 1:4), weights = mode_B),
  composite("SI", multi_items("si", 1:4), weights = mode_B)
)

# build structure part of model
structure <- relationships(
  paths(from = "CRD", to = "CR"),
  paths(from = "TA", to = c("PS","PV")),
  paths(from = "CR", to = c("SE","RC")),
  paths(from = "CA", to = c("CR","SE","RC","RE")),
  paths(from = c("PS","PV","SE","RC","RE","SN"), to = "SI")
)

# generate pls model
pls_model <- estimate_pls(data = all_indicators,
                          measurement_model = measurements,
                          structural_model = structure)

model_summary <- summary(pls_model)
plot(pls_model, title="Extended PMT")

# generate bootstrapped model for significance
boot_estimates <- bootstrap_model(pls_model, nboot = 5000, cores = 7)
bootstrapped_model_summary <- summary(boot_estimates)

# plot bootstrapped model
plot(boot_estimates, title = "Extended PMT (Bootstrapped)")
