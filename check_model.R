
model <- readRDS(
  "D:/Traffic_Accident_ML/results/random_forest/model.rds"
)

cat("MODEL CLASS:\n")
print(class(model))

cat("\nFEATURE NAMES:\n")
print(model$forest$independent.variable.names)

cat("\nCATEGORICAL LEVELS:\n")
str(model$forest$covariate.levels)

cat("\nTARGET:\n")
print(model$dependent.variable.name)

cat("\nTREE COUNT:\n")
print(model$num.trees)
