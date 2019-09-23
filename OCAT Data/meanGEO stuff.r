
GROUPS = list(GPU = results$GPU, API = results$API, Location = results$Location)
GROUPS = list(GPU = results$GPU, API = results$API)

FOLD = ""

test = sepCOL(aggregate(results$MsBetweenPresents, list(API = results$API), meanGEO))

test = cbind(test, test$x/max(test$x)*100)
names(test)[ncol(test) - c(1, 0)] = c("Geometric Mean (ms)", "Normalized (%)")
test[ncol(test) - c(1, 0)] = round(test[ncol(test) - c(1, 0)], 2)

writeOCC(test, dataNAME = "GEO - API")

test = sepCOL(aggregate(results$MsBetweenPresents, list(GPU = results$GPU), meanGEO))

test = cbind(test, test$x/max(test$x)*100)
names(test)[ncol(test) - c(1, 0)] = c("Geometric Mean (ms)", "Normalized (%)")
test[ncol(test) - c(1, 0)] = round(test[ncol(test) - c(1, 0)], 2)

writeOCC(test, dataNAME = "GEO - GPU")

test = sepCOL(aggregate(results$MsBetweenPresents, list(Location = results$Location), meanGEO))

test = cbind(test, test$x/max(test$x)*100)
names(test)[ncol(test) - c(1, 0)] = c("Geometric Mean (ms)", "Normalized (%)")
test[ncol(test) - c(1, 0)] = round(test[ncol(test) - c(1, 0)], 2)

writeOCC(test, dataNAME = "GEO - Location")


test = sepCOL(aggregate(results$MsBetweenPresents, list(GPU = results$GPU, API = results$API), meanGEO))

test = cbind(test, test$x/max(test$x)*100)
names(test)[ncol(test) - c(1, 0)] = c("Geometric Mean (ms)", "Normalized (%)")
test[ncol(test) - c(1, 0)] = round(test[ncol(test) - c(1, 0)], 2)

writeOCC(test, dataNAME = "GEO - GPU-API")

sepCOL(aggregate(results$MsBetweenPresents, list(Location = results$Location), meanGEO))


test = sepCOL(aggregate(results$MsBetweenPresents, list(API = results$API), meanGEO))

test = cbind(test, test$x/max(test$x)*100)
names(test)[ncol(test) - c(1, 0)] = c("Geometric Mean (ms)", "Normalized (%)")
test[ncol(test) - c(1, 0)] = round(test[ncol(test) - c(1, 0)], 2)

writeOCC(test, dataNAME = "dataGEO")

test = sepCOL(aggregate(results$MsUntilRenderComplete, list(API = results$API), meanGEO))

test = cbind(test, test$x/max(test$x)*100)
names(test)[ncol(test) - c(1, 0)] = c("Geometric Mean (ms)", "Normalized (%)")
test[ncol(test) - c(1, 0)] = round(test[ncol(test) - c(1, 0)], 2)

writeOCC(test, dataNAME = "RendGEO")


test = sepCOL(aggregate(results$MsEstimatedDriverLag, list(GPU = results$GPU), meanGEO))
test = sepCOL(aggregate(results$MsEstimatedDriverLag, list(API = results$API), meanGEO))
test = sepCOL(aggregate(results$MsEstimatedDriverLag, list(GPU = results$GPU, API = results$API), meanGEO))

test = cbind(test, test$x/max(test$x)*100)
names(test)[ncol(test) - c(1, 0)] = c("Geometric Mean (ms)", "Normalized (%)")
test[ncol(test) - c(1, 0)] = round(test[ncol(test) - c(1, 0)], 2)

writeOCC(test, dataNAME = "DrivGEO")