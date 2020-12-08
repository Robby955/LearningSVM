#load(".../ESL.mixture.rda")

# First load the ESL.mixture data from the book website. Then you can extract the useful features for this example.
toyData=data.frame(ESL.mixture$x[,1],ESL.mixture$x[,2],ESL.mixture$y)
colnames(toyData)=c("x1","x2","y")
colors=ifelse(ESL.mixture$y==0,"blue","red")

# Make the initial plot
plot(toyData$x1,toyData$x2,col=colors,xlab="x1",ylab="x2",pch=16)

# Fit the linear model
fitLinear=lm(y~x1+x2,data=toyData)

# Solve using the 0.5 boundary
int=(0.5-coef(fitLinear)[1])/coef(fitLinear)[3]
slope=-coef(fitLinear)[2]/coef(fitLinear)[3]

#Second plot
plot(toyData$x1,toyData$x2,col=colors,xlab="x1",ylab="x2",pch=16)
abline(int,slope)

# Third plot
s=-sqrt((toyData$x1-toyData$x2)^2)/2
plot(exp(s),col=colors,pch=16)

# Load the Kernlab Library
library(kernlab)

# Fit the three Support Vector Machines
model.ksvm1 = ksvm(y~x1+x2,data=toyData, type="C-svc",kernel = 'vanilladot',C=10000)
model.ksvm2 = ksvm(y~x1+x2,data=toyData, type="C-svc",kernel = 'vanilladot',C=0.01)
model.ksvm3 = ksvm(y~x1+x2,data=toyData, type="C-svc",kernel = 'rbfdot',C=1)

# Plot the three Support Vector Machines
plot(model.ksvm1,data=toyData)
plot(model.ksvm2,data=toyData)
plot(model.ksvm3,data=toyData)
