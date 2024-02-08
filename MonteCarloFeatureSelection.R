library(rmcfs)

str(Project4)

Project4[1:15, c(1:15, dim(Project4)[2])]



# perform monte carlo feature selection (random reducts) using default paramters
result <- mcfs(ethnicity ~ ., Project4, projections = 1500, projectionSize = 0.1, splits = 5, splitSetSize = 500, cutoffPermutations = 6, threadsNumber = 8)

## will prob need to do again with more projections

head(result$RI)

#difference between the projections on X axis vs Y axis
plot(result, type = "distances")

result2 <- result$RI[1:result$cutoff_value,]

gid <- build.idgraph(result, size = 20)
plot.idgraph(gid, label_dist = 0.3)
