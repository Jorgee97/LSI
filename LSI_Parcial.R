setwd("/home/jorgeam/Documents/parcial")
require("lsa")
Y = matrix(c(3.21, 0, 2.15, 1, 0, 2.21, 8.35, 0, 0, 3.44, 2.15, 0, 0, 
             1.88, 3.25, 0, 2.77, 0, 0, 1.22, 2.18, 0, 4.22, 0, 0, 0, 0,
             0, 2.11, 3.44), nrow = 6, ncol = 5)
rownames(Y) <- c("car", "insurance", "best", "flat", "trip", "vehicule")
colnames(Y) <- c("1", "2", "3", "4", "5")
svdOfY <- svd(Y)
U = svdOfY$u
V = svdOfY$v
VT = t(V)
D = diag(svdOfY$d)

#Ranking the document
k = 2;
Uk = U[,1:k]
Dk = D[1:k,1:k]
Vk = V[,1:k]
VTk = t(Vk)

plot(Vk)

#doing request
reqNum <- 3
arrayOfAP <- array(0, reqNum)
relevanceJudgmentMed <- read.table("./file.REL")
numRelevanceJudg <- dim(relevanceJudgmentMed)[1]
queriesMatrix <- matrix(c(1:6), nrow = reqNum, ncol = k)
for(i in 1:reqNum){
  txtQuery <- readLines(paste("./queries/", toString(i), sep = ""))
  qtext <- ""
  
  for(cq in 1:length(txtQuery))
    qtext <- paste(qtext, txtQuery[cq], sep = " ")

  queryVector <- query(qtext = qtext, termlist = rownames(Y), stemming = TRUE, language = "english")
  q <- t(queryVector) %*% Uk %*% solve(Dk)
  
  queriesMatrix[i,] <- q
  
  ndoc <- ncol(VTk)
  scores <- array(0, ndoc)
  
  for(z in 1:ndoc){
    current <- VTk[1:k, z]
    scores[z] <- cosine(q[1,], current)
  }
  
  sortedScores <- sort.int(scores, decreasing = TRUE, index.return=TRUE)
  topK <- colnames(Y)[sortedScores$ix[1:k]]
  numRelevantRetrievedAt <- array(0, k)
  numRelevant <- 0
  
  for (j in 1:k) {
    cat("query ", i, "\n")
    cat("top ", k, " ", topK[j], "\n")
    for (l in 1:numRelevanceJudg) {
      cat("for query ", relevanceJudgmentMed[l, 1], " is relevant ", toString(relevanceJudgmentMed[l, 3]), "\n")
      if (as.integer(relevanceJudgmentMed[l, 1]) == i && toString(relevanceJudgmentMed[l, 3]) == topK[j]) {
        numRelevant <- numRelevant  + 1
        break
      }
    }
    
    numRelevantRetrievedAt[j] <- numRelevant / j
  }
  arrayOfAP[i] <- mean(numRelevantRetrievedAt)
  write(file="resultFileName.txt", sprintf("AP[%d]: %.4f\n", i, arrayOfAP[i]), append=TRUE)
  print(sprintf("AP[%d]: %.4f\n", i, arrayOfAP[i]))
}

write(file="resultFileName.txt", sprintf("MAP: %.4f\n", mean(arrayOfAP)), append=TRUE)
print(sprintf("MAP: %.4f\n", mean(arrayOfAP)))

plot(queriesMatrix)
