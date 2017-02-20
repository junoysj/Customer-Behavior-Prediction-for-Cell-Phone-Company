# load in churn.Rda

#1. Split the data into training and test data sets with 66.7% and 33.3% of the
#observations, randomly chosen, respectively
set.seed(3478) # for reproduceable results
train = sample(1:nrow(churn),nrow(churn)*0.667)

#2. Use rpart to build a large complex tree with a low value of cp and minsplit. 
#Treating LEAVE as Negative and STAY as positive, determine the “Big Tree” error rates (FPR and FNR) using the test data set.
churn.train = churn[train,]   
churn.test = churn[-train,] 
library(rpart)
#grow the tree
fit = rpart(stay ~ ., 
            data=churn.train, method="class",
            control=rpart.control(xval=10, minsplit=2, cp=0))
#plot the tree
plot(fit, # the tree to be plotted
     uniform=T, # uniform spacing of nodes
     branch=0.5, # bent branches
     compress=T, # take less space
     main="Big Tree", #title
     margin=0.0) #no extra space
text(fit,  # tree to be embellished
     splits=F, # do not detail split criteria
     all=F, # label only terminal nodes labeled
     use.n=T, # label leaves with observations
     pretty=F, # keep it simple
     cex=0.6) # compress fonts to 60%
#Print the confusion matrix for the test data set.
stay.pred = predict(fit, churn.test, type="class")
stay.actual = churn.test[,"stay"]
confusion.matrix = table(stay.actual, stay.pred)
confusion.matrix
#error rates (FPR and FNR) using the test data set
TN_test <- confusion.matrix[1,1]
TP_test <- confusion.matrix[2,2]
FP_test <- confusion.matrix[1,2]
FN_test <- confusion.matrix[2,1]                      
accuracy_test = (TN_test+TP_test)/(TN_test+TP_test+FP_test+FN_test)
accuracy_test
FPR_test = FP_test/(TN_test+FP_test)
FPR_test
FNR_test = FN_test/(FN_test+TP_test)
FNR_test

#3. Find the best cp value to post-prune the tree. Use the test data set to find the
#“Pruned Tree” error rates. Save a PDF of a nicely formatted plot of the pruned tree.
plotcp(fit, # tree for which to plot
       upper="size")  # plot size of tree (no. of nodes) on top
#find the cp which provides the lowest error
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]   # get 0.002298851
#post pruning
fit.post = prune.rpart(fit, cp=0.002299)
nrow(fit.post$frame)
plot(fit.post, uniform=T, branch=0.5, compress=T,
     main="Tree with Post-Pruning cp = 0.002299 (19 Nodes)", margin=0.05)
text(fit.post,  splits=T, all=F, use.n=T, 
     pretty=T, fancy=F, cex=0.8)
#Use the test data set to find the “Pruned Tree” error rates
stay.pred_post = predict(fit.post, churn.test, type="class")
stay.actual_post = churn.test[,"stay"]
confusion.matrix_post = table(stay.actual_post, stay.pred_post)
confusion.matrix_post
TN_test_post <- confusion.matrix_post[1,1]
TP_test_post <- confusion.matrix_post[2,2]
FP_test_post <- confusion.matrix_post[1,2]
FN_test_post <- confusion.matrix_post[2,1]                      
accuracy_test_post = (TN_test_post+TP_test_post)/(TN_test_post+TP_test_post+FP_test_post+FN_test_post)
accuracy_test_post
FPR_test_post = FP_test_post/(TN_test_post+FP_test_post)
FPR_test_post
FNR_test_post = FN_test_post/(FN_test_post+TP_test_post)
FNR_test_post

#4. Use ROCR to find the best threshold. Using this recommended threshold, determine
#the “Best Threshold Pruned Tree” error rates for the test data set.
library(rpart.plot)
library(ROCR)
#P = 490,000, N=510,000
#profit= 1000*490k + 1000*0.5*510k*(1-alpha)
#offer discount($400) only to N, TN will now stay with a 0.50 probability, FN will stay and also enjoy the discount
#cost = TN+FN = N*TNR + P*FNR = 0.5*510k*(1-alpha)*400 + 490k*beta*400
#expected value with classification = 643000000-153000000 alpha -196000000 beta
stay.pred.train = predict(fit.post, churn.train, type="prob")  #probability prediction
head(stay.pred.train)
# compute the score (the predicted P[YES])
stay.pred.score = prediction(stay.pred.train[,2], churn.train$stay) # the actual class
stay.cost = performance(stay.pred.score, measure="cost", cost.fn=196000000, cost.fp=153000000)
plot(stay.cost)
#find the min
cutoffs = data.frame(cut=stay.cost@"x.values"[[1]], cost=stay.cost@"y.values"[[1]])
best.index = which.min(cutoffs$cost)
cutoffs[best.index,]  #0.4646
#make predictions using this cutoff rate for the test set
stay.pred.test = predict(fit.post, churn.test, type="prob")
head(stay.pred.test)
stay.pred.test.cutoff = 
    ifelse(stay.pred.test[,2] < cutoffs[best.index,]$cut,"LEAVE","STAY")  #if the condition is true, get the 1st value, else 2nd value

confusion.matrix_cutoff = table(churn.test$stay,stay.pred.test.cutoff)
confusion.matrix_cutoff
TN_test_cutoff <- confusion.matrix_cutoff[1,1]
TP_test_cutoff <- confusion.matrix_cutoff[2,2]
FP_test_cutoff <- confusion.matrix_cutoff[1,2]
FN_test_cutoff <- confusion.matrix_cutoff[2,1]                      
accuracy_test_cutoff = (TN_test_cutoff+TP_test_cutoff)/(TN_test_cutoff+TP_test_cutoff+FP_test_cutoff+FN_test_cutoff)
accuracy_test_cutoff
FPR_test_cutoff = FP_test_cutoff/(TN_test_cutoff+FP_test_cutoff)
FPR_test_cutoff
FNR_test_cutoff = FN_test_cutoff/(FN_test_cutoff+TP_test_cutoff)
FNR_test_cutoff

#5. For each of the error rates determined in steps 2, 3 and 4 above, find the expected
#values of strategy (c), for the firm. Display the result in a table with rows for Big
#Tree, Pruned Tree, and Best Threshold Pruned tree; and columns for FPR, FNR,
#Accuracy, Expected Value. The Expected Value column refers to the expected value of strategy (c).
expected_value_big_tree = 643000000-153000000*FPR_test -196000000*FNR_test
expected_value_big_tree
expected_value_post = 643000000-153000000*FPR_test_post -196000000*FNR_test_post
expected_value_post
expected_value_cutoff = 643000000-153000000*FPR_test_cutoff -196000000*FNR_test_cutoff
expected_value_cutoff

results = matrix(signif(c(FPR_test,FPR_test_post,FPR_test_cutoff,FNR_test,FNR_test_post,FNR_test_cutoff,
                          accuracy_test,accuracy_test_post,accuracy_test_cutoff,expected_value_big_tree,expected_value_post,expected_value_cutoff),4),nrow=3)
dimnames(results)= list(c("Big Tree","Pruned Tree", "Best Threshold Pruned Tree"),c("FPR","FNR", "Accuracy", "Expected Value"))
result <- as.table(results)
result




