#1. Read in the CSV file using read.csv(file.choose()) and save it into churn data frame.
churn = read.csv(file.choose())
#2. Examine the structure of the churn data frame. 
str(churn)
#3. If the column types do not match, the use the conversion functions to fix it.

#4. Fix the order of levels of factors to match that in the table on the first page. 
churn$college <- ordered(churn$college, levels = c("zero", "one"))
summary(churn$college)
churn$rep_sat <- ordered(churn$rep_sat, levels = c("very_unsat", "unsat", "avg", "sat", "very_sat"))
summary(churn$rep_sat)
churn$rep_usage <- ordered(churn$rep_usage, levels = c("very_little", "little", "avg", "high", "very_high"))
summary(churn$rep_usage)
churn$rep_change <- ordered(churn$rep_change, levels = c("never_thought", "no", "considering", "perhaps", "actively_looking_into_it"))
summary(churn$rep_change)

#5. Save the data frame as churn.Rda for later reuse.
save(churn, file=file.choose())

#6. Create randomly sampled training and test data sets with about 66.7% and
#   33.3% of the observations, respectively. Use the seed 3478 so that it is
#   repeatable across the groups. 
set.seed(3478)
train = sample(1:nrow(churn),nrow(churn)*0.667)
churn.train = churn[train,]   #  churn.train for building the model
churn.test = churn[-train,]   #  churn.test to test the model

#7. Grow a tree using the training dataset to explain the stay class variable. Use
#   minsplit=100 to keep the tree small for now.
library(rpart)
fit = rpart(stay ~ ., 
            data=churn.train, method="class",
            control=rpart.control(xval=0, minsplit=100), 
            parms=list(split="information"))

#8. Display fit (type fit and hit return).
fit

#9. Explain rows numbered 1, 10, and 3. Which node is the parent node. What was the immediate split that created it? What is the count of stay and leave
#   at this node? (put these as comments in the R file)
# Answer: 
#1) is the root node. It means among the total 13340 data, 6525 of them represent leave and 6815 of them represent stay.
#10) is a leaf node and its parent node is 5). The immediate split is leftover>=24.5. The count of leave is 1221, and the count of stay is 774.
#3) is a splitting node and its parent node is the root. The immediate split is house>=604440.5. The count of stay is 3082, and the count of leave is 1379.

#10. Plot and label the tree. (save the pdf)
plot(fit, uniform=TRUE, branch=0.5, 
     main="Classification Tree for Churn", margin=0.1)
text(fit,  use.n=TRUE, all=TRUE, pretty=T, cex=0.6)


#11. Print the confusion matrix for the test data set.
stay.pred = predict(fit, churn.test, type="class")
stay.actual = churn.test[,"stay"]
confusion.matrix = table(stay.actual, stay.pred)
confusion.matrix

#12. Determine the accuracy, error rates, recall, specificity, and precision for this
#    tree and the test data set
# for this tree:
stay.pred_tree = predict(fit, churn.train, type="class")
stay.actual_tree = churn.train[,"stay"]
confusion.matrix_tree = table(stay.actual_tree, stay.pred_tree)
confusion.matrix_tree

TN_tree <- confusion.matrix_tree[1,1]
TP_tree <- confusion.matrix_tree[2,2]
FP_tree <- confusion.matrix_tree[1,2]
FN_tree <- confusion.matrix_tree[2,1]
accuracy_tree = (TN_tree+TP_tree)/(TN_tree+TP_tree+FP_tree+FN_tree)
accuracy_tree
FPR_tree = FP_tree/(TN_tree+FP_tree)   #type 1 error rate
FPR_tree
FNR_tree = FN_tree/(FN_tree+TP_tree)   #type 2 error rate
FNR_tree
recall_tree = TP_tree/(FN_tree+TP_tree)
recall_tree
specificity_tree= TN_tree/(TN_tree+FP_tree)
specificity_tree
precision_tree = TP_tree/(FP_tree+TP_tree)
precision_tree

# for the test data set:
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
recall_test = TP_test/(FN_test+TP_test)
recall_test
specificity_test= TN_test/(TN_test+FP_test)
specificity_test
precision_test = TP_test/(FP_test+TP_test)
precision_test 

#another way:
accuracy = function(cm){return(sum(diag(cm))/sum(cm))}
error_rates = function(cm){return(1-sum(diag(cm))/sum(cm))}
recall = function(cm){return(cm[1,1]/sum(cm[1,]))}
specificity = function(cm){return(cm[2,2]/sum(cm[2,]))}
precision = function(cm){return(cm[1,1]/sum(cm[,1]))}

accuracy_train = accuracy(confusion.matrix_tree)
accuracy_train
error_rates_train = error_rates(confusion.matrix_tree)
error_rates_train
recall_train = recall(confusion.matrix_tree)
recall_train
specificity_train = specificity(confusion.matrix_tree)
specificity_train
precision_train = precision(confusion.matrix_tree)
precision_train

accuracy_test = accuracy(confusion.matrix)
accuracy_test
error_rates_test = error_rates(confusion.matrix)
error_rates_test
recall_test = recall(confusion.matrix)
recall_test
specificity_test = specificity(confusion.matrix)
specificity_test
precision_test = precision(confusion.matrix)
precision_test
