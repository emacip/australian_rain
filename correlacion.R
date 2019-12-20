tmp <- cor(train)
train_matrix[upper.tri(train_matrix)] <- 0
diag(train_matrix) <- 0
train_num.new <- train_num[,!apply(train_matrix,2,function(x) any(x > 0.8))]
head(train_num.new)

