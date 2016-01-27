pml_write_files = function(x) {
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,col.names=FALSE)
  }
}
x <- evaluation_data
y <- x[feature_set[feature_set!='classe']]
answers <- predict(rf,newdata=x)
answers
pml_write_files(answers)