# Algorithm prediction

#answers = rep("A", 20)

pml_write_files = function(x){
     n = length(x)
     for(i in 1:n){
          filename = paste0("./Project_Prediction_Answers/problem_id_",i,".txt")
          write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
     }
}

testPred = predict(modFit, testingRed2)
