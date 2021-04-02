library(caret)
library(e1071)
rm(list = ls())
#variable
jumlah_fold = 5
#membaca data
main_data = read.csv("data_2_class_seimbang.csv", stringsAsFactors = FALSE)
#membagi data menjadi k group
print("Membuat kelompok")
fold_index = createFolds(main_data$class_label, k=jumlah_fold)
if(exists("predict_result")) {
  rm("predict_result")
}
for(fold_i in 1:jumlah_fold){
  print("---------------------------------")
  print(paste("iterasi ke-",fold_i))
  
  #membuat data training & testing - start
  print(paste("Membuat data testing & training ke-", fold_i))
  test_index = as.numeric(unlist((fold_index[fold_i])))
  
  main_data.testing = main_data[test_index,]
  main_data.training = main_data[-test_index,]
  
  #membuat class_label menjadi kategori
  main_data.training$class_label = as.factor(main_data.training$class_label)
  main_data.testing$class_label = as.factor(main_data.testing$class_label)
  #membuat data training & testing - end
  
  #membuat model klasifikasi
  print(paste("Training model klasifikasi ke-", fold_i))
  model_klasifikasi = naiveBayes(class_label~., main_data.training)
  
  
  #melakukan prediksi
  print(paste("Melakukan prediksi ke-", fold_i))
  prediksi_klasifikasi = predict(model_klasifikasi, main_data.testing[,-5])
  
  #label dari data testing dan prediksi dari setiap iterasi dikumpulkan
  if(!exists("predict_result")){
    assign("predict_result", cbind(as.character(prediksi_klasifikasi),
                                   as.character(main_data.testing$class_label)))
  } else {
    predict_result = rbind.data.frame(predict_result,
                                      cbind(as.character(prediksi_klasifikasi),
                                            as.character(main_data.testing$class_label)))
  }
}
colnames(predict_result) = c("predict", "class_label")
predict_result$predict = as.factor(predict_result$predict)
predict_result$class_label = as.factor(predict_result$class_label)
#menghitung kinerja klasifikasi
#kinerja dihitung dengan menggunakan data label dari data testing dan prediksi
#yang telah dikumpulkan
print("---------------------------------")
print("Menghitung kinerja klasifikasi")
kinerja_klasifikasi = confusionMatrix(predict_result$predict,
                                      predict_result$class_label, positive = "positive")
print(kinerja_klasifikasi)
