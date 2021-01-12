
print("global is working")
hr_data<- read.csv("Human_Resources.csv")
hr_data <- setDT(hr_data)



clustering_and_output <- function(hr_data_input){
  
  clus_data <- select(hr_data_input, 
                      satisfaction_level,
                      last_evaluation,
                      number_project,
                      average_montly_hours,
                      time_spend_company
  )
  
  trans = preProcess(clus_data, 
                     method=c(method = "BoxCox", "center", 
                              "scale", "pca"))
  principal_components = predict(trans, clus_data)
  principal_components <- principal_components[,c("PC1","PC2")]
  return(principal_components)
}

cluster_algo <- function(input, n_clusters,sample){
  clusters <- pam(as.matrix(input), k = n_clusters)
  clustering <- as.data.frame(clusters$clustering)
  names(clustering) <- "clusters"
  comb_data <- cbind(input, clustering)
  output_clusters <- cbind(comb_data, head(hr_data,sample))
  return(output_clusters)    
}

cluster_output <- clustering_and_output(head(hr_data,5000))
f <- cluster_algo(cluster_output,2, 5000)

write.csv(f, "clustering2")



