# Name: Omar Amr
# Matrikel-Nr: k11776960

library(kernlab)
library(randomForest)
set.seed(123)

directory <- '~/Desktop/ML-Assign-5-EX-7'
setwd(directory)

read_uci_data <- function()
{
  URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
  brCnames <- c("ID",
                "ClumpThickness",
                "UniformityOfCellSize",
                "UniformityOfCellShape",
                "MarginalAdhesion",
                "SingleEpithelialCellSize",
                "BareNuclei",
                "BlandChromatin",
                "NormalNucleoli",
                "Mitoses",
                "Class")
  
  brCdata <- read.table(URL, header=FALSE, sep=",", col.names=brCnames, na.strings="?")
  brCdata$ID <- NULL
  brCdata$Class <- factor(ifelse(brCdata$Class == 4, "malignant", "benign"))
  return(brCdata)
}

read_wine_data <- function()
{
  URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
  wineCnames <- c("Class",
                "Alcohol",
                "MalicAcid",
                "Ash",
                "AlcalinityOfAsh",
                "Magnesium",
                "TotalPhenols",
                "Flavanoids",
                "NonflavanoidPhenols",
                "Proanthocyanins",
                "ColorIntensity",
                "Hue",
                "OD280/OD315OfDilutedWines",
                "Proline")
  
  wineData <- read.table(URL, header=FALSE, sep=",", col.names=wineCnames, na.strings="?", colClasses = c("Class" = "factor"))
  return(wineData)
}

get_alphabet <- function()
{
  result <- vector()
  for(acid in sequence_training_data)
  {
    temp <- unique(unlist(strsplit(acid, "")))
    result <- unique(append(result, temp))
  }
  result <- sort(result)
  return(result)
}

encode_sequence <- function(input_sequence, acid_alphabet)
{
  data_sample_split <- unlist(strsplit(input_sequence, ""))
  data_sample_vect <- vector()
  data_sample_encoding <- replicate(length(acid_alphabet), "0")
  for (char in data_sample_split)
  {
    temp_encoding <- replicate(length(acid_alphabet), "0")
    temp_encoding[match(char, acid_alphabet)] <- 1
    temp_encoding <- unlist(strsplit(temp_encoding, ""))
    data_sample_vect <- append(data_sample_vect, temp_encoding)
  }
  return(as.integer(data_sample_vect))
}

calculate_encoded_matrix <- function(dataset, acid_alphabet)
{
  matrix_size <- length(dataset)
  result <- matrix(nrow=matrix_size, ncol=acid_length*length(acid_alphabet),byrow =TRUE)
  for (data_sample_counter in 1:matrix_size)
  {
    result[data_sample_counter, ] <- encode_sequence(dataset[data_sample_counter], acid_alphabet)
  }
  print("Data encoding done")
  return(result)
}

sequence_training_set <- read.csv('Sequences_train.csv', sep = ',', header = FALSE, stringsAsFactors = FALSE)
sequence_testing_set <- read.csv('Sequences_test_unlabeled.csv', sep = ',', header = FALSE, stringsAsFactors = FALSE)
sequence_training_data <- sequence_training_set[, 1]
sequence_training_labels <- sequence_training_set[, 2]

breast_cancer_dataset <- read_uci_data()
breast_cancer_dataset <- na.omit(breast_cancer_dataset)
breast_cancer_training_data <-breast_cancer_dataset[, -10]
breast_cancer_training_labels <- breast_cancer_dataset[, 10]

wine_dataset <- read_wine_data()
wine_dataset <- wine_dataset[sample(nrow(wine_dataset)), ]
wine_dataset_training_data <-wine_dataset[, -1]
wine_dataset_training_labels <- wine_dataset[, 1]

acid_length <- nchar(sequence_training_data[1])
alphabet <- get_alphabet()
encoding_matrix <- calculate_encoded_matrix(sequence_training_data, alphabet)


rbf_compare_parameters <- function(input_training, input_lables)
{
  min_error <- 100
  c_vector <- c(5, 10, 50, 100,  200)
  sigma_vector <- c(0.001, 0.01, 0.1, 1, 10, 100)
  for (current_c in c_vector) 
  {
    for (current_sigma in sigma_vector) 
    {
      print(paste0("Current C: ", current_c))
      print(paste0("Current Sigma: ", current_sigma))
      start_time = Sys.time()
      model <- ksvm(input_training,input_lables,type="C-svc",C=current_c,kpar=list(sigma=current_sigma), kernel='rbfdot', cross=10)
      end_time = Sys.time()
      print(paste0("Cross Validation Error ", cross(model)))
      print(end_time - start_time)
      print("=============================")
    }
  }
}

randomForest_cross_validation <- function(input_training, input_lables)
{
  error_values <- vector()
  test_size = floor(length(input_lables) / 10)
  for (ntree in c(10, 100, 1000, 10000))
  {
    start_time <- Sys.time()
    error_counter <- 0
    for(counter in seq(1, 10))
    {
      testingStartIndex <- (test_size * (counter - 1)) + 1
      testingEndIndex <- testingStartIndex + test_size - 1
      
      testingSet <- input_training[seq(testingStartIndex, testingEndIndex), ]
      testingLables <- input_lables[seq(testingStartIndex, testingEndIndex) ]
      
      trainingSet <- input_training[-seq(testingStartIndex, testingEndIndex), ]
      trainingLables <- input_lables[-seq(testingStartIndex, testingEndIndex)]
      
      model <<- randomForest(trainingSet, trainingLables, ntree=ntree)
      
      predictions <- predict(model, testingSet)
      number_of_correct_values = which(predictions==testingLables)
      error_counter <- error_counter + (test_size - length(number_of_correct_values))
    }
    end_time <- Sys.time()
    model <- randomForest(input_training, input_lables, ntree=ntree, importance = TRUE)
    varImpPlot(model)
    print(model)
    print(paste0("Cross Validation Error: ", error_counter/(10.0 * test_size)))
    print(end_time - start_time)
    print("=============================")
  }
}


# Compare different paramters:
################
# Wine Dataset #
################
rbf_compare_parameters(as.matrix(wine_dataset_training_data), wine_dataset_training_labels)
randomForest_cross_validation(wine_dataset_training_data, as.factor(wine_dataset_training_labels))


#########################
# Breast Cancer Dataset #
#########################
rbf_compare_parameters(as.matrix(breast_cancer_training_data), breast_cancer_training_labels)
randomForest_cross_validation(breast_cancer_training_data, breast_cancer_training_labels)


#########################
# Acid Sequence Dataset #
#########################
rbf_compare_parameters(encoding_matrix, as.factor(sequence_training_labels))
randomForest_cross_validation(encoding_matrix, as.factor(sequence_training_labels))





##################################################
# Best Models via Selecting The Right Attributes #
##################################################
# Wine Dataset:

rbf_wine_best_trained_model <- ksvm(as.matrix(wine_dataset_training_data), wine_dataset_training_labels,type="C-svc",C=10,kpar=list(sigma=0.01), kernel='rbfdot', cross=10)
randomForest_wine_best_trained_model <- randomForest(wine_dataset_training_data, as.factor(wine_dataset_training_labels), ntree=1000)





# Breaset Cancer Dataset:
rbf_cancer_best_trained_model <- ksvm(as.matrix(breast_cancer_training_data), breast_cancer_training_labels,type="C-svc",C=10,kpar=list(sigma=0.01), kernel='rbfdot', cross=10)
randomForest_cancer_best_trained_model <- randomForest(breast_cancer_training_data, breast_cancer_training_labels, ntree=1000)





# Acid Sequence Dataset:
rbf_sequence_best_trained_model <- ksvm(encoding_matrix,sequence_training_labels,type="C-svc",C=100,kpar=list(sigma=0.001), kernel='rbfdot', cross=10)
randomForest_sequence_best_trained_model <- randomForest(encoding_matrix, as.factor(sequence_training_labels), ntree=10000)
