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
                "Proline",
                "ID")
  
  wineData <- read.table(URL, header=FALSE, sep=",", col.names=wineCnames, na.strings="?")
  wineData$ID <- NULL
  return(wineData)
}

sequence_training_set <- read.csv('Sequences_train.csv', sep = ',', header = FALSE, stringsAsFactors = FALSE)
sequence_testing_set <- read.csv('Sequences_test_unlabeled.csv', sep = ',', header = FALSE, stringsAsFactors = FALSE)

breast_cancer_dataset <- read_uci_data()
wine_dataset <- read_wine_data()


