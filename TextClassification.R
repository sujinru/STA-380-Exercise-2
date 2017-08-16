library(tm)

## create test/train directories to read
train_author_dirs = Sys.glob('~/Downloads/ReutersC50/C50train/*')
file_list = NULL
labels = NULL
for(author in train_author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

test_author_dirs = Sys.glob('~/Downloads/ReutersC50/C50test/*')
for(author in test_author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en')}

all_docs = lapply(file_list, readerPlain)
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

my_corpus = Corpus(VectorSource(all_docs))
names(my_corpus) = file_list

# Preprocessing: removing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), union(stopwords("SMART"), stopwords('en')))

DTM = DocumentTermMatrix(my_corpus)

# Preprocessing: removes those terms that have count 0 in certain percentage of docs
# Need adjustment
DTM = removeSparseTerms(DTM, 0.975)

# Preprocessing: find words with min count
# Need adjustment
X = as.matrix(DTM)
X = X[, findFreqTerms(DTM, 50)]

# Preprocessing: TFIDF
TF <- X / rowSums(X)

EXI_NUM<-apply(X>0, 2, function(x){table(x)['TRUE']})
IDF<-as.numeric(1 + log(nrow(X)/EXI_NUM))

TFIDF = data.frame(t(t(TF)*IDF))
X = TFIDF

# Preprocessing: PCA
pca = prcomp(X, scale=TRUE)
cumsum((pca$sdev)^2) / sum(pca$sdev^2)
plot(cumsum((pca$sdev)^2)/sum(pca$sdev^2), pch=19, cex=0.1 )
X = pca$x[, 1:20] # choose the first 20 components, need adjustment

# Data preparation
train_X = X[1:2500,]
test_X = X[2501:5000,]
clean_labels = NULL
for (i in 1:5000){
  clean_labels = append(clean_labels, strsplit(labels[i], '/')[[1]][3])
}
train_y = clean_labels[1:2500]
test_y = clean_labels[2501:5000]

# Naive Bayes
library(naivebayes)
model <- naive_bayes(x = train_X, y = train_y)
preds <- predict(model, newdata = test_X)

# Random Forest
# Parameters for RF need adjustment
library(randomForest)
model = randomForest(x = train_X, y = as.factor(train_y))
preds <- predict(model ,newdata=test_X)

# Print out accuracy
conf_matrix <- table(preds, test_y)
sum = 0
for (i in 1:50){
  sum = sum +  conf_matrix[i, i]
}
sum/2500

