    # Read in all texts
    library(tm)

    ## Loading required package: NLP

    library(naivebayes)
    #train_author_dirs = Sys.glob('~/Downloads/ReutersC50/C50train/*')
    train_author_dirs = Sys.glob('~/Documents/SourceTree/STA380/data//ReutersC50/C50train/*')
    file_list = NULL
    labels = NULL
    for(author in train_author_dirs) {
      author_name = substring(author, first=69)
      files_to_add = Sys.glob(paste0(author, '/*.txt'))
      file_list = append(file_list, files_to_add)
      labels = append(labels, rep(author_name, length(files_to_add)))
    }

    #test_author_dirs = Sys.glob('~/Downloads/ReutersC50/C50test/*')
    test_author_dirs = Sys.glob('~/Documents/SourceTree/STA380/data//ReutersC50/C50test/*')
    for(author in test_author_dirs) {
      author_name = substring(author, first=68)
      files_to_add = Sys.glob(paste0(author, '/*.txt'))
      file_list = append(file_list, files_to_add)
      labels = append(labels, rep(author_name, length(files_to_add)))
    }

    readerPlain = function(fname){
      readPlain(elem=list(content=readLines(fname)), 
                id=fname, language='en')}

    all_docs = lapply(file_list, readerPlain)
    names(all_docs) = file_list

    my_corpus = Corpus(VectorSource(all_docs))

    # Create labels for traing and test set
    #clean_labels = NULL
    #for (i in 1:5000){
    #  clean_labels = append(clean_labels, strsplit(labels[i], '/')[[1]][3])
    #}
    train_y = labels[1:2500]
    test_y = labels[2501:5000]

    #Preprocessing
    my_corpus = tm_map(my_corpus,content_transformer(tolower))
    my_corpus = tm_map(my_corpus,content_transformer(removeNumbers))
    my_corpus = tm_map(my_corpus,content_transformer(removePunctuation))
    my_corpus = tm_map(my_corpus,content_transformer(stripWhitespace))
    my_corpus = tm_map(my_corpus,content_transformer(removeWords),union(stopwords('en'),stopwords('SMART')))

Model 3 RandomForest
====================

The second model selected was RandomForest. In terms of both saving
computational cost and remaining a relatively high accuracy, we choose
the parameter ntree1=100

    library(randomForest)

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    testRF<-function(X){
      X_train = X[1:2500,]
      X_test = X[2501:5000,]
      rfmodel <- randomForest(x=X_train,y=factor(train_y),ntree=100)
      predtest = predict(rfmodel,newdata=X_test)
      conf_matrix = table(predtest,test_y)
      sum = 0
      for(i in 1:dim(conf_matrix)[1]){
        sum = sum + conf_matrix[i,i]
      }
      return (sum/2500)
    }

    #Test Naive Bayes
    library(foreach)
    testNB<-function(X,smoothcount){
      X_train = X[1:2500,]
      X_test = X[2501:5000,]
      
      WP = matrix(0,50,dim(X_test)[2])
      for(i in 1:50){
        wp = colSums(X_train[((i-1)*50):(i*50),] + smoothcount)
        WP[i,] = wp/sum(wp)
      }
      colnames(WP) = colnames(X_train)
      rownames(WP) = unique(train_y)
      author = foreach(i = 1:2500, .combine='c') %do%{
        P = X_test[i,]%*%t(log(WP))
        rownames(WP)[P == max(P)][1]
      }
      conf_matrix = table(author,test_y)
      sum=0
      for(i in 1:dim(conf_matrix)[1]){
         sum = sum + conf_matrix[i,i]
      }
      return(sum/2500)
    }

Similary, we iterated through sparcities, and the highest accuracy
occured when we didn't remove anything.

    DTM = DocumentTermMatrix(my_corpus)
    DTM = removeSparseTerms(DTM,0.995)
    X = as.matrix(DTM)
    print(paste('Accuracy for RandomForest is',testRF(X),'; Accuracy for NaiveBayes is',testNB(X,smoothcount=1/50),sep=' '))

    ## [1] "Accuracy for RandomForest is 0.6324 ; Accuracy for NaiveBayes is 0.6488"

After applying TF-IDF processing, its Accuracy

    #TFIDF
    X = as.matrix(DTM)
    TF = X/rowSums(X)
    EXI_NUM<-apply(X>0, 2, function(x){table(x)['TRUE']})
    IDF<-as.numeric(log(1 + nrow(X)/EXI_NUM))
    TFIDF = data.frame(t(t(TF)*IDF))
    print(paste('Accuracy for RandomForest after TFIDF is',testRF(X),'; Accuracy for NaiveBayes after TFIDF is',testNB(X,smoothcount=1/50),sep=' '))

    ## [1] "Accuracy for RandomForest after TFIDF is 0.6336 ; Accuracy for NaiveBayes after TFIDF is 0.6488"
