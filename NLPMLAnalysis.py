#Your assignment is to create a tool that trains several machine learning models to perform the 
#task of classifying online reviews. Some of these online reviews refer to hazardous products, so 
#these machine learning models will help to identify the most serious product complaints.

#Your assignment is to create a tool that trains several machine learning models to perform the 
#task of classifying online reviews. Some of these online reviews refer to hazardous products, so 
#these machine learning models will help to identify the most serious product complaints.


import requests, json, textblob, nltk, warnings,sklearn.neighbors, sklearn.neural_network, sklearn.metrics, sklearn.model_selection, sklearn.tree,joblib 
warnings.filterwarnings("ignore")
nltk.download("punkt")
nltk.download("averaged_perceptron_tagger")
response = requests.get("https://dgoldberg.sdsu.edu/515/appliance_reviews.json")

if response: 
    x =[]
    y = []
    data = json.loads(response.text)
    #review = data["Review"]
    for line in data:
        review = line["Review"]
        stars = line["Stars"]
        #print(review)
        blob = textblob.TextBlob(review)
        poler_level = blob.polarity
        subj_level = blob.subjectivity 
        #print(stars) 
        word_r =blob.words
        #word_lst.append(word_r)
        character = (len(word_r))
        #print(character)
        safety = line["Safety hazard"]
        #print(safety)
        inner_list = [poler_level,subj_level,character,stars]
        x.append(inner_list)
        y.append(safety)

    #Lets train the data 
    x_train, x_test, y_train, y_test = sklearn.model_selection.train_test_split(x, y)

    # Build decision tree
    dt_clf = sklearn.tree.DecisionTreeClassifier()
    dt_clf = dt_clf.fit(x_train, y_train)
    dt_predictions = dt_clf.predict(x_test)
    dt_accuracy = sklearn.metrics.accuracy_score(y_test, dt_predictions)
    print("Decision tree accuracy:", dt_accuracy)

    # Build k-nearest neighbors
    knn_clf = sklearn.neighbors.KNeighborsClassifier(35) # k = 35 tried using Within Sum of Squares method to find K. (Used R Studio to find WSS)
    knn_clf = knn_clf.fit(x_train, y_train)
    knn_predictions = knn_clf.predict(x_test)
    knn_accuracy = sklearn.metrics.accuracy_score(y_test, knn_predictions)
    print("K-nearest neighbors accuracy:", knn_accuracy)

    # Build neural network
    nn_clf = sklearn.neural_network.MLPClassifier()
    nn_clf = nn_clf.fit(x_train, y_train)
    nn_predictions = nn_clf.predict(x_test)
    nn_accuracy = sklearn.metrics.accuracy_score(y_test, nn_predictions)
    print("Neural network accuracy:", nn_accuracy)

    #Saving Most accurate model 

    if dt_accuracy > knn_accuracy and dt_accuracy > nn_accuracy:
        print("Decision Tree model performed the best; saved to model.joblib")
        joblib.dump(dt_clf,"model.joblib")
        

    elif knn_accuracy > dt_accuracy and knn_accuracy > nn_accuracy:
        print("K-Nearest Neighbor model performed the best; saved to model.joblib")
        joblib.dump(knn_clf,"model.joblib")
        

    elif nn_accuracy > dt_accuracy and nn_accuracy > knn_accuracy:
        joblib.dump(nn_clf,"model.joblib")
        
        print("Neural Network model performed the best; saved to model.joblib")
else:
    print("Sorry, connection error.")
        
