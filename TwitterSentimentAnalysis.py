#Your assignment is to create a tool that allows the user to assess the linguistic properties of 
#major companies’ customer service tweets. Your program should first ask the user to specify 
#the type of analysis they would like to perform, and it should then ask the user which company 
#they would like to analyze. Your program should allow the user to perform an unlimited 
#number of analyses.


#𝑓 = 𝑛𝑢𝑚𝑏𝑒𝑟 𝑜𝑓 𝑛𝑜𝑢𝑛𝑠,𝑎𝑑𝑗𝑒𝑐𝑡𝑖𝑣𝑒𝑠,𝑝𝑟𝑒𝑝𝑜𝑠𝑖𝑡𝑖𝑜𝑛𝑠,𝑎𝑛𝑑 𝑑𝑒𝑡𝑒𝑟𝑚𝑖𝑛𝑒𝑟𝑠 
#𝑐 =𝑛𝑢𝑚𝑏𝑒𝑟 𝑜𝑓 𝑝𝑟𝑜𝑛𝑜𝑢𝑛𝑠,𝑣𝑒𝑟𝑏𝑠,𝑎𝑑𝑣𝑒𝑟𝑏𝑠,𝑎𝑛𝑑 𝑖𝑛𝑡𝑒𝑟𝑗𝑒𝑐𝑡𝑖𝑜𝑛𝑠 
 
#Consider the possibility that, when loading the dataset, some connection issue occurs 

#Consider the possibility that the user enters an unsupported mode 

#Ensure that your prompts and output are crisp, professional, and well-formatted. 

#NOTE# mean from numpy was added manually. 

import matplotlib.pyplot as plt, nltk, textblob, requests, json 
nltk.download("punkt")
nltk.download("averaged_perceptron_tagger")
from numpy import mean
response = requests.get("https://dgoldberg.sdsu.edu/515/customer_service_tweets_full.json")
#twitter_input = input("Which Twitter handle would you like to analyze?: ")
#handle_input = input("Which analysis would you like to perform (polarity/subjectivity/formality) ?")
poler_lst = []
subj_lst = []
form_lst = []
f = []
c = []
repeat = "yes"
print("Welcome to the customer service analyzer!")
while repeat.lower().strip() == "yes":
    handle_input = input("Which analysis would you like to perform (polarity/subjectivity/formality)? ")
    twitter_input = input("Which Twitter handle would you like to analyze? ")
    
    if response:
     
        data = json.loads(response.text)
        #print(data)
        for line in data:
            
            if line["Company"] == twitter_input:
                review = line["Text"]
                blob = textblob.TextBlob(review)
                #print("Polarity:", blob.polarity)
                if handle_input.strip().lower() == "polarity":
                    #blob = textblob.TextBlob(review.text)
                    #print("Polarity:", blob.polarity) 
                    poler_lst.append(blob.polarity)
                    avg = sum(poler_lst)/len(poler_lst)
                    #avg = mean(poler_lst)
                    
                if handle_input.lower().strip() == "subjectivity":
                    stext = (blob.subjectivity)
                    subj_lst.append(stext)
                    # = mean(stext)
                    avgsub = sum(subj_lst)/len(subj_lst)
                    #avgsub = mean(subj_lst)
                    #print(avgsub) 
                if handle_input.lower().strip() == "formality":
                    if line["Company"] == twitter_input:
                        #ftext = line["Text"]
                        #fblob = textblob.TextBlob(ftext)
                        for word,tag in blob.tags:
                            if "NN" in tag: 
                                f.append(tag)
                            if "JJ" in tag: 
                                f.append(tag)
                            if "IN" in tag: 
                                f.append(tag)
                            if "DT" in tag: 
                                f.append(tag)
                            if "PR" in tag: 
                                c.append(tag)
                            if "VB" in tag: 
                                c.append(tag)
                            if "RB" in tag: 
                                c.append(tag)
                            if "UH" in tag: 
                                c.append(tag)

                        ftime = len(f)
                        ctime = len(c)
                        form_lin =(50*(((ftime - ctime)/(ftime + ctime))+1))
                    
        

        if handle_input.lower().strip() == "polarity":
            print(twitter_input,":",avg)
        elif handle_input.lower().strip() == "subjectivity":
            print(twitter_input,":",avgsub)
        elif handle_input.lower().strip() == "formality":
            print(twitter_input,":",form_lin) 
        

        elif handle_input.lower().strip() != "polarity":
            print("Sorry, that type of analysis is not supported. Please try again.") 
            
        elif handle_input.lower().strip() != "subjectivity":
            print("Sorry, that type of analysis is not supported. Please try again.")
            
        elif handle_input.lower().strip() != "formality":
            print("Sorry, that type of analysis is not supported. Please try again.")
            

        repeat = input("Would you like to run another analysis?" )
   

             
        
    else: 
        print("connection error")
