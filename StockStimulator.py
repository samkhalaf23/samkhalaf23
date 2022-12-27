repeat = "yes"


while repeat.lower().strip() == "yes":
    increase = 0 
    decrease = 0
    price = float(input("What is the initial price of stock? "))
    original_price = price
    stimulate_days= float(input("how many days would you like to stimulate? "))
    for x in range(1,stimulate_days+1):
        import random
        price = random.uniform(.98,1.02)*price
        x = random.uniform(.98, 1.02)
        if x > 1:
            increase = increase + 1
        else:
            decrease = decrease + 1
    newprice = price
    print(price) 
    print("The stock price increased", increase, "time(s) and decreased", decrease, "time(s).")
    repeat = input("Would you like to perform another stimulation (yes/no)?")
    import csv, google.colab.files
    with open ("randomwalk.csv", "a") as file:
            writer = csv.writer(file, lineterminator = "\n")

        #header row 
            row = [original_price, stimulate_days,newprice ]
            writer.writerow(row)
