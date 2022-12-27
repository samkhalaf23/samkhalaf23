 
#Your assignment is to use a combination of two APIs to collect and store realtime earthquake data. The United States Geological Survey (USGS) maintains a public JSON API with information on all earthquakes affecting the United States in the past hour, located at “https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson”. The information for each earthquake is embedded within a JSON array. The information of interest for each earthquake is: 
#Next, use a second API to obtain more information about the location of each earthquake. The OpenCage API provides a free service the converts from latitude and longitude coordinates to county and state. To avoid the API being overloaded with too many requests, OpenCage requires that each user sign up for their own unique “key” for their application. 



!pip install xmltodict
import requests, json, datetime, xmltodict,csv, google.colab.files

response = requests.get("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
apikey = "de713f2c10944e6c9d6d7b249fd9ca47"
#I figured out .format function will add values into the url when I specify them accordingly 
#The county corresponding to the latitude and longitude. The “county” field contains the county. 
#The state corresponding to the latitude and longitude. The “state” field contains the state. 

if response:
    data = json.loads(response.text)
    
    #print(json.dumps(data, indent = 4))
    features = data["features"]
    #print(features)
    #for loop for each one 
    for line in features: 
        mag = line["properties"]["mag"]
        #mag = line["mag"]
        #print("Magnitude level",mag)
        cord = line["geometry"]["coordinates"]
        lat = cord[0]
        lon = cord[1]
        #conversion of time
        orig_time = line["properties"]["time"]
        orig_time_sec = orig_time / 1000 
        datetime_timestamp = datetime.datetime.utcfromtimestamp(orig_time_sec)
        datetime_adj_timestamp = datetime_timestamp - datetime.timedelta(hours = 7)
        time_str = datetime_adj_timestamp.strftime("%B %d, %Y at %I:%M:%S %p")
        #print(lat,lon)
        #with the format function I am able to insert values accordingly into the {} in sequential order. 
        geoUrl = "https://api.opencagedata.com/geocode/v1/xml?q={}+{}&key={}".format(lon,lat, apikey)
        full_url = requests.get(geoUrl)

        if full_url:
            data2 = xmltodict.parse(full_url.text)
    #print(geoUrl)
    #jsonData = json.dumps(data, indent = 4)
    #print(jsonData)
            try:
                
            #locate = county || locateState = State 
                locate = data2["response"]["results"]["result"]["components"]["county"] 
                locateState = data2["response"]["results"]["result"]["components"]["state"]
                finalLocation = locate + "," + locateState
            # by having my variables as "N/A" it will show only in the CSV as "N/A" but in the printed statement it will show as "in the Ocean"    
            except KeyError: 
                finalLocation = "the Ocean"
                locate = "N/A"
                locateState = "N/A"
            except TypeError:
                finalLocation = "the Ocean"
                locate = "N/A"
                locateState = "N/A"
        else: 
            print("Sorry connection error. ")

        print("Magnitude",mag, "earthquake on",time_str, "located at (",cord[1],",", cord[0],") in", finalLocation)
            #print("Magnitude",mag, "earthquake on",time_str, "Latitude",cord[0], "longitude", cord[1])

        
        with open("earthquakes.csv", "a") as file:
            writer = csv.writer(file, lineterminator = "\n")

        
            row = [time_str, mag,cord[1],cord[0],locate,locateState]
            writer.writerow(row)


else: 
    print("Sorry, connection error.")

print("All complete")
google.colab.files.download("earthquakes.csv")
