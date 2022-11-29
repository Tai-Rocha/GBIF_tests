### Freshwater GBFI data
The idea is to get species and occurrences of freshwater from GBIF 
### Workflow of Methods
### 1 Species and occurrence survey downloaded* from [GBIF]()
#### 1.1 Parameter | Query 
- Ocurrences
- Simple 
- Occurrence status:  Present
- Scientific name: Group name. Ex.: Odonata

* Download option: Simple (small file) 


### 2 Vector data built 
- Joining [world rivers](http://ihp-wins.unesco.org/layers/geonode:world_rivers) and  [countri shape](https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-countries/) to identify rivers by country and finally, create a buffer (10 km left, 10 km right) around line (rivers). Final shapefile available [here]() 

### 3 Select records inside the vector cretated (Final shapefile - step 2)  
- Using latitude and longitude 
- predicated : intesertc and are_within 

### 3 Count Species and Ocuurence per Country

