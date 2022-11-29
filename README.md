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
Joining [world rivers](http://ihp-wins.unesco.org/layers/geonode:world_rivers) and  [country shape](https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-countries/) to identify rivers by country and finally, create a buffer (10 km left, 10 km right) around line (rivers). Final shapefile available [here](https://github.com/Tai-Rocha/GBIF_tests/tree/main/data/shape) 

### 3 Select records inside the vector cretated (Final shapefile - step 2)  
#### 3.1 records are decimalLatitudes and decimalLongitudes from GBIF data 
#### 3.3 predicate: intesertc and are within 

### 4 Counting per contry
#### 4.1 Species per Country
#### 4.2 Occurrences per country

