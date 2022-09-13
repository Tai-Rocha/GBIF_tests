##############################################################################################################################################
### Title: Data analysis of Caatinga Cristalino
### Study authors: BRUNO COUTINHO KURTZ,  DIEGO MEIRELES MONTEIRO, MARINEZ F. SIQUEIRA AND TAINÁ ROCHA
### Codes and GitHub pages author: Tainá Rocha 
### We are investigating the phytogeographical patterns and species turnover/nestedness (beta diversity composition) in Caatinga dry tropics
#### In progress. For preliminary results [click here](https://tai-rocha.github.io/Caatinga_Dry_Forest.github.io/)
##############################################################################################################################################

#### Script 
##### I numbering in ascend order both folders and script files in each folder to guide the order of execution.    

##### Provenance (rdlite package) and Log (logr package) was used for reproducibility purposes. It's important to note that rdlite, specifically, ``prov.init()`` function , works per section, and it needs to finished with ``prov.quit()`` function. If you try to run prov.init() more than one time without finish with prov.quit in the same  R section you'll get an error.


#####Here, I adopted one R section per folder, once each folder makes one complete piece of the workflow, i.e, after running all scripts in a folder, R section is quited. Thus for each forder we used ``prov.init()`` and ``prov.quit()`` . 
