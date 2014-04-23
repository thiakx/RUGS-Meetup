RUGS-Meetup
=====================

R code for my presentation at R User Group Singapore. 

Please download the train & test data into the /data folder. Get them from See-Click-Fix Kaggle Competition: http://www.kaggle.com/c/see-click-predict-fix

Start by running the code in their sequence 0,1,2a,2b...take note that step 3 is in a folder "3.testMod_generateEnsembleRatio" where we will be running models on a subset of training data to obtain the ensemble ratio for step 4 ensemble

'1. cleanData_map.R', will generate two javascript maps via the leaflet library into the 'leafletMaps' folder. The generated folders are called 'scf' & 'scfLocalM'. Within each folder, there will be a .html file that contains the map. The multi layered version used during my demo is in the scfMap_kxDemoVer folder. (Known bug: chrome browsers will need permission for file access for jquery and will not show the map until permission is given. Just use firefox, safari or host on your own server to avoid the hassle)

To chrome allow file access for jquery:
http://stackoverflow.com/questions/4208530/xmlhttprequest-origin-null-is-not-allowed-access-control-access-allow-for-file
