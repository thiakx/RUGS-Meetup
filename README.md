MOK-RUGs-Presentation
=====================

R code for my presentation at R User Group Singapore. 

Data is from See-Click-Fix Kaggle Competition: http://www.kaggle.com/c/see-click-predict-fix

Start by running '1. cleanData_map.R', it will generate two javascript maps via the leaflet library into the 'data' folder. 

The generated folders are called 'scf', 'scf_localM'. Within each folder, there will be a .html file that contains the map (Known bug: chrome browsers will need permission for file access for jquery and will not show the map until permission is given. Just use firefox, safari or host on your own server to avoid the hassle)

chrome allow file access - for jquery:
http://stackoverflow.com/questions/4208530/xmlhttprequest-origin-null-is-not-allowed-access-control-access-allow-for-file
