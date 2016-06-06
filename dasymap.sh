#!bin/bash
echo 'deb http://ftp.gr.debian.org/debian/ jessie-updates main contrib non-free' > /etc/apt/source.list.d/dasymapr.list
echo 'deb-src http://ftp.gr.debian.org/debian/ jessie-updates non-free contrib main' >>  /etc/apt/source.list.d/dasymapr.list
echo 'deb http://cran.cc.uoc.gr/mirrors/CRAN/bin/linux/debian/ jessie-cran3/ ' >> /etc/apt/source.list.d/dasymapr.list
echo 'deb http://cran.rstudio.com/bin/linux/debian/ jessie-cran3/' >> /etc/apt/source.list.d/dasymapr.list
echo 'deb http://qgis.org/debian-ltr/  jessie main' >> /etc/apt/source.list.d/dasymapr.list
echo 'deb-src http://qgis.org/debian-ltr/  jessie main' >> /etc/apt/source.list.d/dasymapr.list
apt-get update
apt-get upgrade
sudo apt-get install gdal-bin
sudo apt-get install r-base
sudo apt-get install r-base-dev
sudo apt-get install gdebi-core
sudo apt-get build-dep libxml2-dev
apt-get install libcurl4-openssl-dev
wget http://download2.rstudio.org/rstudio-server-0.99.902-amd64.deb
wget http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb
sudo gdebi rstudio-server-0.99.902-amd64.deb
sudo su - \ -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
sudo gdebi shiny-server-1.4.2.786-amd64.deb
sudo su - \ -c "R -e \"install.packages('devtools', dependencies=TRUE)\""
sudo su - \ -c "R -e \"devtools::install_github('etsakl/DasyMapR', dependencies=TRUE,build_vignette=TRUE)\""

## Μερικές πληροφορίες που ενδεχομενως είναι απαραίτητες για την εγκτασταση του πακέτου σε ένα \en AWS server \gr.
## Απαιτείται ενδεχομένως ο ορισμός /swap file για να είναι δυνατή η διμιούργία πακέτων όπως \en dyplr \gr
## Επίσης έιναι πιθανό να αντιμτωπσιτούν προβλήματα με ορισεμένες βιβλιοθηκές του \en rgdal \gr. Απιτείται πρώτα η διμιουτγία του \en gdal \gr
## Επίλυση σε ορίσμένε περίπτωσεις τέοινων προβλημάτων μπορεί να γίνει με την χρη του \en apt-file \gr
    
