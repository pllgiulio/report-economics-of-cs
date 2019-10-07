# Mirai - Economics of Cybersecurity

#### Security metrics

The data about Mirai infections we used for our analysis can be found [here](https://surfdrive.surf.nl/files/index.php/s/0vw5WT2GgorszwR).

The additional data we used for counting IP addressed for each country can be found [here](https://download.ip2location.com/lite/IP2LOCATION-LITE-DB1.CSV.ZIP).

In the folder you also find the script used to create the graphs. Before running it, set as **Working directory** the directory containing the downloaded dataset. The Mirai one has to be called `360data.csv` while the one with the IPs has to be `ip.csv`.

#### Security investments

This folder contains the scripts used to compute the infection rate per ISP and the costs reported in the report.

The rate has been computed using the dataset about Mirai infections used also for the security metrics ([here](https://surfdrive.surf.nl/files/index.php/s/0vw5WT2GgorszwR)) and the dataset that maps IPs to the ASN ([here](https://iptoasn.com/)).
