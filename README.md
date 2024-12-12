# vignette-networks
Vignette on analyzing network data in R; created as a class project for PSTAT197A in Fall 2024.


Contributors:

- Ivy Li
- Yamileth Martinez
- Jade O'Brien

Abstract: 

In this vignette, we will explore basic network analysis using the `igraph` package on the `congress_network` dataset. Network analysis helps us explore and understand the structure of a network; in this case, the Twitter interactions of the 117th Congress. Our objective in this vignette is to give a basic overview of how to use the `igraph` package for data visualization, and to analyze whether particular attributes such as party affiliation, chamber, and the geographical region of a member of Congress influences who they interact with on Twitter. We will additionally cover centrality metrics, graph metrics, bootstrap hypothesis testing, and adjacency matrices.

Repository Contents:

The vignette itself can be found in the main directory in a file titled vignette.html. 

The img folder contains all of the adjacency matrix images created in the vignette.

The scripts folder contains a script titled vignette-script.R which contains all of the code used in the vignette, a preprocessing.R script used for preprocessing, as well as an archived folder consisting of archived scripts. 

The data folder contains an RData file of the results of boostrap hypothesis testing, a csv file of node attributes, and a folder called `congress_network` which contains all of the resources and data downloaded [here](https://snap.stanford.edu/data/congress-twitter.html) from Stanford's SNAP website.


References: 
- [Paper corresponding to dataset](https://pmc.ncbi.nlm.nih.gov/articles/PMC10493874/#sec0003)

- [Network Visualization](https://kateto.net/netscix2016.html)

- [igraph documentation](https://igraph.org/r/doc/)
