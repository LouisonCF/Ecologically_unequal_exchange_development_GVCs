# Ecologically unequal exchange and uneven development patterns along global value chains -- Replication kit

This repository contains the R code and material for __Althouse J., Cahen-Fourot L., Carballa-Smichowski B., Durand C., Knauss S. 2023, Ecologically unequal exchange and uneven development patterns along global value chains__, _World Development_, available open-access [__here__](url to be added when paper is online)

This code performs the statistical analysis for the article. It reproduces the principal components analysis and the clustering, exports the files with the PCA and clustering results and generates figures 2 (radar graph) and 3 (3D graph of cluster) in the paper.

---
Make sure you have the dataset _Data_EUE_GVC_index_variables.xlsx_ that you find in the same folder. This file contains the index variables upon which the clustering is performed. We recommend to use the _EUE_GVC_project.rproj_ file to ensure that the code runs in the correct working directory.

Make sure your R version and the packages are up-to-date! This code was tested under R 4.2.2.

---

## Results

The code generates results of the PCA, the clustering, as well as figures of the principal components (not used in the article), the radar graph (figure 2) and the 3D graph (figure 3). Note that the version of the radar graph in the article was generated with excel and not R. The code now generates an R version of the figure.

In addition, the code generates three excel files: the file with the Keyser-Meyer-Ohlin and Bartlett tests results, the file with the PCA results and the file with the clustering results.

## Licences

Database: CC-BY 4.0: https://creativecommons.org/licenses/by/4.0/. This licence applies to the database we provide that contains the index variables that we built upon data from many sources. 

Underlying data to reproduce index variables can be obtained from the authors upon request (note that this replication code does not reproduce the index variables but starts from them). However, the biodiversity data used in the _domestic ecological impact_ and _external balance of ecological degradation_ index variables are from Bjelle, E. L., Kuipers, K., Verones, F., & Wood, R. (2021). Trends in national biodiversity footprints of land use. _Ecological Economics_, 185. https://doi.org/10.1016/j.ecolecon.2021.107059
and were communicated to us by Eivind Lekve Bjelle. Please direct any request for these data to Eivind.

Code: MIT (https://choosealicense.com/licenses/mit/); year: 2023; copyright holders: Jeff Althouse, Louison Cahen-Fourot, Bruno Carballa-Smichowski, Cédric Durand, Steven Knauss.

## Contact
 
The corresponding author is Cédric Durand, cedric.durand _at_ unige.ch. The replication code and folder were prepared by Steven Knauss and Louison Cahen-Fourot. Questions on the code and statistical analysis can be addressed to Louison Cahen-Fourot (lcahenfo _at_ ruc.dk).
