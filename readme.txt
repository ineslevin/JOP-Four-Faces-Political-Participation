This replication package contains data and R code that can be used to reproduce all tables and figures in "The Four Faces of Political Participation in Argentina: Using Latent Class Analysis To Study Political Behavior" by R. Michael Alvarez, Ines Levin, and Lucas Nuñez.

Package contents:

* datamodel.Rdata: recoded survey data

* script01_stage1.R: estimates 1st stage model (produces “samples_stage1.Rdata”)

* script02_stage2.R: estimates 2nd stage model (produces “samples_stage2.Rdata”)

* script03_classplot.R: reproduces Table 1 and Figures 1-2

* script04_coefplot.R: reproduces Figure 3

* script05_mgeffs.R: reproduces Figures 4-6 and Table C1 

The remaining files can be used to reproduce tables in the Supplementary Materials appendix:

* TurnoutByProvince.csv and TurnoutByPartidoBA.csv: data used to construct Figures A1-2

* script06_turnout_plots.R: reproduces Figures A1-2

* script07_stage2_1000.R: estimates 2nd stage model 1,000 times (produces “samples_stage2_J1000.Rdata”)

* script08_findJ.R: reproduces Figure B1

* script09_stage2_byyear.R: estimates 2nd stage model allowing coefficients to vary by interview year (produces “reffs_samples.Rdata”)

* script10_coefplot_byyear.R: reproduces Figure D1

* script11_mgeffs_byyear.R: reproduces Table D1


