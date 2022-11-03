# CyanoVirLasoo

These are networks of the associations between cyanobacteria and their phage-coded host genes in global metagenomes. 
It complements the following manuscript, which is in prepration as of this writing, but likely in review or print by
the time anyone else looks at it. The broad approach is to run a series of lasso regressions to predict the abundance of
each host gene from the cyanobacterial ecotype abundances and a bunch of environemntal parameters. Then we only display the
interactions between the viral host-genes and cyanobacteiral ecotypes.
We do two models, one looking only at positive interactions, and one including both positive and negative interactions.

Associations between picocyanobacterial ecotypes and cyanophage host genes across ocean basins

Clara A. Fuchsman, David Garcia-Prieto, Matthew D. Hays, Jacob A. Cram

Planned submission in November 2022

The analysis can be replicated by running 
`NextVirusNetworkScript.R`

The main scripts calls 
`Bring_in_Data.R`
which loads in data
and also

`Jacob_Lasso_Lib.R` which includes the code for the functions that do the work.

Directories inclue
`data` -- I had several iterations of data with extra stuff added. See Bring-in-Data.R to see which are actually used
`archive` includes old scripts that I generated as I learned to carry out this process
`figures` are the version of the figures that went into the paper
`renv` contains information about libraries needed to run this project. For space reasons I'm `.gitignore`-ing the actual library files.

Libraries can likely be recovered by running renv::restore(), or probably just running R inside of the working directory.
