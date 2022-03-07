########################################
## <~//        Section One       \\~> ##
## <~\\         Overview         //~> ##
########################################

  In the paper "Spatial spread of the West Africa Ebola epidemic" we define 16 models for the spread of Ebola in West Africa.  These models are fit to infection data from between 26 April 2014 and 1 October 2014, and then simulated forward in time to predict future dynamics.  For each of the models we include here two files.  The first is a library file that defines the model in it's entirety, but does not execute anything.  The second is a script file that first collects the appropriate data then fits and simulates the model.  We also include here scripts to reproduce figures from the paper.  

########################################
## <~//        Section Two       \\~> ##
## <~\\     File Descriptions    //~> ##
########################################

You should have received this file as part of a directory "Ebola-DataAndCode" containing multiple files.  

Data - These files store the raw data needed for this project
  AdmUnits_WBtwn.csv
  MobilityDataIDs.rds
  OutbreakDateByCounty_Summer_AllCountries.rds
  WestAfricaCountyPolygons.rds

Simulation output - These directories store the simulation output presented in the figures used in the manuscript. Output is stored as R objects names to correspond to the type of simulation.
  newsims - directory containing hindcast ("init") and forward 	projection ("after) simulations. Each object contains all 	the replicate stochastic simulations for that type.
  location - subdirectory containing the subset of simulations 	that used the best-fit model and started the epidemic in 	each possible location. Results are depicted in manuscript 	Figure 3. 

Libraries - These files define all aspects of the various models, but do not execute any code.
  ebola-alt1-foi.R
  ebola-alt2-foi.R
  ebola-wellmixed.R
  ebola-constant.R
  ebola-ExpNet.R
  ebola-ExpNet-border.R
  ebola-GravNet.R
  ebola-GravNet-border.R
  ebola-GravLong.R
  ebola-MobilityDist.R
  ebola-LongDist.R
  ebola-pwo.R
  ebola-radiation.R

Scripts - These files load data and execute functions to fit models and simulate WNS epidemics.  The second column shows the name of the model run by each script.
  wellmixed.runner.R		-	Well Mixed Model
  constant.runner.R		-	Constant Model
  distmob.runner.R		-	Mobility (Senegal) + Diffusion Model
  exp.runner.R			-	Diffusion Model
  expborder.runner.R		-	Diffusion + Country Borders Model
  expgroupborder.runner.R	-	Diffusion + Core Borders Model
  foi1.runner.R			-	Force of Infection (unnormalized) Model
  foi2.runner.R			-	Force of Infection (normalized) Model
  grav.runner.R			-	Gravity Model
  gravborder.runner.R		-	Gravity + Country Borders Model
  gravgroupborder.runner.R	-	Gravity + Core Borders Models
  gravlong.runner.R		-	Gravity + Long Distance Model
  mobsen.runner.R		-	Mobility (Senegal) Model
  longdist.runner.R		-	Diffusion + Long Distance Model
  pwo.runner.R			-	Population Weighted Opportunity Model
  rad.runner.R			-	Radiation Model

Figure scripts - These files produce the figures included in the manuscript and supplementary material
  CountyGoodnessOfFitPlot.R 				- 	Figures 2, S2 and S4
  Ebola Macroscale fit and correlation tables.R	-	Figure S1 and table S4
  Ebola Forward projection plot.R 			-	Figure 3
  

Other - These files do not fit neatly into another category.
  analysis.R					-	correlation analysis of link weights
  collect.R             			- 	used to organize data for models
  Ebola data West Africa.R 		- 	cleans and organizes external data to produce data files listed above
  Ebola simulation summarization.R	- 	Summarizes simulation output to enable Figure S1
  README.txt

########################################
## <~//       Section Three      \\~> ##
## <~\\     Automated Scripts    //~> ##
########################################

  There are 16 scripts referred to as runners because each one is a step by step recipe for running a model from start to finish.  (in this document "script" and "runner" are used interchangeably)  All the code in this project is written in R.  In addition to the R environment the runner scripts require 6 additional packages available from CRAN: maptools, PBSmapping, lattice, Matrix, mvtnorm, and numDeriv.  In order to run the code provided herein you must have the R environment and these packages properly installed.  Once these dependencies are installed executing a runner script is simple.  To run the Simple Diffusion models start from the R console, navigate to the "Ebola-DataAndCode" directory, and enter the command 

> source("gravgroupborder.runner.R")

  To execute a different script simply change "gravgroupborder.runner.R" to the file name for the appropriate runner.  You can reference Section 2 to see which runner scripts execute which models.  After entering the source() command R will proceed to load required data, fit the appropriate models to that data, and simulate epidemics.  This process may take some time depending on your system specs.  You will know the script has finished running when the R console once again displays the command prompt.  At this point the models and simulations are stored in variables inside the R environment.  By default the runner scripts do not save anything to disk.  The purpose of each line of code is commented in the script.



