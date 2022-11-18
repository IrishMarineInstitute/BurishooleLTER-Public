#First set your working directory to the lake folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
##change this depending  on where youare on your computer 
setwd("..\\feeagh_2020 - sean")

library(GOTMr)
library(gotmtools)

#View 'gotm.yaml' file  #Hold 'Ctrl' and left-click 'gotm.yaml'
gotm_yaml <- 'gotm.yaml'

#Check GOTM version
run_gotm(args = c("-v"), verbose = TRUE)
# Should get the same input as below...
# ------------------------------------------------------------------------
#   GOTM:    5.4.0 (unknown branch)
# YAML:    0.1.0 (unknown branch)
# flexout: 0.1.0 (unknown branch)
# STIM:     (unknown branch)
# FABM:    unknown (unknown branch)
# WET:  unknown (unknown branch)
# NetCDF: 3.6.1-beta1 of Mar  7 2018 17:17:53 $
#   ------------------------------------------------------------------------
#   Compiler: Intel 19.0.0.20181018



#Run GOTM
run_gotm(yaml_file = gotm_yaml, verbose = T)
# system('run_gotm.bat')  #Use this command if the GOTMr package
# isn't installed

##Check output in the console to make sure that GOTM has ran successfully
##if you get a 0, its worked

# ...
# ------------------------------------------------------------------------
#   GOTM finished on 2019/09/27 at 14:23:12
# ------------------------------------------------------------------------
#   CPU time:                       2.152814      seconds
# Simulated time/CPU time:        13404595.6534740     
# ------------------------------------------------------------------------
#   GOTM:    5.4.0 (unknown branch)
# YAML:    0.1.0 (unknown branch)
# flexout: 0.1.0 (unknown branch)
# STIM:     (unknown branch)
# FABM:    unknown (unknown branch)
# NetCDF: 3.6.1-beta1 of Mar  7 2018 17:17:53 $
#   ------------------------------------------------------------------------
#   Compiler: Intel 19.0.0.20181018

#Run run_gotm
system2("run_gotm.bat")


##If you get the above output, SUCCESS!
##You can now run GOTM in R



#################################################################
############# Editing parameters ################################

# Before we start editing the the 'gotm.yaml' we are going to make a
# master copy
file.copy(from = 'gotm.yaml', to = 'gotm_master.yaml', overwrite = FALSE)
# [1] TRUE  



start <- '2019-10-25 00:00:00'
stop <- '2021-01-31 00:00:00'


input_yaml(file = gotm_yaml, label = 'time', key = 'start', value = start)
input_yaml(file = gotm_yaml, label = 'time', key = 'stop', value = stop)
input_yaml(file = gotm_yaml, label = 'location', key = 'name', value = 'feeagh')
input_yaml(file = gotm_yaml, label = 'roughness', key = 'charnock', value = 'false')

# Create initial temperature file for simulation
init_prof(obs_file = 'wtemp_dly_oct_2019.dat', date = start, tprof_file = 'init_tprof.dat')


library(gotmtools)

gotm_yaml <- "gotm.yaml"

#######################################################################


##If you want to save the text from the GOTM run
gotm_out <- run_gotm(yaml_file = gotm_yaml, verbose = T)
# gotm_out <- system2("run_gotm.bat", stdout = T)
gotm_out
write.table(gotm_out, file='got_out.txt', row.names = F, quote = F, col.names = F)


out <- 'output.nc' # output file

list_vars(ncdf = out, long = T) # View long names of variables in netCDF
list_vars(ncdf = out, long = F) #View short names of variables in netCDF, corresponds to positions of long names

lnams <- list_vars(ncdf = out, long = T) # Store long names
index <- which(lnams == "potential temperature") # Store index of where temp is
snam <- list_vars(ncdf = out, long = F)[index] # Extracts short name
snam

mod_wtemp <- get_vari(ncdf = out, var = snam) # Extract potential temp
head(mod_wtemp) # View first 6 rows

z <- get_vari(out, 'z') # Extract depths corresponding to temp
head(z)

p1 <- plot_wtemp(out, size = 2) # Plot water temperature, adjust 'size' to fill empty space
p1 <- p1 + xlab('') + ylab('Depth (m)') + ggtitle('Feeagh - Modelled Heatmap') # Add axis labels and title
windows()
p1 # Prints plot to screen


write.table(mod_wtemp, file = "modelled_2020.wtr",row.names = T, col.names = TRUE, sep = '\t', quote = F)

