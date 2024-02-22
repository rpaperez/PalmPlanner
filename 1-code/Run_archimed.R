##----------------------------------------------##
# Script to run archimed simulation form opf generation to light simulation ##
#----------------------------------------------##

# Load packages -----------------------------------------------------------

packs <- c('stringr','yaml')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)



# inputs ------------------------------------------------------------------

source('1-code/helpers.R')


#' Run Archimed simulation
#'
#' @param onlyOPF only generate OPF without simulating light (T or F)
#'@param onlyArchimed Lunch archimed without reconstructing opf (opf already exist) (T or F)
#' @param paramFileName name of the Vpalm param file (.txt)
#' @param opsTemplateName name of the ops template (.ops)
#' @param pathVpalmParam path to paramFileName
#' @param pathArchimed path to archimed jar
#' @param pathVpalmJar path to Vpalm Jar
#' @param pathOpf Path where opf is written
#' @param pathTemplateOps path to get ops template
#' @param pathOPS path where ops is written
#' @param run_photosynthesis run phototsynthesis (T or F)
#' @param opfStepExport step of simulation when the opf is exported (numeric 0-28)
#'
#' @return
#' @export
#'
#' @examples
runArchimed=function(onlyOPF=F,onlyArchimed=F,paramFileName="MockUpA_seed1_MAP_72.txt",opsTemplateName='DesignA.ops',pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathTemplateOps=pathTemplateOps,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=14){
  
  #### debug
  # paramFileName=paramFiles[2]
  # opsTemplateName='DesignA.ops'
  # run_photosynthesis=F
  
  # opsTemplateName=paste0(str_sub(string = str_replace(string = paramFileName,pattern = 'MockUp',replacement = 'Design'),start = 1,end = 7),'.ops')
  
  # runArchimed(paramFileName = f,opsTemplateName = opsT)
  
  
  opfname=paste0(str_replace(string = paramFileName,pattern ='.txt',replacement = '.opf'))
  
  # generate opf ------------------------------------------------------------
  
  if(onlyArchimed==F){
    system(command = paste0('java -jar ',pathVpalmJar,' ','/',pathVpalmParam,paramFileName,' ',pathOpf,'/',opfname))
    
  }
  
  
  if (onlyOPF==F){
    # generate ops file -------------------------------------------------------
    
    opsName=paste0(str_remove(string = opsTemplateName,pattern = '.ops'),"_",str_replace(string = opfname,pattern = '.opf',replacement = '.ops'))
    
    read_write_ops(path =paste0(pathTemplateOps,opsTemplateName),opfname = opfname,pathOPS = paste0(pathOPS,opsName))
    
    
    
    # generate config file ----------------------------------------------------
    
    ##"load template
    
    configYml=read_yaml(file =  '0-data/Archimed_inputs/config_template.yml')
    
    ## change names & dirs
    configYml$scene=paste0('scene/',opsName)
    configYml$simulation_directory=str_remove(string = opsName,pattern = '.ops')
    configYml$export_ops=as.character(round(opfStepExport))
    
    
    
    if (run_photosynthesis==T){
      # Absorbed light
      configYml$component_variables$Ra_PAR_0_f=T
      configYml$component_variables$Ra_NIR_0_f=T
      configYml$component_variables$Ra_PAR_0_q=T
      configYml$component_variables$Ra_NIR_0_q=T
      configYml$component_variables$Ra_PAR_f=T
      configYml$component_variables$Ra_NIR_f=T
      configYml$component_variables$Ra_TIR_f=T
      configYml$component_variables$Ra_PAR_q=T
      configYml$component_variables$Ra_NIR_q=T
      configYml$component_variables$Ra_TIR_q=T
      # Assimilation
      configYml$component_variables$An_f=T
      configYml$component_variables$An_q=T
      configYml$component_variables$Gs=T
      # Energy
      configYml$component_variables$H_q=T
      configYml$component_variables$H_f=T
      configYml$component_variables$LE_q=T
      configYml$component_variables$LE_f=T
      configYml$component_variables$Tr_f=T
      configYml$component_variables$Tr_q=T
      configYml$component_variables$T=T
    }
    
    
    pathConfig=paste0('2-outputs/Run_archimed/',str_replace(string = opsName,pattern = '.ops',replacement = '.yml'))
    
    yaml::write_yaml(x =  configYml,file =pathConfig)
    
    debut=Sys.time()
    system(command =paste0('java -jar ',pathArchimed,' ',pathConfig))
    
    print(paste("compute time :",difftime(time1 = Sys.time(),time2 = debut,units = "mins"),'mins'))
    
  }
}

pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_archimed/scene/opf/'
pathTemplateOps='/Users/perez/Documents/TheseEcoBreedAF/proposalPTSmart/SimuThinning/0-data/Ops_template/'
pathOPS='./2-outputs/Run_archimed/scene/'



# run simulations ---------------------------------------------------------

file='Mockup_seed1_MAP_180.txt'

runArchimed(onlyOPF = F,onlyArchimed = T,paramFileName =file,opsTemplateName ='Design90.ops',pathVpalmParam = pathVpalmParam,pathArchimed = pathArchimed, pathVpalmJar = pathVpalmJar,pathOpf = pathOpf,pathTemplateOps=pathTemplateOps,pathOPS = pathOPS,run_photosynthesis=T,opfStepExport=14)
