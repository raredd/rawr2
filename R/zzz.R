## binding notes
if (getRversion() >= '2.15.1') {
  utils::globalVariables(c(
    ## install.bioc
    'biocLite',
    
    ## mgrep
    'makeCluster', 'detectCores', 'stopCluster',
    'clusterExport', 'parLapply'
  ))
}
