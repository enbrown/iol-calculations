#' Constants for IOL calculations and biometry
#'
Constants <- list(
  IOL = list(A_to_pACD_a0 = -63.896,  # From Holladay conversion 1988
             A_to_pACD_a1 = 0.58357,
             A_to_S_a0 = -65.60,
             A_to_S_a1 = 0.5663,
             pACD_to_S_a0 = -3.595,
             pACD_to_S_a1 = 0.9704,
             A_to_ACD_a0 = -68.747,   # From http://bjo.bmj.com/content/86/6/620.full.pdf for SRK-II A --> SRK/T ACD
             A_to_ACD_a1 = 0.62467),
  Average = list(
    L = 23.5, # http://ascrs12.expoplanner.com/handouts_tn/000025_12247071_Holladay_Advanced_IOL_Calculations.pdf
    L.sd = 1.25,
    K = 43.81,
    K.sd = 1.6,
    ACD = 3.1,
    ACD.sd = 0.30,
    lens.thickness = 4.7,
    lens.thickness.sd = 0.41,
    age = 72,
    age.sd = 12),
  Biometry = list(# http://onlinelibrary.wiley.com/doi/10.1046/j.1442-9071.2003.00617.x/abstract
    us_to_pci = 0.15,
    # http://www.doctor-hill.com/iol-main/advanced_ascan.htm
    us_1532_to_us = 0.32,
    # http://www.doctor-hill.com/iol-main/applanation_ascan.htm
    us_applination_to_us = 0.2,
    # http://www.ucsfcme.com/2011/slides/MOP11002/27ChiuComparingRefractiveResultsBetweenTheIOLMasterAndImmers.pdf
    immersion_us_to_pci_a1 = 0.9951,
    immersion_us_to_pci_a0 = 0.0779,
    # http://www.doctor-hill.com/iol-main/a-scan.htm
    us_immersion_error = 0.12,
    # http://www.augenklinik.uni-wuerzburg.de/ulib/czm/texte/czjalm-e.htm
    us_applination_error = 0.13,
    us_immersion_1532 = 0.13,
    # http://www.augenklinik.uni-wuerzburg.de/ulib/czm/texte/czjalm-e.htm
    pci_iolmaster_error = 0.02,
    # http://www.augenklinik.uni-wuerzburg.de/ulib/czm/texte/kprobl/kprobl.htm
    corneal_index = 1.3375,
    # Holladay 1988
    corneal_index_2 = 4/3,
    aqueous_index = 1.336,
    retinal_thickness = 0.200,
    corneal_principle_plane = 0.05
  ))
