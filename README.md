# Hidden toll of violent deaths during pregnancy and the postpartum period: a nationwide analysis of Mexican death records

This repository contains replication materials for the article "Hidden toll of violent deaths during pregnancy and the postpartum period: a nationwide analysis of Mexican death records" by Ursula Gazeley, Maria Gargiulo, Hallie Eilerts-Spinelli, Anushé Hassan, Itzel Díaz-Juárez, and Alexis Palfreyman published in _BMJ Public Health_ (DOI: https://doi.org/10.1136/bmjph-2025-004871).

The `code` subdirectory contains all code necessary to replicate all analyses from the article and the supplementary materials. All analyses were conducted using `R` (version 4.4.0) The script `code/000-main.R` specifies the order that analysis scrips should be run in, installs `pacman` which is used for package installation and loading, and will create the `output` directory where all results are written.

In order to run the code, you will first need to download birth and death certificate microdata for the period 1998--2024 from the _Instituto Nacional de Estadística y Geografía_ (INEGI) and place them in the `data` subdirectory. Birth certificate microdata can be downloaded from: https://www.inegi.org.mx/programas/natalidad/#microdatos. Death certificate microdata can be downloaded from: https://www.inegi.org.mx/programas/edr/#microdatos. Note: there is some inconsistent use of capitalization across file names from INEGI, the code assumes that all filenames are uppercase. 

Article citation:

```
@article{gazeley2026hidden,
  title={Hidden toll of violent deaths during pregnancy and the postpartum period: a nationwide analysis of Mexican death records},
  author={Gazeley, Ursula and Gargiulo, Maria and Eilerts-Spinelli, Hallie and Hassan, Anush{\'e} and D{\'\i}az-Ju{\'a}rez, Itzel and Palfreyman, Alexis},
  journal={BMJ Public Health},
  volume={4},
  number={2},
  year={2026},
  publisher={BMJ Publishing Group Ltd}
}
```

<!-- done. -->
