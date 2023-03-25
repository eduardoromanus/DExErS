# DExErS

This repository contains the code and files needed to reproduce the experiments reported in the paper *Empirical probabilistic forecasting: An approach solely based on deterministic explanatory variables for the selection of past forecast errors* (https://doi.org/10.1016/j.ijforecast.2023.01.003). The method proposed in this paper was nicknamed DExErS (short for **D**eterministic **Ex**planatory variables **Er**ror **S**election).

## System requirements

The experiments were executed on a VM with 8 vCPUs (at 2.35 GHz to 2.45 GHz base frequency), 64 GiB RAM, 256 GiB SSD, and Ubuntu 18.04 LTS OS. Therefore, we recommend using a system with similar requirements (especially the amount of RAM).

## Base files

It may take considerable time to generate the base point forecasts needed to replicate the probabilistic forecasts by DExErS. With this in mind, we have made the point forecasts used in our experiments (as well as other important data) available in the following public folder: https://1drv.ms/u/s!Aqc9ixNFtFWAiv4UrLtqtd1QUkMHSA?e=exWalJ.

## Point forecasts

The code to reproduce the base point forecasts is also available in this GitHub repository, but keep in mind that the algorithm employed to generate these forecasts is not fully deterministic, so small variations may be seen in the results.
