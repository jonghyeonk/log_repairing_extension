# Anomaly Reconstruction

This repository shows an extended version of the Pattern-based Anomaly Reconstruction (PBAR) method [1].


## Prepared Data1 - 5 artificial logs
We used 5 types of process models including small, medium, large, huge, and wide refered from [2] to generate artificial logs.

## Prepared Data2 - 2 artificial logs
We used two process models (credit-card & Pub) refered from [3] to generate artificial logs.

## Prepared Data3 - 4 real-life logs
For the real life logs, we consider the Hospital Billing event log containing events about the billing of medical services that have been obtained from the financial modules of the ERP system of a regional hospital, and the Road Traffic event log which collects events about a road traffic fine management process at a local police authority in Italy.

For all logs, we injected 5 types of anomaly patterns including "insert", "skip", "moved", "replace", and "rework" introduced in [4]. The statistics of datasets are summarised in Table 1 in our paper.


## R-files
- util/vTree.R : function for training a reference model (directly followed graph)
- util/vForest.R : function for calculating anomaly matrix and reconstructing anomalies
- preprocesssing.R : data preprocessing before implementing anomaly reconstruction 
- implementation.R : Implementation of vTree and vForest (= Pattern-based Anomaly Reconstruction)


&#x1F53A; Be careful to correctly set your working directory in Rscripts.


## References

[1] Ko, J., & Comuzzi, M. (2022). Pattern-based Reconstruction of Anomalous Traces in Business Process Event Logs. Proceedings of the 1st International Workshop on Computational Intelligence for Process Mining (CI4PM) and the 1st International Workshop on Pervasive Artificial Intelligence (PAI), co-located with the IEEE World Congress on Computational Intelligence (WCCI).

[2] Nolle, T., Luettgen, S., Seeliger, A., & Mühlhäuser, M. (2019). Binet: Multi-perspective business process anomaly classification. Information Systems, 101458.

[3] Ko, J., & Comuzzi, M. (2022). Keeping our rivers clean: Information-theoretic online anomaly detection for streaming business process events. Information Systems, 104, 101894.

[4] Ko, J., Lee, J., & Comuzzi, M. (2020). AIR-BAGEL: An Interactive Root cause-Based Anomaly Generator for Event Logs. In ICPM Doctoral Consortium/Tools (pp. 35-38).


 
