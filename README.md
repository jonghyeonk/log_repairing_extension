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
- PBAR/DiscoverNBG.R : function for fitting a reference model (normal behaviour graph)
- PBAR/PBAR_effi.R : main function of PBAR
- PBAR/algo_recon_effi.R : inner algorithms for reconstructing each anomaly pattern
- PBAR/VotingMatrix.R : function of voting matrix
- PBAR/0.preprocessing_for_sampleddata : to get normal data with different number of traces (represented in Fig.11)
- PBAR/0.preprocessing_for_encoding : main preprecessing step
- PBAR/1.implementation : to apply PBAR to the 11 total logs
- PBAR/1.1.merge_result_alignment : to calculate the alignment result with same format of PBAR
- PBAR/1.2.merge_result_deepalign : to calculate the deepalign result with same format of PBAR
- PBAR/1.3.alpha_test_PBAR : to see results by changing the value of alpha parameter
- PBAR/1.4.PBAR_with_PNUH : to apply PBAR to PNUH data (but, the PNUH data is not sharable)
  
&#x1F53A; Be careful to correctly set your working directory in Rscripts.


## References

[1] Ko, J., & Comuzzi, M. (2022). Pattern-based Reconstruction of Anomalous Traces in Business Process Event Logs. Proceedings of the 1st International Workshop on Computational Intelligence for Process Mining (CI4PM) and the 1st International Workshop on Pervasive Artificial Intelligence (PAI), co-located with the IEEE World Congress on Computational Intelligence (WCCI).

[2] Nolle, T., Luettgen, S., Seeliger, A., & Mühlhäuser, M. (2019). Binet: Multi-perspective business process anomaly classification. Information Systems, 101458.

[3] Ko, J., & Comuzzi, M. (2022). Keeping our rivers clean: Information-theoretic online anomaly detection for streaming business process events. Information Systems, 104, 101894.

[4] Ko, J., Lee, J., & Comuzzi, M. (2020). AIR-BAGEL: An Interactive Root cause-Based Anomaly Generator for Event Logs. In ICPM Doctoral Consortium/Tools (pp. 35-38).


 
