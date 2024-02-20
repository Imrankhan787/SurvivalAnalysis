

*** Cox Proportional Hazard model to evaluate the testable implications of stag-elephant game


*** Import the dataset
****************************
*****************************
stset Year, id(dyad_combine)  failure(Bit_agreement) origin(time atrisk)

*****************************MODEL 1********************************************
********************************************************************************
********************************************************************************
***Simplest survival model contains variables for all hypotheses without any covariates
stcox Cum_bits_byYear is_dyad_in_multilateral learning


****************************MODEL 2*********************************************
********************************************************************************
********************************************************************************
*** Model contains contains variables for all hypotheses with full set of covariates
stcox Cum_bits_byYear is_dyad_in_multilateral learning GDP_constant_host GDP_growth_host FDI_inflow_hostLagOneYear FDI_outflow_home colony common_official_lang shortTermDebt_host comcol  trade_volume Imfcredit_host
 

***************************MODEL 3**********************************************
********************************************************************************
********************************************************************************
stcox Cum_bits_byYear is_dyad_in_multilateral learning GDP_constant_host GDP_growth_host FDI_inflow_hostLagOneYear FDI_outflow_home colony common_official_lang shortTermDebt_host comcol  trade_volume Imfcredit_host dyad_Multi_1yrlag dyad_Multi_2yrlag dyad_Multi_3yrlag dyad_Multi_4yrlag


*********Runing the same model assuming weibull distribution**********************
**********************************************************************************
***********************Model 1 with weibull distribution**************************

streg Cum_bits_byYear is_dyad_in_multilateral learning, distribution(weibull)

**********************************************************************************
***********************Model 2 with weibull distribution**************************
streg Cum_bits_byYear is_dyad_in_multilateral learning GDP_constant_host GDP_growth_host FDI_inflow_hostLagOneYear FDI_outflow_home colony common_official_lang shortTermDebt_host comcol  trade_volume Imfcredit_host, distribution(weibull)


*********************************************************************************
***********************Model 3 with weibull distribution*************************
streg Cum_bits_byYear is_dyad_in_multilateral learning GDP_constant_host GDP_growth_host FDI_inflow_hostLagOneYear FDI_outflow_home colony common_official_lang shortTermDebt_host comcol  trade_volume Imfcredit_host dyad_Multi_1yrlag dyad_Multi_2yrlag dyad_Multi_3yrlag dyad_Multi_4yrlag, distribution(weibull)


*********************************************************************************
*********************************************************************************
****with "net fdi inflows to GDP" and "net fdi outflows to GDP"******************
*********************************************************************************
*************************Model 2*************************************************

stcox Cum_bits_byYear is_dyad_in_multilateral learning GDP_constant_host GDP_growth_host Net_fdiInflow_Gdp_host_lag Net_fdiOutflow_Gdp_home colony common_official_lang shortTermDebt_host comcol  trade_volume Imfcredit_host


*************************Model 3**************************************************
**********************************************************************************

stcox Cum_bits_byYear is_dyad_in_multilateral learning GDP_constant_host GDP_growth_host Net_fdiInflow_Gdp_host_lag Net_fdiOutflow_Gdp_home colony common_official_lang shortTermDebt_host comcol  trade_volume Imfcredit_host dyad_Multi_1yrlag dyad_Multi_2yrlag dyad_Multi_3yrlag dyad_Multi_4yrlag
