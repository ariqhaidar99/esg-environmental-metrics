***Scotland Carbon Footprint Trend Analysis (1998–2021)***

**Summary**
This project provides an in-depth exploratory data analysis and statistical evaluation of Scotland’s greenhouse gas emissions, indexed by carbon source. By leveraging historical data, this study evaluates the efficacy of current decarbonization trends against Scotland’s legislative target to achieve Net Zero by 2045.

**Project Architecture**
The analysis is conducted using Base R to understand data manipulation, statistical modeling, and graphical representation without reliance on external packages.
* Data Cleaning & Pre-processing: Implemented chronological sorting and handling of missing values to ensure the integrity of time-series visualizations.  
* Time Series Forecasting: Applied linear regression models to project domestic household emission trajectories through 2045.  
* Data Transformation: Utilized log and square-root transformations to stabilize heteroscedasticity, satisfying the assumptions of ANOVA models.  
* Visualization: Developed custom plotting functions to index emissions to a 1998 baseline, allowing for the direct comparison of disparate emission sources.

**Methodology Highlights**
* Statistical Validation: ANOVA testing, supported by Tukey HSD post-hoc analysis, confirmed a statistically significant variance between all emission sources (p < 0.05), validating the structural differences in carbon intensities between domestic and imported sectors.
* Residual analysis (Residuals vs. Fitted) was utilized to identify and rectify skewed data distributions, ensuring the robustness of our inferential results.

Key Insights: 
* The Net Zero Gap: Linear forecasting indicates that at historical rates of decay, direct household emissions will plateau at ~10 Mt CO2e by 2045, failing to meet the Net Zero mandate. This suggests a requirement for aggressive, structural policy intervention rather than incremental change.
* Economic Resilience vs. Vulnerability: Overlaid macro-economic analysis demonstrates that emissions tied to commercial supply chains (Imports and UK-Produced Goods) are highly sensitive to global shocks like the 2008 Financial Crisis and COVID-19, whereas domestic household emissions remain resilient to these shocks.
* Relative Decarbonization: Indexing reveals that while total volumes fluctuate, UK-produced goods have experienced the steepest relative decline in carbon intensity since 1998 compared to both imported and domestic household sources.
