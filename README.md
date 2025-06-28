# 🗺️ Spatial Analysis of Unemployment in West Java using GWR

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Spatial Analysis](https://img.shields.io/badge/Spatial-Analysis-8A2BE2)
![GWR](https://img.shields.io/badge/GWR-Geographically_Weighted_Regression-blueviolet)

## 📖 Overview
The Open Unemployment Rate (OUR) in West Java, Indonesia's most populous province, varies significantly across its regencies and cities. Traditional global regression models often fail to capture these local nuances. This project employs **Geographically Weighted Regression (GWR)**, an advanced spatial analysis technique, to model the spatially varying relationships between unemployment and its potential drivers. The goal is to provide a more accurate and localized understanding of unemployment dynamics to support effective, region-specific policymaking.

## 📊 Dataset
* **Topic:** Open Unemployment Rate (OUR) in West Java for the year 2024.
* **Source:** Official data from **BPS (Statistics Indonesia) of West Java Province**.
* **Dependent Variable:** `OUR` (Open Unemployment Rate, %).
* **Independent Variables:** `LFPR` (Labor Force Participation Rate), `MSI` (Number of Micro & Small Industries), `DTC` (Distance to Provincial Capital in km), and `PH` (Number of Public Hospitals).

## ⚙️ Methodology & Workflow
The analysis followed a comparative approach to highlight the advantages of a local spatial model over a global one.

1.  **Global Regression (OLS):** A standard Ordinary Least Squares (OLS) multiple linear regression model was first constructed as a baseline. Diagnostic tests on this model revealed the presence of **heteroscedasticity** (p-value < 0.05), indicating that the variance was not constant across space and justifying the need for a spatial model.
2.  **GWR Model Development:** A GWR model was developed to account for spatial heterogeneity. The performance of three different kernel functions was compared:
    * Gaussian
    * **Bisquare**
    * Tricube
3.  **Model Selection:** The GWR model using the **Bisquare kernel** was selected as the best-performing model, as it yielded the lowest AIC value (**78.4253**), indicating a better balance of model fit and parsimony compared to the OLS model (AIC = 87.1886).
4.  **Spatial Analysis & Visualization:** The results from the best GWR model were mapped to visualize how the influence and statistical significance of each predictor variable change across the 27 regencies and cities of West Java.

## 📈 Key Results & Findings

#### GWR is a Superior Model
The GWR model with a Bisquare kernel proved to be a more accurate and effective method for this dataset. It produced a narrower range of residuals (0.029 to 1.876) compared to the global OLS model (0.012 to 1.996), demonstrating its superior ability to capture local variations and reduce prediction errors.

#### Spatial Heterogeneity Confirmed
The core finding of this study is the confirmation of **spatial heterogeneity**. The GWR model revealed that the relationships between variables are not uniform across the province. For example:
* The **Labor Force Participation Rate (LFPR)** had a statistically significant impact on unemployment in urban centers like Bandung and Bekasi, but its effect was non-significant in other regions like Pangandaran.
* This proves that a "one-size-fits-all" policy approach is suboptimal, and region-specific strategies are necessary.

![image](https://github.com/user-attachments/assets/e46de52d-d26b-47ff-8b32-ffdaf2f27876)


