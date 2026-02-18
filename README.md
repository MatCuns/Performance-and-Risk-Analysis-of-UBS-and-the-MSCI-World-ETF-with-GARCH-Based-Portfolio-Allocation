# Performance-and-Risk-Analysis-of-UBS-and-the-MSCI-World-ETF-with-GARCH-Based-Portfolio-Allocation

An in depth analysis of UBS vs the MSCI World CHF hedged ETF with stylized facts of returns, QQ plots for normality and tail diagnostics, multi method VaR forecasting and violation backtesting, time varying volatility modelling, and VaR based dynamic portfolio allocation in R and LaTeX.

## Overview
This project studies the risk and performance of UBS compared with a global market benchmark, the iShares MSCI World CHF Hedged UCITS ETF (IWDC.SW). The goal is to understand how a single banking stock behaves relative to the overall equity market, and how volatility and downside risk can be measured and managed.

UBS is an interesting case because it is one of the largest European banking groups and it experienced strong price movements during major market episodes such as the COVID crisis and the Credit Suisse takeover in 2023. These events generated strong volatility and make UBS a useful case study for risk modelling.

To keep the comparison consistent, both assets are analysed in Swiss francs. UBS is traded in CHF, and the ETF is CHF hedged, reducing distortions from exchange rate movements.

Daily adjusted closing prices are used from January 2015 to December 2025. Data is retrieved from Yahoo Finance and processed in R. After aligning trading days and computing daily log returns, the analysis compares UBS and the benchmark using performance metrics, stylized facts, Value at Risk models, and time varying volatility methods. Finally, VaR forecasts are used to build a simple risk managed portfolio allocation rule.

## What is included
- Performance and risk measures (Sharpe, Sortino, maximum drawdown, Calmar)
- Stylized facts: low return autocorrelation, volatility clustering, leverage effect, non normality
- Tail diagnostics with density plots and QQ plots
- VaR at 95% with Historical, Gaussian, and Cornish Fisher methods
- Rolling VaR forecasting and violation backtesting
- Time varying volatility modelling (including GARCH based VaR)
- VaR based dynamic portfolio allocation between UBS and IWDC

## Repository contents
This repository includes:
- The compiled PDF report
- The LaTeX source code of the paper
- The full R script used for data processing, analysis, VaR models, and portfolio allocation
- The exported images of all generated charts used in the paper
