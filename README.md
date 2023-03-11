### Description
We examine energy generation patterns and their impact on Locational Marginal Prices (LMP) in the New England region. We use principal component analysis, clustering, and regression to analyze the data. Our findings show that our regression model can provide accurate LMP estimates using less data than traditional computational models. The project offers insights into renewable energy's role in power generation and the importance of efficient LMP estimation.

#### Regression Model: Method
1. Checking multicollinearity: We checked for correlated independent variables in a multiple regression model using the Variance Inflation Factor, to measure how much the variance is inflated. As a rule of thumb, it's an indication of problems if the VIF > 3, and if VIF > 3, the independent variable is dropped.
2. Best subset selection for variable selection: We used Best Subset selection to choose from the remaining variables.
3. We plotted the adjusted r-square, regression sum of squares, Cp and BIC to get a better idea of the marginal gains of adding variables. These different performance metrics were used  to select the correct number of variables. 
4. The F statistic and the ANOVA function were used to identify the significant variables. 
5. K fold validation was used to evaluate our current model's ability when there is new data and when we only have access to a subset of the data.
6. Check for heteroskedasticity: A core assumption that allows us to have confidence and prediction intervals and the significance tests for the coefficients, is that the error terms are normally distributed and that the variance in those distributions are constant across all the values of the independent variables. If this is not true, it impacts the reliability of all our estimates. If the distributions don’t have constant variance, we say there is heteroskedasticity of the residuals. We used the error plot, and the Breusch Pagan Test to check this.
7. We fixed the heteroskedasticity, performed the t test on the adjusted coefficients, and checked the significance of the variables selected to finalize our model. 

##### Team 
[Harshika](https://www.linkedin.com/in/harshika-g-a12553170/)   
[Fridtjof](https://www.linkedin.com/in/fridtjofcs/)    
[Natalie](https://www.linkedin.com/in/ziqing-liang-199838247/)   
[Sarah](https://www.linkedin.com/in/sarah-a-subik/)  
