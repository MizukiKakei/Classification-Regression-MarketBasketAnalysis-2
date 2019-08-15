# Classification-Regression-MarketBasketAnalysis-2
## Regression for Sales volume 

### Objects and goals
The data of Customer reviwes and sales volume of products were given. 
Our task is making a prediction of sale volume for 25 new products, choosing the most beneficial products and giving the future bussiness plan.

### Data set
There are two different Data sets : Existing products and New Products

The given data sets has the attributes of 
- ProductType
- ProductNum
- Price
- 1 - 5 Star Reviews
- Positive and Negative Service Review
- Recommend product
- BestSellers Rank
- Shipping Weight
- Product Depth, height and width
- ProfitMargin
- And Only Existing Products have sales Volume

### Methods of Analysis
RandomForest, KNN, SVM were used for modeling. In the end, since SVM model obtained the best values of R-Squred, the sales volume of new products were predicted based on SVM model.
