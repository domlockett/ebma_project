# EBMA Project

The Ensemble Bayesian Model Averaging (EBMA) project aims to enhance predictive accuracy in presidential forecasting by integrating multiple models and accounting for their uncertainties. This approach utilizes Bayesian statistics to determine the weights of each model based on their historical performance and prediction accuracy.

### Goals:
1. **Enhance Predictive Accuracy**: The project seeks to improve overall forecast precision through the integration of various models.
2. **Manage Model Uncertainty**: EBMA adjusts for uncertainties present in individual models, resulting in more robust predictions.

### Methods:

1. **Data Preparation**:
   - The script preprocesses data by handling missing values and normalizing features.
   - It splits the data into training and testing sets.

2. **Training Models**:
   - **Lasso Regression**: Utilizes the `glmnet` package for Lasso regression, optimizing hyperparameters through cross-validation.
   - **Elastic Net Regression**: Also implemented using `glmnet`, it combines Lasso and Ridge penalties.
   - **Ridge Regression**: Again, implemented via the `glmnet` package, focusing on regularization to prevent overfitting.
   - **Bayesian Regression**: Uses the `arm` package to fit Bayesian generalized linear models.
   - **Bayesian Additive Regression Trees (BART)**: Implemented using the `BayesTree` package, employing a Bayesian framework for model fitting.
   - **Random Forest**: Utilizes the `randomForest` package, employing hyperparameter tuning for optimal model performance.
   - **Kernel Regression with Least Squares (KRLS)**: Implemented using the `KRLS` package.
   - **FindIt**: Utilizes the `FindIt` package to identify relevant predictors through a systematic search process.

3. **Model Evaluation**:
   - Each model's performance is evaluated using metrics such as accuracy and confusion matrices.
   - Cross-validation is employed to ensure robustness and mitigate overfitting.

4. **Ensemble Averaging**:
   - The EBMA methodology is implemented to calculate model weights based on validation set performance.
   - The weights are iteratively updated to minimize prediction errors, leveraging the strengths of each model.

This comprehensive approach ensures that the EBMA model combines the strengths of individual models while compensating for their weaknesses, resulting in a more robust and accurate prediction system.

### Core Concepts
- **Ensemble Learning**: EBMA employs ensemble learning to combine predictions from multiple models, enhancing overall performance.
- **Bayesian Statistics**: Bayesian techniques calculate the likelihood of different models being correct, allowing for dynamic weight adjustments.
- **Predictive Accuracy**: Focused on improving prediction quality by integrating diverse models.

### Implementation
- **Model Diversity**: Incorporates both parametric and non-parametric models to capture various data aspects.
- **Weight Calculation**: Historical performance data is utilized to compute the likelihood of each model's accuracy.
- **Iterative Refinement**: Model weights are continuously updated based on new data, enhancing prediction accuracy.

### Applications
- **Presidential Forecasting**: Initially applied to forecasting U.S. presidential elections, demonstrating the method's effectiveness in political predictions.
- **Broader Implications**: The approach has potential applications in other fields that require accurate forecasting by combining multiple predictive models.

For further insights, refer to the full article: [2012 Article on EBMA Project](https://github.com/domlockett/ebma_project/blob/main/EBMA_experiments/Presidential%20Forecasting%202020/2012Article.pdf).

### Parametric Models

**Lasso Regression**:
- **Characteristics**: Assumes a linear relationship between dependent and independent variables. Uses penalties to reduce complexity.
- **Application**: Effective for predictive modeling where a linear relationship is assumed.

**Elastic Net Regression**:
- **Characteristics**: Combines Lasso and Ridge penalties, allowing for variable selection and regularization.
- **Application**: Useful in high-dimensional data scenarios where predictors are highly correlated.

**Ridge Regression**:
- **Characteristics**: Similar to Lasso but employs L2 regularization, preventing overfitting in complex models.
- **Application**: Suitable for situations where multicollinearity exists among predictors.

**Bayesian Regression**:
- **Characteristics**: Models relationships using Bayesian inference, providing probabilistic estimates.
- **Application**: Effective in situations requiring uncertainty quantification.

### Non-Parametric Models

**Random Forest (RF)**:
- **Characteristics**: An ensemble method leveraging multiple decision trees without assuming a fixed functional form.
- **Application**: Effective for classification and regression tasks, particularly with large datasets.

**Support Vector Machine (SVM)**:
- **Characteristics**: Classifies data by finding the optimal hyperplane in high-dimensional spaces.
- **Application**: Suitable for complex classification tasks.

**Bayesian Additive Regression Trees (BART)**:
- **Characteristics**: Constructs models sequentially, each addressing errors from the previous model.
- **Application**: Provides flexibility and accuracy in both regression and classification tasks.

**Kernel Regression with Least Squares (KRLS)**:
- **Characteristics**: Non-parametric regression technique that adapts to data through local fitting.
- **Application**: Useful in regression tasks with complex, nonlinear relationships.

**Bayesian Model Averaging (BMA)**:
- **Characteristics**: Averages predictions from multiple models, weighted by their posterior probabilities.
- **Application**: Enhances predictive performance by leveraging the strengths of multiple models.

By categorizing these models into parametric and non-parametric, we highlight their underlying assumptions and suitability for different predictive modeling tasks, aiding in the understanding of the strengths and limitations within the EBMA framework.

For detailed exploration, visit the repository: [EBMA_experiments](https://github.com/domlockett/ebma_project/tree/main/EBMA_experiments/Scripts).

### Detailed Review of `model_training.R` Script

## Acknowledgments

This project represents a collaborative effort involving significant contributions from various scholars. The provided files contain my contributions and are not the original project files. Data has been excluded from this repository to protect privacy. This project was created for the Political Science Department at Washington University in St. Louis.
