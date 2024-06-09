# EBMA Project

The EBMA (Ensemble Bayesian Model Averaging) project aimed to improve predictive accuracy in presidential forecasting by combining multiple models and accounting for their uncertainties. The approach leveraged Bayesian statistics to calculate the weights of each model based on their historical performance and prediction accuracy.


### Goals:
1. **Improve Predictive Accuracy**: By integrating various models, the project seeks to enhance overall forecast precision.
2. **Manage Model Uncertainty**: EBMA adjusts for uncertainties in individual models, providing a more robust prediction.

### Methods:
- **Data Preparation**: Clean and prepare datasets for analysis.
- **Model Training**: Train multiple predictive models, including Random Forest, SVM, GBM, k-NN, Linear Regression, Logistic Regression, and Bayesian Model Averaging.
- **Bayesian Model Averaging**: Combine model predictions by calculating weights based on posterior probabilities, iteratively updating these weights to minimize errors.
- **Evaluation and Visualization**: Assess model performance using metrics like RMSE, MAE, accuracy, and visualize results with plots for better interpretation.

### Core Concepts
- **Ensemble Learning**: EBMA uses ensemble learning to combine multiple models, enhancing predictive performance.
- **Bayesian Statistics**: Bayesian techniques calculate the probability of different models being correct, adjusting weights accordingly.
- **Predictive Accuracy**: Focuses on making accurate predictions by integrating diverse models, each contributing to the final forecast.

### Implementation
- **Model Diversity**: Uses various parametric and non-parametric models to capture different data aspects.
- **Weight Calculation**: Uses historical performance data to compute the likelihood of each model's accuracy.
- **Iterative Refinement**: Continuously updates model weights based on new data to improve prediction accuracy.

### Applications
- **Presidential Forecasting**: Initially applied to forecasting U.S. presidential elections, demonstrating the method's effectiveness in political predictions.
- **Broader Implications**: Potential applications in other fields requiring accurate forecasting by combining multiple predictive models.


For more detailed insights, you can read the full article: [2012 Article on EBMA Project](https://github.com/domlockett/ebma_project/blob/main/EBMA_experiments/Presidential%20Forecasting%202020/2012Article.pdf).

### Parametric Models

**Linear Regression**:
- **Characteristics**: Assumes a linear relationship between dependent and independent variables. Defined by parameters (coefficients) that are estimated from the data.
- **Application**: Predictive modeling where a linear relationship is assumed.

**Logistic Regression**:
- **Characteristics**: Used for binary classification problems. Models the probability of a binary outcome using a logistic function.
- **Application**: Binary classification tasks, like disease diagnosis and spam detection.

### Non-Parametric Models

**Random Forest (RF)**:
- **Characteristics**: An ensemble method that uses multiple decision trees. Non-parametric as it doesn't assume a fixed form for the function that predicts outcomes.
- **Application**: Suitable for classification and regression tasks, especially with large datasets.

**Support Vector Machine (SVM)**:
- **Characteristics**: Classifies data by finding the optimal hyperplane. Non-parametric, particularly in high-dimensional spaces where the number of dimensions can exceed the number of samples.
- **Application**: Effective in classification tasks.

**Gradient Boosting Machines (GBM)**:
- **Characteristics**: Builds models sequentially, each correcting the errors of the previous. Non-parametric as it doesn't assume a fixed underlying model structure.
- **Application**: Used for regression and classification problems requiring high accuracy.

**k-Nearest Neighbors (k-NN)**:
- **Characteristics**: Makes predictions based on the closest training examples in the feature space. Non-parametric since it doesn’t assume any specific parametric form of the data.
- **Application**: Simple and effective for smaller datasets, used in classification tasks.

**Bayesian Model Averaging (BMA)**:
- **Characteristics**: Averages predictions from multiple models weighted by their posterior probabilities. Although it uses Bayesian methods to combine models, it’s considered non-parametric due to the lack of fixed functional form.
- **Application**: Enhances predictive performance by leveraging multiple models and their uncertainties.

By categorizing these models into parametric and non-parametric, we highlight their underlying assumptions and their suitability for different types of predictive modeling tasks. This categorization helps in understanding the strengths and limitations of each model in the context of the Ensemble Bayesian Model Averaging (EBMA) approach used in the project. For more details, explore the repository: [EBMA_experiments](https://github.com/domlockett/ebma_project/tree/main/EBMA_experiments/Scripts).
### Detailed Review of `model_training.R` Script

The `model_training.R` script is responsible for training these seven models:

1. **Data Preparation**:
   - Preprocesses the data, handling missing values and normalizing features.
   - Splits data into training and testing sets.

2. **Training Models**:
   - **Random Forest**: Utilizes the `randomForest` package to train the model, with hyperparameter tuning for the number of trees and maximum depth.
   - **SVM**: Uses the `e1071` package, tuning the cost and gamma parameters.
   - **GBM**: Employs the `gbm` package, adjusting parameters such as the number of trees, interaction depth, and learning rate.
   - **k-NN**: Uses the `class` package, tuning the number of neighbors (k).
   - **Linear Regression**: Implements using the `lm` function, straightforward with minimal hyperparameter tuning.
   - **Logistic Regression**: Uses the `glm` function with a binomial family for binary outcomes.
   - **Bayesian Model Averaging**: Combines predictions from the above models using the EBMA approach to account for uncertainty.

3. **Model Evaluation**:
   - Evaluates each model's performance using metrics such as RMSE, MAE, accuracy, and precision.
   - Uses cross-validation to ensure robustness and prevent overfitting.

4. **Ensemble Averaging**:
   - Implements EBMA by calculating the weights for each model based on their performance on the validation set.
   - Iteratively updates these weights to minimize prediction errors, combining the strengths of each model.

This comprehensive approach ensures that the combined model, created using EBMA, leverages the strengths and compensates for the weaknesses of individual models, providing a more robust and accurate prediction system.

## Acknowledgments

This project is a collaborative effort involving significant contributions from various scholars. The presented files provide a copy of the project containing my contributions and are not original or source project files. Data has been excluded from this repository for privacy purposes. The project was created for the Washington University in Saint Louis Political Science Department. 

