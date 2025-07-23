# 🚗 Linear Regression & Matrix Analysis with R – mtcars Dataset

This project is a comprehensive exploration of **matrix operations**, **statistical analysis**, and **linear regression modeling** in *R* using the classic mtcars dataset.  
It includes everything from basic matrix manipulations to building and optimizing linear models.

---

## 📊 What This Project Covers

- ✅ Matrix transposition and multiplication
- ✅ Matrix invertibility and determinant check
- ✅ Mean calculations with matrix operations
- ✅ Data exploration: max horsepower, correlation matrix, and most influential variable
- ✅ Hypothesis testing (T-test) to evaluate transmission types
- ✅ Multiple linear regression and **stepwise regression (AIC-based)**
- ✅ Data visualization using ggplot2 and corrplot

---

## 🧪 Dataset Used

We use R’s built-in dataset: [mtcars](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html)  
It contains fuel consumption and 10 aspects of automobile design/performance for 32 car models (1974 Motor Trend US magazine).

---

## 🔍 Key Questions Answered

- Is the matrix of selected features invertible?
- Which car has the highest horsepower?
- Which variable is most correlated with others?
- Is *manual* or *automatic* transmission better for MPG?
- What variables best predict *miles per gallon (mpg)*?

---

## 🧰 Technologies Used

- Language: *R*
- Libraries:
  - MASS – for stepAIC (stepwise regression)
  - ggplot2 – for data visualization
  - corrplot – for correlation heatmaps

---

## 📈 Linear Regression Modeling

We build two regression models:
- A manually defined model: mpg ~ wt + hp + qsec
- A *stepwise model* using stepAIC for best predictor selection

### 📌 Output includes:
- Model summaries (summary(lm))
- Variable significance levels
- Model visualization against the most significant predictor

---

## 📊 Visualization Examples

### 🔵 Correlation Matrix

```r
corrplot(correlation_matrix, method = "circle")
```

📉 MPG vs Most Significant Variable
```r
ggplot(mtcars, aes(x = !!significant_variable_sym, y = "mpg")) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

---

▶ How to Run

🛠 Requirements

Ensure you have R and the required packages installed:

```r
install.packages("MASS")
install.packages("ggplot2")
install.packages("corrplot")
```

🚀 Run the Script

You can simply run the R script in your R console or RStudio:

```r
source("linear_regression_mtcars.R")
```

---

📂 Project Structure
```
.
├── linear_regression_mtcars.R   # Main R script
└── README.md                    # This file
```


---

🙋‍♂ Author

Created as a practice project to apply linear regression, matrix algebra, and data visualization in R.
