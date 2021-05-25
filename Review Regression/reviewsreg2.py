import numpy as np
import pandas as pd
from sklearn.preprocessing import StandardScaler, PolynomialFeatures
import statsmodels.api as sm
from sklearn.pipeline import make_pipeline
import matplotlib.pyplot as plt

data = pd.read_csv("num_reviews.csv", skiprows=1, thousands=',')
data = data.fillna(0)
data = data.drop(data.head(3).index)
data = data.drop(data.tail(1).index)
data = data.reset_index()

X = np.arange(1, len(data["Month of Date"]) + 1).reshape(-1, 1)

transform = make_pipeline(StandardScaler(), PolynomialFeatures(5))
X_trans = transform.fit_transform(X)

X_train = X_trans[:-14]
y_train = data.drop(data.tail(14).index)

states = data.columns[2:]
print(states)

for state in states:
    model = sm.OLS(y_train[state], X_train)
    model = model.fit()
    pred = model.get_prediction(X_trans)
    conf = pred.conf_int(0.01)
    
    plt.fill_between(X.reshape(-1), conf[:,0], conf[:,1], facecolor="green", alpha=0.3, zorder=1)
    plt.errorbar(X, pred.predicted_mean, np.sqrt(pred.var_pred_mean), zorder=2)
    plt.plot(X, data[state], zorder=3)
    plt.title(state)
    plt.ylabel("Review Count")
    plt.legend(["Actual", "99% Confidence Interval", "Predicted"])
    xt = list(range(5, len(data["Month of Date"]) + 1, 12))
    plt.xticks(xt, [data["Month of Date"][i] for i in xt], rotation=30, ha="right")
    plt.grid(axis='x')
    plt.ylim(bottom=-100)
    plt.savefig("{state}.png".format(state=state), bbox_inches="tight")
    plt.show()