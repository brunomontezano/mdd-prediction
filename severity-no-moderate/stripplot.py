import matplotlib.pyplot as plt
import seaborn as sns
sns.set_context("paper")
sns.set_style('whitegrid')

import pandas as pd
df = pd.read_csv('predictions-depression-severity.csv')
print(df)

sns.boxplot(x="Yes", y="outcome", data=df)
sns.stripplot(x='Yes', y='outcome', color=".3", data=df)

plt.xlabel('Predictions')
plt.ylabel('Severity')
plt.tight_layout()
plt.show()
