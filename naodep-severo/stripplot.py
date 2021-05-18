import matplotlib.pyplot as plt
import seaborn as sns
sns.set_context("paper")
sns.set_style('whitegrid')

import pandas as pd
df = pd.read_csv('../data/ds_plot_prediction.csv')
print(df)

sns.boxplot(x="Yes", y="outcome", data=df)
sns.stripplot(x='Yes', y='outcome', color=".3", data=df)

plt.xlabel('Predições')
plt.ylabel('Severidade')
plt.tight_layout()
plt.show()
