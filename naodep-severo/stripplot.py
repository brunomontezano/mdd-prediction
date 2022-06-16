import matplotlib.pyplot as plt
import seaborn as sns
sns.set_context("paper")
sns.set_style("whitegrid")

import pandas as pd
df = pd.read_csv("../data/ds_plot_prediction.csv")
print(df)

df['outcome'].value_counts()

def mudar_nome(valor):
    if valor == 'Sem depressão':
        return 'No depression'
    elif valor == 'Leve':
        return 'Mild'
    elif valor == 'Moderada':
        return 'Moderate'
    elif valor == 'Severa':
        return 'Severe'
    else:
        return valor

df['outcome'] = df['outcome'].apply(mudar_nome)

color_theme = ["#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"]

sns.boxplot(x="Yes", y="outcome", palette=color_theme, data=df)
sns.stripplot(x="Yes", y="outcome", linewidth=1, palette=color_theme, data=df)

plt.xlabel("Predictions", size = 18)
plt.ylabel("Severity of depressive episode", size = 18)
plt.yticks(fontsize = 18)
plt.xticks(fontsize = 18)

#plt.tight_layout()
plt.show()
