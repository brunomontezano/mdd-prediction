import csv
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
sns.set()
sns.set_style("whitegrid")

predictions = []
with open('data/predictions.csv') as fid:
    reader = csv.reader(fid, delimiter=',', quotechar='|')
    for i, row in enumerate(reader):
        print(row)
        if i == 0:
            continue
        predictions.append((float(row[2]), row[3].replace('"', '')))

    # predictions = []
    # for i, line in enumerate(f):
    #     line = line.split()
    #     if i > 0:
    #         prediction = float(line[2])
    #         label = line[3]
    #         # print(prediction, label)
    #         predictions.append((prediction, label))

s_pred = sorted(predictions, key=lambda tup: tup[0])[::-1]
labels = [tup[1] for tup in s_pred]
labels = np.array(labels)
len(labels)
labels_split = np.array_split(labels, 5)
# print(labels)
print(labels_split)
bars_yes = []
bars_no = []
for split in labels_split:
    # bar_yes = len(split[split=="Yes"])/len(split)
    # bar_no = len(split[split=="No"])/len(split)
    bar_no = len(split[split=="No"])/len(labels[labels=="No"])
    bar_yes = len(split[split=="Yes"])/len(labels[labels=="Yes"])
    bars_yes.append(bar_yes)
    bars_no.append(bar_no)
print(sum(bars_yes))
print(sum(bars_no))
pallete = sns.color_palette("coolwarm", 7)
# plt.bar(np.arange(10), [(a+b)*100 for a,b in zip(bars_yes, bars_no)], color=pallete[0], width=0.5)
plt.bar(np.arange(5), [a*100 for a in bars_yes], color=pallete[6], width=0.5)
print(sum([a*100 for a in bars_yes][0:2]))
# plt.legend(["No", "Yes"], loc='upper center', ncol=2)
#plt.xticks(np.arange(5), np.arange(20)+1)
plt.ylabel("All Recurrent Depressive Episodes (%)")
plt.xlabel("Quintile of Predicted Risk")
#plt.yticks(list(range(0, 120, 10)), list(range(0, 110, 10)))
plt.tight_layout()
plt.grid('off')
ax = plt.axes()
ax.yaxis.grid()
# plt.xaxis.grid()
# plt.show()
plt.savefig('images/risk.png')
plt.savefig('images/risk.eps')
# print(s_pred)
