import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt


def analysis_missing_value(df: pd.DataFrame):
    df.info()

    print('\n{}\n'.format('=' * 30))
    print('{:<16} {:>2}'.format('Column', 'Procent of missing'))
    print('{:<16} {:>2}'.format('-' * len('Column'), '-' * len('Procent of missing')))
    for col in df.columns:
        ratioOfMissing = np.mean(df[col].isnull())
        print('{:<16} - {:>2}%'.format(col, round(ratioOfMissing * 100)))

    fig, ax = plt.subplots(1, 2, figsize=(16, 8))

    # определяем цвета
    # желтый - пропущенные данные, темно синий - не пропущенные
    yellowColor = '#ffff00'
    blueColor = '#000080'
    colours = [blueColor, yellowColor]
    sns.heatmap(df.isnull(), cmap=sns.color_palette(colours), ax=ax[0])
    ax[0].set_title('Null value Heatmap', fontdict={'fontsize': 12}, pad=12);

    index = []
    count = []
    for col in df.columns:
        miss = df[col].isnull()
        countMiss = np.sum(miss)
        if countMiss > 0:
            index.append(col)
            count.append(countMiss)
    ax[1].bar(index, count)
    ax[1].set_title('Null value Bar charts', fontdict={'fontsize': 12}, pad=12)

    fig.tight_layout()
