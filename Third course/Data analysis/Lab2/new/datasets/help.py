import numpy
from matplotlib import pyplot as plt


def func(pct: numpy.float64, allvals: list):
    absolute = int(pct / 100. * numpy.sum(allvals))
    return "{:.1f}%".format(pct, absolute)


def draw(index, labels, sizes, title, colors):
    plt.figure(index)
    wedges, texts, autotexts = plt.pie(sizes, autopct=lambda pct: func(pct, sizes),
                                       colors=colors,
                                       startangle=90, counterclock=False)
    plt.legend(wedges, labels,
               title="Answer options",
               loc="center left",
               bbox_to_anchor=(1, 0, 0.5, 1))
    plt.setp(autotexts, size=12)
    plt.title(title)


def show():
    plt.show()
