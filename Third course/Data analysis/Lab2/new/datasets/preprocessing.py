import pandas as pd
from datetime import datetime


def calculate_average_time(start: pd.Series, finish: pd.Series) -> list:
    part_dur = []
    for i in range(min(start.size, finish.size)):
        d2 = datetime.strptime(start[i], "%d.%m.%Y %H:%M")
        d1 = datetime.strptime(finish[i], "%d.%m.%Y %H:%M")
        diff = d1 - d2
        part_dur.append(diff.seconds / 60)
    return part_dur
