def sample_completed(count):
    if count < 1000:
        return 0
    elif count < 2000:
        return 1
    elif count < 3000:
        return 2
    elif count < 4000:
        return 3
    else:
        return 4


def sample_days_watched(count):
    if count < 30.0:
        return 0
    elif count < 60.0:
        return 1
    elif count < 90.0:
        return 2
    elif count < 180.0:
        return 3
    elif count < 360.0:
        return 4
    else:
        return 5


def sample_mean_score(score):
    if score < 2.0:
        return 0
    elif score < 4.0:
        return 1
    elif score < 6.0:
        return 2
    elif score < 8.0:
        return 3
    else:
        return 4


def sample_age(age):
    if age < 18:
        return 0
    elif age < 25:
        return 1
    elif age < 35:
        return 2
    else:
        return 3
