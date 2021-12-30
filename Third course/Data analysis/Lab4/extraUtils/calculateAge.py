from datetime import date

def calculateAge(born):
    today = date.today()
    return today.year - born.year - ((today.month, today.day) < (born.month, born.day))