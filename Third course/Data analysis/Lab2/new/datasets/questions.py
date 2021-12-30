import yaml
import pandas as pd

Q1 = None
Q2 = None
Q3 = None
Q4 = None
Q5and10 = None
Q6and12 = None
Q7and9 = None
Q8 = None
Q11 = None
Q13 = None
Q14 = None
Q15 = None
Q16 = None
Q17 = None
Q18 = None
Q19 = None
Q20 = None
Q21 = None
Q22 = None
Q23 = None
Q24 = None
Q25 = None
Q26 = None
Q27 = None
Q28 = None
Q29 = None
Q30 = None
ind_group = None
data = None


def create(path: str):
    with open(path) as f:
        resources = yaml.safe_load(f)

    global Q1, Q2, Q3, Q4, Q5and10, Q6and12, Q7and9, Q8, Q11, Q13, Q14, Q15, Q16, Q17, Q18, Q19, Q20
    global Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28, Q29, Q30, ind_group, data

    data = pd.read_csv(resources['path']['raw']['surveydata'])

    # create sub-dataframes for each question and name them Q...
    # Q1 - Extent of external data usage
    Q1 = data[resources["questions"][1]]

    # Q2 - Company context
    Q2 = data[resources["questions"][2]]

    # Q3 - Out of company context
    Q3 = data[resources["questions"][3]]

    # Q4 - Outside of company but industry-affiliation
    Q4 = data[resources["questions"][4]]

    # Q5&10 - Industry & industry group affiliation
    Q5and10 = data.loc[:, "Accounting":"Writing and Editing"]
    ind_group = data.loc[:, "Art":"Transportation"]

    # Q6&12 - Value chain application area
    Q6and12 = data.loc[:, "Inbound logistics":"Finances & controlling"]

    # Q7&9 - Country
    Q7and9 = data["Country"]

    # Q8 - Company size
    Q8 = data[resources["questions"][8]]

    # Q11 - Job title
    Q11 = data[resources["questions"][11]]

    # Q13 - Use of selection criteria
    Q13 = data[resources["questions"][13]]

    # Q14 - Data selection criteria
    Q14 = data.loc[:, "Machine readibility":"Textfeld"]

    # Q15 - Alternative data selection
    Q15 = data[resources["questions"][15]]

    # Q16 - Knowledge about data structure
    Q16 = data[resources["questions"][16]]

    # Q17 - Data structure
    Q17 = data[resources["questions"][17]]

    # Q18 - Knowledge about file formats
    Q18 = data[resources["questions"][18]]

    # Q19 - Preferred formats
    Q19 = data[resources["questions"][19]]

    # Q20 - Refrained from formats
    Q20 = data[resources["questions"][20]]

    # Q21 - Data providers
    Q21 = data.loc[:, "government agencies (incl. universities)":"Textfeld.1"]

    # Q22 - Data accessibility
    Q22 = data.loc[:, "Public access (open data)":"Textfeld.2"]

    # Q23 - Data types
    Q23 = data.loc[:,
          "personal data (e.g., address, medical details, transactions, communications, etc.)":"Client's information"]

    # Q24 - Statements on external data
    Q24 = data.loc[:,
          "It is possible to derive a practical benefit from Machine Learning models which are trained explicitly on external data.":"Model training on purely external data suits educational purposes."]

    # Q25 - Knowledge about model complexity
    Q25 = data[resources["questions"][25]]

    # Q26 - Model complexity
    Q26 = data[resources["questions"][26]]

    # Q27 - Model building process
    Q27 = data.loc[:, "Non-automated frameworks (e.g. scikit-learn, Keras or TensorFlow in Python)":"Textfeld.3"]

    # Q28 - Learning methods
    Q28 = data[resources["questions"][28]]

    # Q29 - External data sources
    Q29 = data[resources["questions"][29]]

    # Q30 - Dummy question
    Q30 = data[resources["questions"][30]]
