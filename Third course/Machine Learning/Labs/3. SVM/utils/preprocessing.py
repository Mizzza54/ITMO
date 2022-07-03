from pandas import DataFrame
from sklearn import preprocessing


def minmax_normalization(df: DataFrame):
    scaler = preprocessing.MinMaxScaler()
    names_df = df.columns.drop('class')
    df[names_df] = scaler.fit_transform(df[names_df])


def target_discretization(df: DataFrame):
    mapping = {'P': 1, 'N': -1}
    df['class'].replace(to_replace=mapping, inplace=True)


def preprocessing_initial_data(df: DataFrame):
    minmax_normalization(df)
    target_discretization(df)
