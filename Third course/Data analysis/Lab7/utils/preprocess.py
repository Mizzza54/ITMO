import nltk
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
import re
import string

nltk.download('stopwords')
nltk.download('wordnet')
eng_stopwords = stopwords.words('english')
lemmatizer = WordNetLemmatizer()
banned_list = string.punctuation + 'Ã'+'±'+'ã'+'¼'+'â'+'»'+'§'


def preprocess_text(text: str) -> str:
    text = re.sub(r"(?:\@|https?\://)\S+", "", text)  # remove links and mentions
    text = re.sub(r'[^\x00-\x7f]', r'', text) #remove non utf8/ascii characters such as '\x9a\x91\x97\x9a\x97'
    table = str.maketrans('', '', banned_list)
    text = text.translate(table)
    text = text.lower()
    is_allowed_char = lambda c: (c.isalpha() or c == ' ')
    text = ''.join(list(filter(is_allowed_char, text)))

    is_stopword = lambda word: word not in eng_stopwords
    text = ' '.join(list(filter(is_stopword, text.split())))

    text = ' '.join(list(map(lemmatizer.lemmatize, text.split())))
    return text
