import collections, re


def count_words(sentence):
    return dict(
        collections.Counter(
            filter(
                None,
                [
                    word.strip(".'\"")
                    for word in re.findall("[A-Za-z0-9']+", sentence.lower())
                ],
            )
        )
    )
