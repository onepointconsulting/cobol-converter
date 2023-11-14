from typing import List


def extract_code(text: str, language: str = "python") -> List[str]:
    code_start = -1
    language_len = len(language)
    text_len = len(text)
    text_list = []
    delimiter_length = 3
    for i in range(text_len):
        if i > text_len - delimiter_length:
            break
        snippet = text[i : i + delimiter_length]
        if "```" == snippet:
            if (
                text[i + delimiter_length : i + delimiter_length + language_len]
                == language
            ):
                code_start = i + delimiter_length + language_len
            else:
                text_list.append(text[code_start:i].strip())
                code_start = -1
    return text_list
