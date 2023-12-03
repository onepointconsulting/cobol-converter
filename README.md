# Cobol Python Converter

## Description

A simple tool used to convert Cobol into Python and generate unit tests.

## Installation instructions

```
conda create -n cobol_converter python=3.11
conda activate cobol_converter
pip install poetry
poetry install
```
This creates a specific environment with all the libraries you need!


## Configuration
configure the .env file might like this:
To specify configurations use .env file

```
# Open AI / Model related
OPENAI_API_KEY= <openai key>
OPENAI_MODEL = gpt-4
TEMPERATURE = 0

# Autogen
TERMINATE_TOKEN =  TERMINATE
REQUEST_TIMEOUT = 300
SEED = 42
MAX_AUTO_REPLY = 4
CODE_DIR = /tmp/cobol_converter

# Application related
CONVERSION_PYTHON_DIR = /tmp/cobol_converter/python
LLM_CACHE = False
LANGCHAIN_DEBUG = True

# Project related
PROJECT_ROOT = <the root of your project / property_finder>
```

## Running unit tests

```
python -m unittest
```

## Running the main script

Overwrite files in folder
```
python ./cobol_converter/cobol_converter_main.py overwrite
```

Clear all files after creating backup of existing files
```
python ./cobol_converter/cobol_converter_main.py clear
```

Incrementally convert more files
```
python ./cobol_converter/cobol_converter_main.py only_new
```