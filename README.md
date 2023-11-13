## Description

A simple tool used to convert Cobol into Python and generate unit tests.

## Installation instructions


```
conda create -n cobol_converter python=3.11
conda activate cobol_converter
pip install poetry
poetry install
```
This creates a specific environment with all the libraries in need!



## to start the chatbot
``````




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
