import unittest

from cobol_converter.service.code_extractor import extract_code


class TestCodeExtraction(unittest.TestCase):
    def test_predict(self):
        text = """
Here is the equivalent Python code for the given COBOL program:

```python
# Author: Gil Fernandes
# Date:  2023-11-13
# Purpose: See if Cobol works

def main():
    print("Hello world, I love you!")
    print("This is my first cobol programme")

if __name__ == "__main__":
    main()
```

Now, let's write some unit tests for this Python code. We'll use the `unittest` framework which is included in the Python standard library.

```python
import unittest
from io import StringIO
from unittest.mock import patch

class TestHelloWorld(unittest.TestCase):
    def test_output(self):
        expected_output = "Hello world, I love you!\nThis is my first cobol programme\n"
        with patch('sys.stdout', new=StringIO()) as fake_out:
            main()
            self.assertEqual(fake_out.getvalue(), expected_output)

if __name__ == '__main__':
    unittest.main()
```

To run the unit tests, you would save the Python code and the unit tests in a file (e.g., `hello_world.py`) and then execute the file using the Python interpreter. The `unittest` framework will automatically run the test method `test_output` and check if the output from the `main` function matches the expected output."""
        extracted = extract_code(text)
        self.assertIsNotNone(extracted)
        self.assertEqual(len(extracted), 2)
        # self.assertEqual(p.predict([1, 1, 1, 1]), 4)


if __name__ == "__main__":
    unittest.main()
