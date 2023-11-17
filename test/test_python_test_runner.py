from pathlib import Path

import unittest

from cobol_converter.service.python_test_runner import run_subprocess


class TestSubprocessRunner(unittest.TestCase):
    def test_run_subprocess(self):
        cur_file = Path(__file__)
        test_file = cur_file.parent.parent / "sample_python/test_hello.py"
        assert test_file.exists(), f"File {test_file} does not exist."
        run_subprocess(test_file)
        assert (
            cur_file.parent.parent / "sample_python/test_hello.py_test_output.log"
        ).exists()


if __name__ == "__main__":
    unittest.main()

    "WindowsPath('C:/development/playground/agents/cobol_converter/sample_python/test_hello.py')"
