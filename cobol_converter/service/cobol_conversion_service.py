from pathlib import Path

from typing import List

from pylint.lint.run import Run

from cobol_converter.service.agent_setup import (
    user_proxy,
    python_test_agent,
    conversion_manager,
)
from cobol_converter.service.code_extractor import extract_code
from cobol_converter.config import cfg


def cobol_conversion(cobol_files: List[Path]):
    assert cobol_files is not None
    for cobol_file in cobol_files:
        convert_single_file(cobol_file)


def lint_code(file: Path):
    parent = file.parent
    file_name = file.name
    output_file = parent/f"{file_name}_lint.txt"
    Run([f"--output={output_file}", file.as_posix()], exit=False)


def convert_single_file(cobol_file: Path):
    user_proxy.initiate_chat(
        conversion_manager,
        message=f"""Please convert the Cobol code to Python code and write unit tests for it. 
The Cobol code can be found between the delimiters === CODE START === and === CODE END ===. 
Make sure that the code is formatted using markdown syntax.

=== CODE START ===
{cobol_file.read_text()}
=== CODE END ===

""",
    )
    python_test_message = python_test_agent.last_message()
    if "content" in python_test_message:
        content = python_test_message["content"]
        conversion_python_dir = cfg.conversion_python_dir
        code_blocks = extract_code(content)
        code_blocks_len = len(code_blocks)
        python_file: Path = conversion_python_dir / f"{cobol_file.stem}.py"
        if code_blocks_len > 0:
            # Assume main code
            python_file.write_text(code_blocks[0])
            lint_code(python_file)
        if code_blocks_len > 1:
            # Assume tests
            python_file: Path = conversion_python_dir / f"test_{cobol_file.stem}.py"
            python_file.write_text(code_blocks[1])
            lint_code(python_file)




if __name__ == "__main__":
    from cobol_converter.service.cobol_retriever_function import list_cobol_files
    cobol_conversion(list_cobol_files())