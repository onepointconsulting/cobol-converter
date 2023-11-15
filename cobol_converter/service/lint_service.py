from pathlib import Path

from pylint.lint.run import Run


def lint_code(file: Path):
    parent = file.parent
    file_name = file.name
    output_file = parent / f"{file_name}_lint.txt"
    Run([f"--output={output_file}", file.as_posix()], exit=False)
