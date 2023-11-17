from pathlib import Path
import subprocess

from cobol_converter.log_factory import logger


def run_subprocess(script_path: Path):
    try:
        proc = subprocess.run(
            ["python.exe", script_path.as_posix()],
            capture_output=True,
            check=True,
            shell=True,
        )
        test_output_file = script_path.parent / f"{script_path.name}_test_output.log"
        with open(test_output_file, "wb") as f:
            f.write(b"\n=====================\nStandard Out:\n")
            f.write(proc.stdout)
            f.write(b"\n=====================\nErrors:\n")
            f.write(proc.stderr)
    except subprocess.CalledProcessError as grepexc:
        logger.exception(
            f"Process exited with {grepexc.returncode} and {grepexc.output} "
        )
    except Exception as e:
        logger.exception(f"Failed to run process for {script_path}")
        test_output_file = script_path.parent / f"{script_path.name}_test_error.log"
        test_output_file.write_text(str(e))