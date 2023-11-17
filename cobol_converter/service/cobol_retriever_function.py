from typing import List
from cobol_converter.config import cfg


def list_cobol_files(exclusion_file_names: List[str] = []):
    for file in cfg.source_code_dir.rglob("*.cbl"):
        if file.stem not in exclusion_file_names:
            yield file


def get_cobol():
    """
    Retrieve cobol code to be converted.
    """
    files = list(list_cobol_files())
    return "\n\n".join([f.read_text() for f in files])


if __name__ == "__main__":
    from cobol_converter.log_factory import logger

    counter = 0
    for f in list_cobol_files():
        counter += 1
    assert counter > 0, "No files found"
    cobol = get_cobol()
    assert cobol is not None
    logger.info("\n" + cobol)
