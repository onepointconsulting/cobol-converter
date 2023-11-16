from pathlib import Path

from typing import List, Optional

from cobol_converter.service.agent_setup import user_proxy, conversion_manager

from cobol_converter.service.code_extractor import extract_code
from cobol_converter.service.lint_service import lint_code
from cobol_converter.service.format_service import format_file
from cobol_converter.toml_support import prompts
from cobol_converter.config import cfg
from cobol_converter.log_factory import logger
from cobol_converter.service.python_test_runner import run_subprocess


def cobol_conversion(cobol_files: List[Path]):
    assert cobol_files is not None
    for cobol_file in cobol_files:
        try:
            convert_single_file(cobol_file)
        except:
            logger.exception(f"Failed to convert file {cobol_file}")


def process_python_file(file: Path, content: str):
    if len(content.strip()) > 0:
        file.write_text(content)
        format_file(file)
        lint_code(file)


def convert_single_file(cobol_file: Path):
    cobol_code = cobol_file.read_text()
    user_proxy_message = (prompts["agents"]["userproxy"]["message"]).format(
        cobol_code=cobol_code
    )
    user_proxy.initiate_chat(
        conversion_manager,
        message=user_proxy_message,
    )

    def process_message(message: dict, prefix: str, suffix: str = "py") -> Optional[Path]:
        if "content" in message:
            content = message["content"]
            conversion_python_dir = cfg.conversion_python_dir
            code_blocks = extract_code(content)
            code_blocks_len = len(code_blocks)
            some_file = conversion_python_dir / f"{prefix}{cobol_file.stem}.{suffix}"
            if code_blocks_len > 0:
                if suffix == "py":
                    process_python_file(some_file, code_blocks[0])
            elif suffix == "txt":
                some_file.write_text(content)
            return some_file
        return None

    for message in user_proxy.chat_messages[conversion_manager]:
        if "name" in message:
            agent_name = message["name"]
            match agent_name:
                case "Python_Coder":
                    process_message(message, "")
                case "Unit_Tester":
                    test_file = process_message(message, "test_")
                    if test_file is not None:
                        run_subprocess(test_file)
                case "Code_Critic":
                    process_message(message, "critique_", "txt")


if __name__ == "__main__":
    from cobol_converter.service.cobol_retriever_function import list_cobol_files

    cobol_conversion(list_cobol_files())
