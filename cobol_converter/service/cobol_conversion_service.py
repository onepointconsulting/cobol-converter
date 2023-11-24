from pathlib import Path

from typing import List, Optional

from autogen import UserProxyAgent
from autogen.agentchat.conversable_agent import ConversableAgent

from cobol_converter.service.agent_setup import user_proxy, conversion_manager

from cobol_converter.service.code_extractor import extract_code
from cobol_converter.service.lint_service import lint_code
from cobol_converter.service.format_service import format_file
from cobol_converter.toml_support import prompts
from cobol_converter.config import cfg
from cobol_converter.log_factory import logger
from cobol_converter.service.python_test_runner import run_subprocess
from cobol_converter.service.agent_setup import AgentType
from cobol_converter.service.agent_rest_setup import (
    create_user_proxy_chat_manager,
    initiate_rest_chat,
)


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


def process_message(
    message: dict, prefix: str, cobol_file: Path, suffix: str = "py"
) -> Optional[Path]:
    if "content" in message:
        content = message["content"]
        conversion_python_dir = cfg.conversion_python_dir
        code_blocks = extract_code(content)
        code_blocks_len = len(code_blocks)
        conversion_folder = conversion_python_dir / cobol_file.stem
        conversion_folder.mkdir(parents=True, exist_ok=True)
        assert conversion_folder, f"Folder {conversion_folder} does not exist."
        some_file = conversion_folder / f"{prefix}{cobol_file.stem}.{suffix}"
        if code_blocks_len > 0:
            if suffix == "py":
                process_python_file(some_file, code_blocks[0])
        elif suffix == "txt":
            some_file.write_text(content)
        return some_file
    return None


def process_rest_conversion(file: Path):
    python_code = file.read_text()

    rest_user_proxy, rest_group_chat_manager = create_user_proxy_chat_manager()

    initiate_rest_chat(rest_user_proxy, rest_group_chat_manager, python_code)

    for message, agent_name in loop_chat_messages(
        rest_user_proxy, rest_group_chat_manager
    ):
        match agent_name:
            case AgentType.AGENT_REST_INTERFACE_GENERATOR:
                content = message["content"]
                code_blocks = extract_code(content)
                if len(code_blocks) > 0:
                    rest_interface_file = (
                        cfg.conversion_python_dir / file.stem / f"rest_{file.stem}.py"
                    )
                    all_code = "\n\n".join(code_blocks)
                    if not rest_interface_file.parent.exists():
                        rest_interface_file.parent.mkdir(parents=True, exist_ok=True)
                    process_python_file(rest_interface_file, all_code)
            case AgentType.AGENT_CODE_CRITIC:
                process_message(message, "rest_critique_", file, "txt")


def loop_chat_messages(user_proxy: UserProxyAgent, manager: ConversableAgent):
    for message in user_proxy.chat_messages[manager]:
        if "name" in message:
            agent_name = message["name"]
            yield message, agent_name


def convert_single_file(cobol_file: Path):
    """
    Convert a single COBOL file to Python code and perform additional processing.

    :param cobol_file: The Path object representing the input COBOL file.
    :type cobol_file: Path

    This function reads the content of the COBOL file, initiates a chat with the user proxy
    and processes messages received during the chat.
    The messages are handled by different agents based on their types.

    - For messages from the Python coder agent, the COBOL code is converted to a Python file,
      and additional processing is performed using the `process_rest_conversion` function.

    - For messages from the unit tester agent, a test file is generated and executed using
      the `run_subprocess` function.

    - For messages from the REST interface generator agent, the corresponding processing is done.

    - For messages from the code critic agent, a critique file is generated.

    :raises: Exceptions may be raised during the conversion and processing steps.

    This function relies on the following global variables:
    - `prompts`: A dictionary containing messages for different agents.
    - `user_proxy`: An object representing the user proxy for chat-based interaction.
    - `conversion_manager`: An object managing the conversion process.
    """
    cobol_code = cobol_file.read_text()
    user_proxy_message = (prompts["agents"]["userproxy"]["message"]).format(
        cobol_code=cobol_code
    )
    user_proxy.initiate_chat(
        conversion_manager,
        message=user_proxy_message,
    )

    for message, agent_name in loop_chat_messages(user_proxy, conversion_manager):
        match agent_name:
            case AgentType.AGENT_PYTHON_CODER:
                python_file = process_message(message, "", cobol_file)
                process_rest_conversion(python_file)
            case AgentType.AGENT_UNIT_TESTER:
                test_file = process_message(message, "test_", cobol_file)
                if test_file is not None:
                    run_subprocess(test_file)
            case AgentType.AGENT_REST_INTERFACE_GENERATOR:
                process_message(message, "rest_", cobol_file)
            case AgentType.AGENT_CODE_CRITIC:
                process_message(message, "critique_", cobol_file, "txt")
