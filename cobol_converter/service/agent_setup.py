from typing import List
from autogen import AssistantAgent, UserProxyAgent, GroupChat, GroupChatManager

from cobol_converter.autogen_config import llm_config
from cobol_converter.service.cobol_retriever_function import list_cobol_files
from cobol_converter.autogen_config import config_list
from cobol_converter.config import cfg
from cobol_converter.service.code_extractor import extract_code
from cobol_converter.toml_support import prompts
from cobol_converter.service.terminators import terminate_lambda


class AgentType:
    AGENT_PYTHON_CODER = "Python_Coder"
    AGENT_UNIT_TESTER = "Unit_Tester"
    AGENT_CODE_CRITIC = "Code_Critic"
    AGENT_REST_INTERFACE_GENERATOR = "Code_REST_Generator"


class UserProxyType:
    MAIN_PROXY = "user_proxy"
    REST_PROXY = "rest_user_proxy"


llm_config = {
    "cache_seed": cfg.seed,  # seed for caching and reproducibility
    "config_list": config_list,  # a list of OpenAI API configurations
    "temperature": cfg.temperature,  # temperature for sampling
}


def cobol_convert_agent_factory() -> AssistantAgent:
    assistant = AssistantAgent(
        name=AgentType.AGENT_PYTHON_CODER,
        system_message=prompts["agents"]["python_coder"]["system_message"],
        llm_config=llm_config,
    )
    return assistant


def python_test_agent_factory() -> AssistantAgent:
    assistant = AssistantAgent(
        name=AgentType.AGENT_UNIT_TESTER,
        system_message=prompts["agents"]["python_unit_tester"]["system_message"],
        llm_config=llm_config,
    )
    return assistant


def python_code_critic_factory() -> AssistantAgent:
    assistant = AssistantAgent(
        name=AgentType.AGENT_CODE_CRITIC,
        system_message=prompts["agents"]["code_critic"]["system_message"],
        llm_config=llm_config,
        is_termination_msg=terminate_lambda,
    )
    return assistant


def user_proxy_factory() -> UserProxyAgent:
    user_proxy_agent = UserProxyAgent(
        name=UserProxyType.MAIN_PROXY,
        human_input_mode="NEVER",
        max_consecutive_auto_reply=10,
        is_termination_msg=terminate_lambda,
        code_execution_config={"work_dir": cfg.code_execution_dir, "use_docker": False},
        system_message=prompts["agents"]["userproxy"]["system_message"],
    )
    return user_proxy_agent


def create_group_chat_manager(
    user_proxy: UserProxyAgent,
    agents: List[AssistantAgent],
) -> GroupChatManager:
    groupchat = GroupChat(
        agents=[user_proxy, *agents],
        messages=[],
        max_round=cfg.max_tries,
    )
    manager = GroupChatManager(groupchat=groupchat, llm_config=llm_config)
    return manager


user_proxy = user_proxy_factory()
conversion_manager = create_group_chat_manager(
    user_proxy,
    [
        cobol_convert_agent_factory(),
        python_test_agent_factory(),
        python_code_critic_factory(),
    ],
)


if __name__ == "__main__":
    assert conversion_manager is not None
