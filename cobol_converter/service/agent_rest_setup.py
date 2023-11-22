from typing import Tuple
from autogen import AssistantAgent, UserProxyAgent, GroupChat, GroupChatManager
from cobol_converter.autogen_config import llm_config

from cobol_converter.service.agent_setup import (
    AgentType,
    UserProxyType,
    python_code_critic_factory,
)
from cobol_converter.service.terminators import terminate_lambda
from cobol_converter.toml_support import prompts
from cobol_converter.config import cfg


def python_code_rest_generator() -> AssistantAgent:
    return AssistantAgent(
        name=AgentType.AGENT_REST_INTERFACE_GENERATOR,
        system_message=prompts["agents"]["code_rest"]["system_message"],
        llm_config=llm_config,
        is_termination_msg=terminate_lambda,
    )


def user_proxy_rest_factory() -> UserProxyAgent:
    user_proxy_agent = UserProxyAgent(
        name=UserProxyType.REST_PROXY,
        human_input_mode="NEVER",
        max_consecutive_auto_reply=10,
        is_termination_msg=terminate_lambda,
        code_execution_config=False,  # No execution of code, because this was stalling the code generation in Windows
        system_message=prompts["agents"]["rest_user_proxy"]["system_message"],
    )
    return user_proxy_agent


def create_rest_group_chat_manager(user_proxy: UserProxyAgent) -> GroupChatManager:
    groupchat = GroupChat(
        agents=[user_proxy, python_code_rest_generator(), python_code_critic_factory()],
        messages=[],
        max_round=cfg.max_tries,
    )
    return GroupChatManager(groupchat=groupchat, llm_config=llm_config)


def create_user_proxy_chat_manager() -> Tuple[UserProxyAgent, GroupChatManager]:
    rest_user_proxy = user_proxy_rest_factory()
    rest_group_chat_manager = create_rest_group_chat_manager(rest_user_proxy)
    return rest_user_proxy, rest_group_chat_manager


def initiate_rest_chat(
    user_proxy: UserProxyAgent, group_chat_manager: GroupChatManager, python_code: str
):
    user_proxy_message = (prompts["agents"]["rest_user_proxy"]["message"]).format(
        python_code=python_code
    )
    user_proxy.initiate_chat(
        group_chat_manager,
        message=user_proxy_message,
    )


if __name__ == "__main__":
    rest_user_proxy = user_proxy_rest_factory()
    rest_group_chat_manager = create_rest_group_chat_manager(rest_user_proxy)
    rest_user_proxy, rest_group_chat_manager = create_user_proxy_chat_manager()
    assert not rest_user_proxy is None
    assert not rest_group_chat_manager is None
