
from vars import Variable, typify_script


def create_list(target : str) -> Variable:
    list_content = target[1:-1]
    list_items = list_content.split(",")
    return typify_script(list_items), "list"
