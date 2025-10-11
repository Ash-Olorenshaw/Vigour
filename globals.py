
from typing import Tuple
from vars import Variable

# structure : [ ( { foo: 0 }, 19 ) ]
scopes : list[Tuple[dict[str, Variable], int]] = [] # (vars are these - s:, a:, None)
