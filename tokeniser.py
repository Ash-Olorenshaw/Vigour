from typing import Tuple


special_tkns = [ "=", "+", "-", "*", "/", "%", "==", "!=", ">", ">=", "<", "<=", "=~", "!~", "==?", "==#", "..", "." ]
bracket_pair_tkns = { '{' : '}', '(' : ')', '[' : ']', "'" : "'", '"' : '"' }
keyword_tkns = [ "if", "elseif", "else", "endif",
                "while", "endwhile",
                "for", "in", "endfor",
                "try", "catch", "finally", "endtry",
                "function", "return", "endfunction",
                "range", "dict",
                "call", "execute", "normal",
                "let", "unlet", "delfunction",
                "break", "continue",
                "else", "elseif"]
# note to self - ignore this: 1.2e-3

def is_special_tkn(line : str, target_char : int) -> Tuple[bool, str]:
    for tkn in special_tkns:
        if tkn == line[target_char:(len(tkn) + target_char)]:
            return True, tkn
    return False, ''

def is_bracket_tkn(line : str, target_char : int) -> Tuple[bool, int]:
    if line[target_char] in bracket_pair_tkns:
        bracket_end = bracket_pair_tkns[line[target_char]]
        return True, line.find(bracket_end, target_char)
    return False, 0

def add_line_tkn(line_tokens : list[str], tkn : str):
    if tkn and tkn != '\\':
        line_tokens.append(tkn)


def generate_program_line_tokens(program_lines : list[str]) -> list[list[str]]:
    result : list[list[str]] = []

    for li in range(len(program_lines)):
        line : str = program_lines[li]
        line_tokens : list[str] = []
        current_line : str = ""
        skip_to : int = -1
        for ci in range(len(line)): 
            c = line[ci]

            if ci < skip_to:
                current_line += c
                continue

            if c.isspace():
                add_line_tkn(line_tokens, current_line)
                current_line = ""

            is_bracket, skip_to = is_bracket_tkn(line, ci)

            if is_bracket:
                current_line += c
                continue

            is_special, tkn = is_special_tkn(line, ci)
            if is_special:
                add_line_tkn(line_tokens, current_line)
                line_tokens.append(tkn)
                skip_to = ci + len(tkn) - 1
                current_line = ""
            elif ci == len(line) - 1:
                current_line += c
                add_line_tkn(line_tokens, current_line)
                current_line = ""
            else:
                current_line += c

        line_tokens = list(filter(lambda token : not token.isspace(), line_tokens))
        result.append(line_tokens)

    result = list(filter(lambda tkn_line : len(tkn_line) > 0, result))
    return result

