from typing import Tuple

from error import raise_err


special_tkns = [ "=", "+", "-", "*", "/", "%", "==", "!=", ">", ">=", "<", "<=", "==?", "==#", "!=?", "!=#" ,"..", ".", "!", "?", ":" ]
# TODO - implement these (vimeval.txt): >#, >?, >=#, >=?, <#, <?, <=#, <=?, =~, =~#, =~?, !~, !~#, !~?, is, is#, is?, isnot, isnot#, isnot?
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
        if (target_char > 0 and target_char < len(line) - 1):
            if tkn == "." and line[target_char] == '.' and line[target_char - 1].isnumeric() and line[target_char + 1].isnumeric():
                return False, ''

        if tkn == line[target_char:(len(tkn) + target_char)]:
            return True, tkn
    return False, ''

def is_bracket_tkn(line : str, target_char : int) -> Tuple[bool, int]:
    if line[target_char] in bracket_pair_tkns:
        bracket_end = bracket_pair_tkns[line[target_char]]
        if line[target_char] != "'" and line[target_char] != '"':
            count = 0
            for ci in range(len(line)):
                c = line[ci]
                if c == bracket_end:
                    count -= 1
                    if count == 0:
                        return True, ci
                elif c == line[target_char]:
                    count += 1

            raise_err(f"Err - failed to find end to '{line[target_char]}' on line: {line}")
        else:
            open = False
            escape_tkn = "\\" if line[target_char] == "\"" else "'"
            for ci in range(len(line)):
                c = line[ci]
                if c == line[target_char] and line[ci - 1] != escape_tkn:
                    open = not open
                    if open == False:
                        return True, ci + 1
            raise_err(f"Err - failed to find end to ' {line[target_char]} ' on line: {line}")
    return False, 0

def add_line_tkn(line_tokens : list[str], tkn : str):
    if tkn and tkn != '\\':
        line_tokens.append(tkn)


def generate_program_line_tokens(program_lines : list[str]) -> list[list[str]]:
    # TODO - ignore comments
    result : list[list[str]] = []

    for li in range(len(program_lines)):
        result.append(generate_line_tokens(program_lines[li]))

    result = list(filter(lambda tkn_line : len(tkn_line) > 0, result))
    return result

def generate_line_tokens(line : str) -> list[str]:
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

    if current_line:
        add_line_tkn(line_tokens, current_line)

    line_tokens = list(filter(lambda token : not token.isspace(), line_tokens))
    return line_tokens
