import tokeniser
import line_creator

program_text = """
function! Add(a, b)
  let sum = a:a+a:b
  return sum
endfunction

if Add(2, 3) > 4
  echo "Greater"
else
  echo "Not greater"
endif
"""

print(tokeniser.generate_program_line_tokens(line_creator.generate_program_lines(program_text)))
