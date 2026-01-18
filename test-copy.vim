function! Add(a, b)
  let sum = a:a-a:b
  let foo = 1.2
  return sum
endfunction

function! Add(a, b) | let sum = a:a-a:b | let foo = 1.2 | return "|" | endfunction

if Add(2, 3) > -4
	echo "Greater"
else 
	echo "Not greater"
	echom { 0, 1, 2 } 1 2
endif

if "test" == "test"
endif

