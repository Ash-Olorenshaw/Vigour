#include <stdlib.h>
#include "../internal.h"

vim_var vim_str_to_number(vim_var vim_str) {
	/// DOES NOT CHECK IF IS A STRING BEFORE CONVERTING
	return CREATE_SIMPLE_VIM_VAR(atoi(vim_str.val.String), Number);
}

// vim_var vim_number_to_str(vim_var vim_str) {
// 	/// DOES NOT CHECK IF IS A NUMBER BEFORE CONVERTING
// 	// TODO : gcvt
// }



// 	Number 123	-->	String "123" ~
// 	Number 0	-->	String "0" ~
// 	Number -1	-->	String "-1" ~
// 							*octal*
// Conversion from a String to a Number is done by converting the first digits to
// a number.  Hexadecimal "0xf9", Octal "017" or "0o17", and Binary "0b10"
// numbers are recognized.  If the String doesn't start with digits, the result
// is zero. Examples:
// 	String "456"	-->	Number 456 ~
// 	String "6bar"	-->	Number 6 ~
// 	String "foo"	-->	Number 0 ~
// 	String "0xf1"	-->	Number 241 ~
// 	String "0100"	-->	Number 64 ~
// 	String "0o100"	-->	Number 64 ~
// 	String "0b101"	-->	Number 5 ~
// 	String "-8"	-->	Number -8 ~
// 	String "+8"	-->	Number 0 ~
