#include "./internal.h"
#include "./equality.h"

#define COMP_FUNC(__comparator__) \
	if (elem1.type == Float) { \
		if (elem2.type == Float) { \
			return CREATE_SIMPLE_VIM_VAR((elem1.val.Float __comparator__ elem2.val.Float), Number); \
		} \
		else if (elem2.type == Number) { \
			return CREATE_SIMPLE_VIM_VAR((elem1.val.Float __comparator__ elem2.val.Number), Number); \
		} \
	} \
	else if (elem1.type == Number) { \
		if (elem2.type == Float) { \
			return CREATE_SIMPLE_VIM_VAR((elem1.val.Number __comparator__ elem2.val.Float), Number); \
		} \
		else if (elem2.type == Number) { \
			return CREATE_SIMPLE_VIM_VAR((elem1.val.Number __comparator__ elem2.val.Number), Number); \
		} \
	} \
	else { \
	} \
	return CREATE_SIMPLE_VIM_VAR(0, Number);


vim_var vim_eq(vim_var elem1, vim_var elem2, vim_case_sensitivity mode) {
	COMP_FUNC(==);
}

vim_var vim_ne(vim_var elem1, vim_var elem2, vim_case_sensitivity mode) {
	COMP_FUNC(!=);
}

vim_var vim_gt(vim_var elem1, vim_var elem2, vim_case_sensitivity mode) {
	COMP_FUNC(>);
}

vim_var vim_gte(vim_var elem1, vim_var elem2, vim_case_sensitivity mode) {
	COMP_FUNC(>=);
}

vim_var vim_lt(vim_var elem1, vim_var elem2, vim_case_sensitivity mode) {
	COMP_FUNC(<);
}

vim_var vim_lte(vim_var elem1, vim_var elem2, vim_case_sensitivity mode) {
	COMP_FUNC(<=);
}


// equal			==		==#		==?
// not equal		!=		!=#		!=?
// greater than		>		>#		>?
// greater than or equal	>=		>=#		>=?
// smaller than		<		<#		<?
// smaller than or equal	<=		<=#		<=?
// regexp matches		=~		=~#		=~?
// regexp doesn't match	!~		!~#		!~?
// same instance		is		is#		is?
// different instance	isnot		isnot#		isnot?
//
//
// A |List| can only be compared with a |List| and only "equal", "not equal",
// "is" and "isnot" can be used.  This compares the values of the list,
// recursively.  Ignoring case means case is ignored when comparing item values.
//
//
// A |Dictionary| can only be compared with a |Dictionary| and only "equal", "not
// equal", "is" and "isnot" can be used.  This compares the key/values of the
// |Dictionary| recursively.  Ignoring case means case is ignored when comparing
// item values.
// 
//
// A |Funcref| can only be compared with a |Funcref| and only "equal", "not
// equal", "is" and "isnot" can be used.  Case is never ignored.  Whether
// arguments or a Dictionary are bound (with a partial) matters.  The
// Dictionaries must also be equal (or the same, in case of "is") and the
// arguments must be equal (or the same).
//
// To compare Funcrefs to see if they refer to the same function, ignoring bound
// Dictionary and arguments, use |get()| to get the function name: >
// 	if get(Part1, 'name') == get(Part2, 'name')
// 	   " Part1 and Part2 refer to the same function
//
//
// Using "is" or "isnot" with a |List|, |Dictionary| or |Blob| checks whether
// the expressions are referring to the same |List|, |Dictionary| or |Blob|
// instance.  A copy of a |List| is different from the original |List|.  When
// using "is" without a |List|, |Dictionary| or |Blob|, it is equivalent to
// using "equal", using "isnot" is equivalent to using "not equal".  Except that
// a different type means the values are different: >
// 	echo 4 == '4'
// 	1
// 	echo 4 is '4'
// 	0
// 	echo 0 is []
// 	0
// "is#"/"isnot#" and "is?"/"isnot?" can be used to match and ignore case.
//
// When comparing a String with a Number, the String is converted to a Number,
// and the comparison is done on Numbers.  This means that: >
// 	echo 0 == 'x'
// 	1
// because 'x' converted to a Number is zero.  However: >
// 	echo [0] == ['x']
// 	0
// Inside a List or Dictionary this conversion is not used.
//
// When comparing two Strings, this is done with strcmp() or stricmp().  This
// results in the mathematical difference (comparing byte values), not
// necessarily the alphabetical difference in the local language.
//
// When using the operators with a trailing '#', or the short version and
// 'ignorecase' is off, the comparing is done with strcmp(): case matters.
//
// When using the operators with a trailing '?', or the short version and
// 'ignorecase' is set, the comparing is done with stricmp(): case is ignored.
//
// 'smartcase' is not used.
//
// The "=~" and "!~" operators match the lefthand argument with the righthand
// argument, which is used as a pattern.  See |pattern| for what a pattern is.
// This matching is always done like 'magic' was set and 'cpoptions' is empty, no
// matter what the actual value of 'magic' or 'cpoptions' is.  This makes scripts
// portable.  To avoid backslashes in the regexp pattern to be doubled, use a
// single-quote string, see |literal-string|.
// Since a string is considered to be a single line, a multi-line pattern
// (containing \n, backslash-n) will not match.  However, a literal NL character
// can be matched like an ordinary character.  Examples:
// 	"foo\nbar" =~ "\n"	evaluates to 1
// 	"foo\nbar" =~ "\\n"	evaluates to 0
//

