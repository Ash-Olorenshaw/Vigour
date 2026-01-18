#include "../internal.h"
#include "../io.h"

vim_var vim_add(vim_var elem1, vim_var elem2) {
	vim_var result;
	if (elem1.type == Number) {
		result.type = Number;
		if (elem2.type == Number)
			result.val.Number = elem1.val.Number + elem2.val.Number;
		else if (elem2.type == Float)
			result.val.Number = elem1.val.Number + elem2.val.Float;
		else 
			raise_err("Cannot perform addition operation with specified types.");
	}
	else if (elem1.type == Float) {
		result.type = Float;
		if (elem2.type == Number)
			result.val.Float = elem1.val.Float + elem2.val.Number;
		else if (elem2.type == Float)
			result.val.Float = elem1.val.Float + elem2.val.Float;
		else 
			raise_err("Cannot perform addition operation with specified types.");
	}
	else if (elem1.type == String) {
		// TODO
	}
	else {
		raise_err("Cannot perform addition operation with specified types.");
	}

	return result;
}

vim_var vim_sub(vim_var elem1, vim_var elem2) {
	vim_var result;
	if (elem1.type == Number) {
		result.type = Number;
		if (elem2.type == Number)
			result.val.Number = elem1.val.Number - elem2.val.Number;
		else if (elem2.type == Float)
			result.val.Number = elem1.val.Number - elem2.val.Float;
		else 
			raise_err("Cannot perform subtract operation with specified types.");
	}
	else if (elem1.type == Float) {
		result.type = Float;
		if (elem2.type == Number)
			result.val.Float = elem1.val.Float - elem2.val.Number;
		else if (elem2.type == Float)
			result.val.Float = elem1.val.Float - elem2.val.Float;
		else 
			raise_err("Cannot perform subtract operation with specified types.");
	}
	else if (elem1.type == String) {
		// TODO
	}
	else {
		raise_err("Cannot perform subtract operation with specified types.");
	}

	return result;
}

vim_var vim_mult(vim_var elem1, vim_var elem2) {
	vim_var result;
	if (elem1.type == Number) {
		result.type = Number;
		if (elem2.type == Number)
			result.val.Number = elem1.val.Number * elem2.val.Number;
		else if (elem2.type == Float)
			result.val.Number = elem1.val.Number * elem2.val.Float;
		else 
			raise_err("Cannot perform multiplication operation with specified types.");
	}
	else if (elem1.type == Float) {
		result.type = Float;
		if (elem2.type == Number)
			result.val.Float = elem1.val.Float * elem2.val.Number;
		else if (elem2.type == Float)
			result.val.Float = elem1.val.Float * elem2.val.Float;
		else 
			raise_err("Cannot perform multiplication operation with specified types.");
	}
	else if (elem1.type == String) {
		// TODO
	}
	else {
		raise_err("Cannot perform multiplication operation with specified types.");
	}

	return result;
}

vim_var vim_div(vim_var elem1, vim_var elem2) {
	vim_var result;
	if (elem1.type == Number) {
		result.type = Number;
		if (elem2.type == Number) {
			if (elem2.val.Number == 0) 
				raise_err("Cannot perform division by 0.");
			result.val.Number = elem1.val.Number / elem2.val.Number;
		}
		else if (elem2.type == Float) {
			if (elem2.val.Float == 0) 
				raise_err("Cannot perform division by 0.");
			result.val.Number = elem1.val.Number / elem2.val.Float;
		}
		else 
			raise_err("Cannot perform multiplication operation with specified types.");
	}
	else if (elem1.type == Float) {
		result.type = Float;
		if (elem2.type == Number) {
			if (elem2.val.Number == 0) 
				raise_err("Cannot perform division by 0.");
			result.val.Float = elem1.val.Float / elem2.val.Number;
		}
		else if (elem2.type == Float) {
			if (elem2.val.Float == 0) 
				raise_err("Cannot perform division by 0.");
			result.val.Float = elem1.val.Float / elem2.val.Float;
		}
		else 
			raise_err("Cannot perform multiplication operation with specified types.");
	}
	else if (elem1.type == String) {
		// TODO
	}
	else {
		raise_err("Cannot perform multiplication operation with specified types.");
	}

	return result;
}
