#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define ARENA_MAX_SIZE 1024

#define ARENA_INIT(target_arena) ({ \
		memset(target_arena->arena, 0, ARENA_MAX_SIZE); \
		target_arena->size = 0; \
		target_arena->max_size = ARENA_MAX_SIZE; \
	})

typedef char vim_byte;
typedef double float64;
typedef int64_t int64;
typedef char *vim_string;

typedef union {
	float64 Float;
	vim_string String;
	int64 Number; // we just do everything as 64 (maybe 32 in future?)
	void *Object; // Funcref, List, Dictionary
	vim_byte *Blob;
} vim_true_val;

typedef enum {
	Number,
	Float,
	String, 
	Funcref, // currently not available from the tokeniser
	List,
	Dictionary,
	Blob // currently not available from the tokeniser
} vim_type;

typedef struct {
	vim_true_val val;
	vim_type type;
} vim_var;

typedef struct {
	vim_string name;
	vim_var val;
} vim_dict_item;

typedef vim_dict_item *vim_dict;
typedef vim_var *vim_list; // not a real list because we aren't dumb!!

typedef struct {
	void *arena[ARENA_MAX_SIZE];
	int size, max_size;
} arena;

void *arena_reserve(arena *target_arena, int space_size) {
	if (target_arena->size < target_arena->max_size) {
		void *new_space = malloc(space_size);
		if (new_space == NULL) {
			fprintf(stderr, "Err - Failed to allocate space for arena.");
			exit(1);
		}
		memset(new_space, 0, space_size);
		target_arena->arena[target_arena->size] = new_space;
		target_arena->size++;
		return new_space;
	}
	else 
		return NULL;
}

void arena_free(arena *target_arena) {
	for (int i = 0; i < target_arena->size; i++) {
		free(target_arena->arena[i]);
		target_arena->arena[i] = NULL;
	}
	target_arena->size = 0;
}


