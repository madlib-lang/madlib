# Madlib runtime library

## Description
Runtime library that powers madlib's native code. It is based on libuv.

## Note on naming
Names in header files must be defined as follow:
- for functions -> madlib__{module_name}[__internal]__functionName
- for types -> madlib__{module_name}[__internal]__TypeName_t

## Madlib data structures

### List
A list is defined as follows:

```c
typedef struct madlib__list__Node {
  void *value;
  struct madlib__list__Node *next;
} madlib__list__Node_t;
```

Important: To be able to reassign/mutate a reference to a list, the boxed list is represented as `madlib__list__Node_t**`. Also we need a pointer for the empty list, so for now an empty list is represented as `{ .value = NULL, .next = NULL }`, and a list with one item as `{ .value = xyz, .next = { .value = NULL, .next = NULL }}`.


### String
A String is defined as a `char*`. A boxed String is defined as a `char**`.

## Dependencies

- libuv
- libgc
- http-parser
