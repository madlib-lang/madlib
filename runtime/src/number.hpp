#ifndef NUMBERS_H
#define NUMBERS_H

#include <stdint.h>
#include <iostream>
#include "maybe.hpp"

#ifdef __cplusplus
extern "C" {
#endif

// Byte
char *madlib__number__internal__showByte(unsigned char i);
char **madlib__number__internal__inspectByte(unsigned char *i);

unsigned char *madlib__number__internal__numberToByte(int64_t *a);
unsigned char *madlib__number__internal__negateByte(unsigned char *a);

unsigned char *madlib__number__internal__addBytes(unsigned char *a, unsigned char *b);
unsigned char *madlib__number__internal__substractBytes(unsigned char *a, unsigned char *b);
unsigned char *madlib__number__internal__multiplyBytes(unsigned char *a, unsigned char *b);

unsigned char *madlib__number__internal__andBytes(unsigned char *a, unsigned char *b);
unsigned char *madlib__number__internal__orBytes(unsigned char *a, unsigned char *b);
unsigned char *madlib__number__internal__xorBytes(unsigned char *a, unsigned char *b);
unsigned char *madlib__number__internal__complementBytes(unsigned char *a);
unsigned char *madlib__number__internal__leftShiftBytes(unsigned char *a, unsigned char *b);
unsigned char *madlib__number__internal__rightShiftBytes(unsigned char *a, unsigned char *b);

bool *madlib__number__internal__gtBytes(unsigned char *a, unsigned char *b);
bool *madlib__number__internal__ltBytes(unsigned char *a, unsigned char *b);
bool *madlib__number__internal__gteBytes(unsigned char *a, unsigned char *b);
bool *madlib__number__internal__lteBytes(unsigned char *a, unsigned char *b);

bool *madlib__number__internal__eqByte(unsigned char *a, unsigned char *b);

madlib__maybe__Maybe_t *madlib__number__scanByte(char *s);

// Float
char *madlib__number__internal__showFloat(double d);
char **madlib__number__internal__inspectFloat(double *i);

double *madlib__number__internal__numberToFloat(int64_t *a);
double *madlib__number__internal__negateFloat(double *a);
double *madlib__number__internal__addFloats(double *a, double *b);
double *madlib__number__internal__substractFloats(double *a, double *b);
double *madlib__number__internal__multiplyFloats(double *a, double *b);
bool *madlib__number__internal__gtFloats(double *a, double *b);
bool *madlib__number__internal__ltFloats(double *a, double *b);
bool *madlib__number__internal__gteFloats(double *a, double *b);
bool *madlib__number__internal__lteFloats(double *a, double *b);

bool *madlib__number__internal__eqFloat(double *a, double *b);

madlib__maybe__Maybe_t *madlib__number__scanFloat(char *s);

// Integer
char *madlib__number__internal__showInteger(int64_t i);
char **madlib__number__internal__inspectInteger(int64_t *i);

int64_t *madlib__number__internal__numberToInteger(int64_t *a);
int64_t *madlib__number__internal__negateInteger(int64_t *a);
int64_t *madlib__number__internal__addIntegers(int64_t *a, int64_t *b);
int64_t *madlib__number__internal__substractIntegers(int64_t *a, int64_t *b);
int64_t *madlib__number__internal__multiplyIntegers(int64_t *a, int64_t *b);

int64_t *madlib__number__internal__andIntegers(int64_t *a, int64_t *b);
int64_t *madlib__number__internal__orIntegers(int64_t *a, int64_t *b);
int64_t *madlib__number__internal__xorIntegers(int64_t *a, int64_t *b);
int64_t *madlib__number__internal__complementIntegers(int64_t *a);
int64_t *madlib__number__internal__leftShiftIntegers(int64_t *a, int64_t *b);
int64_t *madlib__number__internal__rightShiftIntegers(int64_t *a, int64_t *b);

bool *madlib__number__internal__gtIntegers(int64_t *a, int64_t *b);
bool *madlib__number__internal__ltIntegers(int64_t *a, int64_t *b);
bool *madlib__number__internal__gteIntegers(int64_t *a, int64_t *b);
bool *madlib__number__internal__lteIntegers(int64_t *a, int64_t *b);

bool *madlib__number__internal__eqInteger(int64_t *a, int64_t *b);

madlib__maybe__Maybe_t *madlib__number__scanInteger(char *s);


// conversion
double madlib__number__intToFloat(int64_t x);
char madlib__number__intToByte(int64_t x);

double madlib__number__byteToFloat(char x);
int64_t madlib__number__byteToInt(char x);

int64_t madlib__number__floatToInt(double x);
char madlib__number__floatToByte(double x);


#ifdef __cplusplus
}
#endif

#endif  // NUMBERS_H