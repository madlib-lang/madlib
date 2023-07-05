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
char *madlib__number__internal__inspectByte(unsigned char i);

madlib__maybe__Maybe_t *madlib__number__scanByte(char *s);

// Float
double unboxDouble(double *x);
double *boxDouble(double x);

char *madlib__number__internal__showFloat(double d);
char *madlib__number__internal__inspectFloat(double *i);

madlib__maybe__Maybe_t *madlib__number__scanFloat(char *s);

// Integer
char *madlib__number__internal__showInteger(int64_t i);
char *madlib__number__internal__inspectInteger(int64_t i);

madlib__maybe__Maybe_t *madlib__number__scanInteger(char *s);


// Short
char *madlib__number__internal__showShort(int32_t i);
char *madlib__number__internal__inspectShort(int32_t i);
madlib__maybe__Maybe_t *madlib__number__scanShort(char *s);


// conversion
double madlib__number__intToFloat(int64_t x);
unsigned char madlib__number__intToByte(int64_t x);
int32_t madlib__number__intToShort(int64_t x);

double madlib__number__byteToFloat(unsigned char x);
int64_t madlib__number__byteToInt(unsigned char x);
int32_t madlib__number__byteToShort(unsigned char x);

int64_t madlib__number__floatToInt(double x);
unsigned char madlib__number__floatToByte(double x);
int32_t madlib__number__floatToShort(double x);

int64_t madlib__number__shortToInt(int32_t x);
double madlib__number__shortToFloat(int32_t x);
unsigned char madlib__number__shortToByte(int32_t x);

int32_t madlib__number__shortToChar(int32_t x);
int32_t madlib__number__charToShort(int32_t x);


#ifdef __cplusplus
}
#endif

#endif  // NUMBERS_H