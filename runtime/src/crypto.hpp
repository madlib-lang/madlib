#ifndef CRYPTO_H
#define CRYPTO_H

#ifdef __cplusplus
extern "C" {
#endif


char *madlib__crypto__md5(char *input);

char *madlib__crypto__sha256(char *input);


#ifdef __cplusplus
}
#endif

#endif // CRYPTO_H
