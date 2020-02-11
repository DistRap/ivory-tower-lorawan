#ifndef _LORAWAN_CRYPTO_H
#define _LORAWAN_CRYPTO_H

#include "aes.h"
#include "cmac.h"

#define LEN 16
#define AES_BUF_LEN 128
#define CMAC_BUF_LEN 128
#define CMAC_TAG_LEN 4

void aes_enc(const uint8_t key[LEN], const uint8_t buf[LEN], uint8_t encoded[LEN]);
void aes_buf(const uint8_t key[LEN], const uint8_t buf[AES_BUF_LEN], uint8_t *len, uint8_t encoded[AES_BUF_LEN]);
void cmac(const uint8_t key[LEN], uint8_t buf[CMAC_BUF_LEN], const uint8_t *size, uint8_t tag[CMAC_TAG_LEN]);

#endif
