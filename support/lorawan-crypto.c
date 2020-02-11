#include "lorawan-crypto.h"

void cmac(const uint8_t key[LEN], uint8_t buf[CMAC_BUF_LEN], const uint8_t *size, uint8_t tag[CMAC_TAG_LEN]) {
    uint8_t fullTag[LEN];
    cmac_ctx contx[1];
    cmac_init(key, contx);
    cmac_data(buf, *size, contx);
    cmac_end(fullTag, contx);
    for(uint8_t i = 0; i <= CMAC_TAG_LEN; i++) tag[i] = fullTag[i];
}

void aes_enc(const uint8_t key[LEN], const uint8_t buf[LEN], uint8_t encoded[LEN]) {
    aes_encrypt_ctx cx[1];
    aes_encrypt_key128(key, cx);
    aes_encrypt(buf, encoded, cx);
}

void aes_buf(const uint8_t key[LEN], const uint8_t buf[AES_BUF_LEN], uint8_t *len, uint8_t encoded[AES_BUF_LEN]) {
    aes_encrypt_ctx cx[1];
    aes_encrypt_key128(key, cx);
    for(int i=0; i<*len; i+=16) {
      aes_encrypt(&buf[i], &encoded[i], cx);
    }
}
