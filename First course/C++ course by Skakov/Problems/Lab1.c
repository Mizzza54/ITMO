/**
 * @author Michale Gerasimov
 * start: 30.03.2020
 * @version 03.04.2020
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

/**------------------------------------- UTF to unicode point -------------------------------------*/
unsigned long UTF8_to_code_point(unsigned char *const buffer, size_t NoB) {
    unsigned long code_point;
    switch (NoB) {
        case 1:
            code_point = buffer[0];
            return code_point;
        case 2:
            code_point = ((buffer[1] - 0xC0) << 6) + (buffer[0] - 0x80);
            return code_point;
        case 3:
            code_point = ((buffer[2] - 0xE0) << 12) + ((buffer[1] - 0x80) << 6) + (buffer[0] - 0x80);
            return code_point;
        case 4:
            code_point = ((buffer[3] - 0xF0) << 18) + ((buffer[2] - 0x80) << 12) + ((buffer[1] - 0x80) << 6) + (buffer[0] - 0x80);
            return code_point;
    }
    return 0;
}

unsigned long UTF16BE_to_code_point(unsigned char *const buffer, size_t NoB) {
    unsigned long code_point;
    switch (NoB) {
        case 2:
            code_point = (buffer[0] << 8) + buffer[1];
            return code_point;
        case 4:
            code_point = ((((buffer[0] << 8) + buffer[1]) - 0xD800) * 0x0400) + (((buffer[2] << 8) + buffer[3]) - 0xDC00) + 0x10000;
            return code_point;
    }
    return 0;
}

unsigned long UTF16LE_to_code_point(unsigned char *const buffer, size_t NoB) {
    unsigned long code_point;
    switch (NoB) {
        case 2:
            code_point = (buffer[1] << 8) + buffer[0];
            return code_point;
        case 4:
            code_point = ((((buffer[1] << 8) + buffer[0]) - 0xD800) * 0x0400) + (((buffer[3] << 8) + buffer[2]) - 0xDC00) + 0x10000;
            return code_point;
    }
    return 0;
}

unsigned long UTF32BE_to_code_point(unsigned char *const buffer, size_t NoB) {
    unsigned long code_point;
    switch (NoB) {
        case 4:
            code_point = (buffer[0] << 24) + (buffer[1] << 16) + (buffer[2] << 8) + (buffer[3]);
            return code_point;
    }
    return 0;
}

unsigned long UTF32LE_to_code_point(unsigned char *const buffer, size_t NoB) {
    unsigned long code_point;
    switch (NoB) {
        case 4:
            code_point = (buffer[3] << 24) + (buffer[2] << 16) + (buffer[1] << 8) + (buffer[0]);
            return code_point;
    }
    return 0;
}
/**------------------------------------- UTF to unicode point -------------------------------------*/



/**------------------------------------- Unicode point to UTF -------------------------------------*/
size_t code_point_to_UTF8(unsigned char *const buffer, const unsigned long code_point)
                                                        //  Bits for |                                     |   First   and  Last
                                                        // code point|                                     | code point   code point
{                                                       //           |                                     |
    if (code_point <= 0x007F) {                         //     7     | 0xxxxxxx                            | U+0000  <->  U+007F
        buffer[0] = code_point;                         //           |                                     |
        return 1;                                       //           |                                     |
    }                                                   //           |                                     |
    if (code_point <= 0x7FF) {                          //           |                                     |
        buffer[0] = 0xC0 | (code_point >> 6);           //     11    | 110xxxxx 10xxxxxx                   | U+0080  <->  U+07FF
        buffer[1] = 0x80 | (code_point & 0x3F);         //           |                                     |
        return 2;                                       //           |                                     |
    }                                                   //           |                                     |
    if (code_point <= 0xFFFF) {                         //           |                                     |
        buffer[0] = 0xE0 | (code_point >> 12);          //     16    | 1110xxxx 10xxxxxx 10xxxxxx          | U+0800 <->  U+FFFF
        buffer[1] = 0x80 | ((code_point >> 6) & 0x3F);  //           |                                     |
        buffer[2] = 0x80 | (code_point & 0x3F);         //           |                                     |
        return 3;                                       //           |                                     |
    }                                                   //           |                                     |
    if (code_point <= 0x10FFFF) {                       //           |                                     |
        buffer[0] = 0xF0 | (code_point >> 18);          //     21    | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx | U+10000  <->  U+10FFFF
        buffer[1] = 0x80 | ((code_point >> 12) & 0x3F); //           |                                     |
        buffer[2] = 0x80 | ((code_point >> 6) & 0x3F);  //           |                                     |
        buffer[3] = 0x80 | (code_point & 0x3F);         //           |                                     |
        return 4;
    }
    return 0;
}

size_t code_point_to_UTF16BE(unsigned char *const buffer, unsigned long code_point) {
    if (code_point <= 0xFFFF) {
        unsigned long byte1 = code_point >> 8;
        unsigned long byte2 = (code_point << 24) >> 24;
        buffer[0] = byte1;
        buffer[1] = byte2;
        return 2;
    }
    if (code_point <= 0x10FFFF) {
        code_point = code_point - 0x10000;
        unsigned long word1 = ((code_point / 0x400)) + 0xD800;
        unsigned long word2 = (code_point % 0x400) + 0xDC00;
        unsigned long byte1 = word1 >> 8;
        unsigned long byte2 = (word1 << 24) >> 24;
        unsigned long byte3 = word2 >> 8;
        unsigned long byte4 = (word2 << 24) >> 24;
        buffer[0] = byte1;
        buffer[1] = byte2;
        buffer[2] = byte3;
        buffer[3] = byte4;
        return 4;
    }
    return 0;
}

size_t code_point_to_UTF16LE(unsigned char *const buffer, unsigned long code_point) {
    if (code_point <= 0xFFFF) {
        unsigned long byte1 = code_point >> 8;
        unsigned long byte2 = (code_point << 24) >> 24;
        buffer[1] = byte1;
        buffer[0] = byte2;
        return 2;
    }
    if (code_point <= 0x10FFFF) {
        code_point = code_point - 0x10000;
        unsigned long word2 = ((code_point / 0x400)) + 0xD800;
        unsigned long word1 = (code_point % 0x400) + 0xDC00;
        unsigned long byte1 = word1 >> 8;
        unsigned long byte2 = (word1 << 24) >> 24;
        unsigned long byte3 = word2 >> 8;
        unsigned long byte4 = (word2 << 24) >> 24;
        buffer[3] = byte1;
        buffer[2] = byte2;
        buffer[1] = byte3;
        buffer[0] = byte4;
        return 4;
    }
    return 0;
}

size_t code_point_to_UTF32BE(unsigned char *const buffer, const unsigned long code_point) {
    unsigned long byte1 = code_point >> 24;
    unsigned long byte2 = (code_point << 8) >> 24;
    unsigned long byte3 = (code_point << 16) >> 24;
    unsigned long byte4 = (code_point << 24) >> 24;
    buffer[0] = byte1;
    buffer[1] = byte2;
    buffer[2] = byte3;
    buffer[3] = byte4;
    return 4;
}

size_t code_point_to_UTF32LE(unsigned char *const buffer, const unsigned long code_point) {
    unsigned long byte1 = code_point >> 24;
    unsigned long byte2 = (code_point << 8) >> 24;
    unsigned long byte3 = (code_point << 16) >> 24;
    unsigned long byte4 = (code_point << 24) >> 24;
    buffer[3] = byte1;
    buffer[2] = byte2;
    buffer[1] = byte3;
    buffer[0] = byte4;
    return 4;
}
/**------------------------------------- Unicode point to UTF -------------------------------------*/



/**------------------------------------- Converters -------------------------------------*/
/**
 How the Converters work:
 1)  UTF-8 -> UTF-16 Big Endian
 2)  UTF-8 -> UTF-16 Little Endian
 3)  UTF-8 -> UTF-32 Big Endian
 4)  UTF-8 -> UTF-32 Little Endian

 5)  UTF-16 Big Endian -> UTF-8
 6  UTF-16 Big Endian -> UTF-16 Little Endian:    UTF-16 Big Endian -> UTF-8 -> UTF-16 Little Endian
 7  UTF-16 Big Endian -> UTF-32 Big Endian:       UTF-16 Big Endian -> UTF-8 -> UTF-32 Big Endian
 8  UTF-16 Big Endian -> UTF-32 Little Endian:    UTF-16 Big Endian -> UTF-8 -> UTF-32 Little Endian

 9)  UTF-16 Little Endian -> UTF-8
 10 UTF-16 Little Endian -> UTF-16 Big Endian:    UTF-16 Little Endian -> UTF-8 -> UTF-16 Big Endian
 11 UTF-16 Little Endian -> UTF-32 Big Endian:    UTF-16 Little Endian -> UTF-8 -> UTF-32 Big Endian
 12 UTF-16 Little Endian -> UTF-32 Little Endian: UTF-16 Little Endian -> UTF-8 -> UTF-32 Little Endian

 13) UTF-32 Big Endian -> UTF-8
 14 UTF-32 Big Endian -> UTF-16 Big Endian:       UTF-32 Big Endian -> UTF-8 -> UTF-16 Big Endian
 15 UTF-32 Big Endian -> UTF-16 Little Endian:    UTF-32 Big Endian -> UTF-8 -> UTF-16 Little Endian
 16 UTF-32 Big Endian -> UTF-32 Little Endian:    UTF-32 Big Endian -> UTF-8 -> UTF-32 Little Endian

 17) UTF-32 Little Endian -> UTF-8
 18 UTF-32 Little Endian -> UTF-16 Big Endian:    UTF-32 Little Endian -> UTF-8 -> UTF-16 Big Endian
 19 UTF-32 Little Endian -> UTF-16 Little Endian: UTF-32 Little Endian -> UTF-8 -> UTF-16 Little Endian
 20 UTF-32 Little Endian -> UTF-32 Big Endian:    UTF-32 Little Endian -> UTF-8 -> UTF-32 Big Endian

 */


void UTF8_convert_to_UTF16BE(FILE *f, FILE *fo) {
    unsigned char ch1, ch2[2], ch3[3], ch4[4];
    unsigned char result[4];
    size_t NoB;
    fprintf(fo, "%c%c", 0xFE, 0xFF);
    while (1) {
        fscanf(f, "%c", &ch1);
        if (feof(f)) {
            break;
        }
        if (ch1 <= 0x007F) {
            NoB = code_point_to_UTF16BE(result, UTF8_to_code_point(&ch1, 1));
            if (NoB == 2) {
                fprintf(fo, "%c%c", result[0], result[1]);
            } else {
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        } else {
            ch2[1] = ch1;
            fscanf(f, "%c", &ch2[0]);
            if (UTF8_to_code_point(ch2, 2) <= 0x7FF) {
                NoB = code_point_to_UTF16BE(result, UTF8_to_code_point(ch2, 2));
                if (NoB == 2) {
                    fprintf(fo, "%c%c", result[0], result[1]);
                } else {
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                }
            } else {
                ch3[2] = ch2[1];
                ch3[1] = ch2[0];
                fscanf(f, "%c", &ch3[0]);
                if (UTF8_to_code_point(ch3, 3) <= 0xFFFF) {
                    NoB = code_point_to_UTF16BE(result, UTF8_to_code_point(ch3, 3));
                    if (NoB == 2) {
                        fprintf(fo, "%c%c", result[0], result[1]);
                    } else {
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    }
                } else {
                    ch4[3] = ch3[2];
                    ch4[2] = ch3[1];
                    ch4[1] = ch3[0];
                    fscanf(f, "%c", &ch4[0]);
                    if (UTF8_to_code_point(ch4, 4) <= 0x10FFFF) {
                        NoB = code_point_to_UTF16BE(result, UTF8_to_code_point(ch4, 4));
                        if (NoB == 2) {
                            fprintf(fo, "%c%c", result[0], result[1]);
                        } else {
                            fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                        }
                    } else {
                        fprintf(fo, "%c%c", 0xFF, 0xFD);
                    }
                }
            }
        }
    }
}

void UTF8_convert_to_UTF16LE(FILE *f, FILE *fo) {
    unsigned char ch1, ch2[2], ch3[3], ch4[4];
    unsigned char result[4];
    size_t NoB;
    fprintf(fo, "%c%c", 0xFF, 0xFE);
    while (1) {
        fscanf(f, "%c", &ch1);
        if (feof(f)) {
            break;
        }
        if (ch1 <= 0x007F) {
            NoB = code_point_to_UTF16LE(result, UTF8_to_code_point(&ch1, 1));
            if (NoB == 2) {
                fprintf(fo, "%c%c", result[0], result[1]);
            } else {
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        } else {
            ch2[1] = ch1;
            fscanf(f, "%c", &ch2[0]);
            if (UTF8_to_code_point(ch2, 2) <= 0x7FF) {
                NoB = code_point_to_UTF16LE(result, UTF8_to_code_point(ch2, 2));
                if (NoB == 2) {
                    fprintf(fo, "%c%c", result[0], result[1]);
                } else {
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                }
            } else {
                ch3[2] = ch2[1];
                ch3[1] = ch2[0];
                fscanf(f, "%c", &ch3[0]);
                if (UTF8_to_code_point(ch3, 3) <= 0xFFFF) {
                    NoB = code_point_to_UTF16LE(result, UTF8_to_code_point(ch3, 3));
                    if (NoB == 2) {
                        fprintf(fo, "%c%c", result[0], result[1]);
                    } else {
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    }
                } else {
                    ch4[3] = ch3[2];
                    ch4[2] = ch3[1];
                    ch4[1] = ch3[0];
                    fscanf(f, "%c", &ch4[0]);
                    if (UTF8_to_code_point(ch4, 4) <= 0x10FFFF) {
                        NoB = code_point_to_UTF16LE(result, UTF8_to_code_point(ch4, 4));
                        if (NoB == 2) {
                            fprintf(fo, "%c%c", result[0], result[1]);
                        } else {
                            fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                        }
                    } else {
                        fprintf(fo, "%c%c", 0xFD, 0xFF);
                    }
                }
            }
        }
    }
}

void UTF8_convert_to_UTF32BE(FILE *f, FILE *fo) {
    unsigned char ch1, ch2[2], ch3[3], ch4[4];
    unsigned char result[4];
    fprintf(fo, "%c%c%c%c", 0x00, 0x00, 0xFE, 0xFF);
    while (1) {
        fscanf(f, "%c", &ch1);
        if (feof(f)) {
            break;
        }
        if (ch1 <= 0x007F) {
            code_point_to_UTF32BE(result, UTF8_to_code_point(&ch1, 1));
            fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
        } else {
            ch2[1] = ch1;
            fscanf(f, "%c", &ch2[0]);
            if (UTF8_to_code_point(ch2, 2) <= 0x7FF) {
                code_point_to_UTF32BE(result, UTF8_to_code_point(ch2, 2));
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            } else {
                ch3[2] = ch2[1];
                ch3[1] = ch2[0];
                fscanf(f, "%c", &ch3[0]);
                if (UTF8_to_code_point(ch3, 3) <= 0xFFFF) {
                    code_point_to_UTF32BE(result, UTF8_to_code_point(ch3, 3));
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                } else {
                    ch4[3] = ch3[2];
                    ch4[2] = ch3[1];
                    ch4[1] = ch3[0];
                    fscanf(f, "%c", &ch4[0]);
                    if (UTF8_to_code_point(ch4, 4) <= 0x10FFFF) {
                        code_point_to_UTF32BE(result, UTF8_to_code_point(ch4, 4));
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    } else {
                        fprintf(fo, "%c%c%c%c", 0x00, 0x00, 0xFF, 0xFD);
                    }
                }
            }
        }
    }
}

void UTF8_convert_to_UTF32LE(FILE *f, FILE *fo) {
    unsigned char ch1, ch2[2], ch3[3], ch4[4];
    unsigned char result[4];
    fprintf(fo, "%c%c%c%c", 0xFF, 0xFE, 0x00, 0x00);
    while (1) {
        fscanf(f, "%c", &ch1);
        if (feof(f)) {
            break;
        }
        if (ch1 <= 0x007F) {
            code_point_to_UTF32LE(result, UTF8_to_code_point(&ch1, 1));
            fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
        } else {
            ch2[1] = ch1;
            fscanf(f, "%c", &ch2[0]);
            if (UTF8_to_code_point(ch2, 2) <= 0x7FF) {
                code_point_to_UTF32LE(result, UTF8_to_code_point(ch2, 2));
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            } else {
                ch3[2] = ch2[1];
                ch3[1] = ch2[0];
                fscanf(f, "%c", &ch3[0]);
                if (UTF8_to_code_point(ch3, 3) <= 0xFFFF) {
                    code_point_to_UTF32LE(result, UTF8_to_code_point(ch3, 3));
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                } else {
                    ch4[3] = ch3[2];
                    ch4[2] = ch3[1];
                    ch4[1] = ch3[0];
                    fscanf(f, "%c", &ch4[0]);
                    if (UTF8_to_code_point(ch4, 4) <= 0x10FFFF) {
                        code_point_to_UTF32LE(result, UTF8_to_code_point(ch4, 4));
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    } else {
                        fprintf(fo, "%c%c%c%c", 0xFD, 0xFF, 0x00, 0x00);
                    }
                }
            }
        }
    }
}

void UTF32BE_convert_to_UTF8(FILE *f, FILE *fo, bool with) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    if (with == true) {
        fprintf(fo, "%c%c%c", 0xEF, 0xBB, 0xBF);
    }
    fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]);
        if (feof(f)) {
            break;
        }
        NoB = code_point_to_UTF8(result, UTF32BE_to_code_point(ch, 4));
        switch(NoB) {
            case 1:
                fprintf(fo, "%c", result[0]);
                break;
            case 2:
                fprintf(fo, "%c%c", result[0], result[1]);
                break;
            case 3:
                fprintf(fo, "%c%c%c", result[0], result[1], result[2]);
                break;
            case 4:
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
            default:
                code_point_to_UTF8(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
        }
    }
}

void UTF32LE_convert_to_UTF8(FILE *f, FILE *fo, bool with) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    if (with == true) {
        fprintf(fo, "%c%c%c", 0xEF, 0xBB, 0xBF);
    }
    fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]);
        if (feof(f)) {
            break;
        }
        NoB = code_point_to_UTF8(result, UTF32LE_to_code_point(ch, 4));
        switch(NoB) {
            case 1:
                fprintf(fo, "%c", result[0]);
                break;
            case 2:
                fprintf(fo, "%c%c", result[0], result[1]);
                break;
            case 3:
                fprintf(fo, "%c%c%c", result[0], result[1], result[2]);
                break;
            case 4:
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
            default:
                code_point_to_UTF8(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
        }
    }
}

void UTF16BE_convert_to_UTF8(FILE *f, FILE *fo, bool with) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    if (with == true) {
        fprintf(fo, "%c%c%c", 0xEF, 0xBB, 0xBF);
    }
    fscanf(f, "%c%c", &ch[0], &ch[1]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c", &ch[0], &ch[1]);
        if (feof(f)) {
            break;
        }
        if (UTF16BE_to_code_point(ch, 2) <= 0xD7FF) {
            NoB = code_point_to_UTF8(result, UTF16BE_to_code_point(ch, 2));
            switch (NoB) {
                case 1:
                    fprintf(fo, "%c", result[0]);
                    break;
                case 2:
                    fprintf(fo, "%c%c", result[0], result[1]);
                    break;
                case 3:
                    fprintf(fo, "%c%c%c", result[0], result[1], result[2]);
                    break;
                case 4:
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    break;
            }
        } else {
            fscanf(f, "%c%c", &ch[2], &ch[3]);
            if (UTF16BE_to_code_point(ch, 4) <= 0x10FFFF) {
                NoB = code_point_to_UTF8(result, UTF16BE_to_code_point(ch, 4));
                switch (NoB) {
                    case 1:
                        fprintf(fo, "%c", result[0]);
                        break;
                    case 2:
                        fprintf(fo, "%c%c", result[0], result[1]);
                        break;
                    case 3:
                        fprintf(fo, "%c%c%c", result[0], result[1], result[2]);
                        break;
                    case 4:
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                        break;
                }
            } else {
                code_point_to_UTF8(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        }
    }
}

void UTF16LE_convert_to_UTF8(FILE *f, FILE *fo, bool with) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    if (with == true) {
        fprintf(fo, "%c%c%c", 0xEF, 0xBB, 0xBF);
    }
    fscanf(f, "%c%c", &ch[0], &ch[1]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c", &ch[0], &ch[1]);
        if (feof(f)) {
            break;
        }
        if (UTF16LE_to_code_point(ch, 2) <= 0xD7FF) {
            NoB = code_point_to_UTF8(result, UTF16LE_to_code_point(ch, 2));
            switch (NoB) {
                case 1:
                    fprintf(fo, "%c", result[0]);
                    break;
                case 2:
                    fprintf(fo, "%c%c", result[0], result[1]);
                    break;
                case 3:
                    fprintf(fo, "%c%c%c", result[0], result[1], result[2]);
                    break;
                case 4:
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    break;
            }
        } else {
            fscanf(f, "%c%c", &ch[2], &ch[3]);
            if (UTF16LE_to_code_point(ch, 4) <= 0x10FFFF) {
                NoB = code_point_to_UTF8(result, UTF16LE_to_code_point(ch, 4));
                switch (NoB) {
                    case 1:
                        fprintf(fo, "%c", result[0]);
                        break;
                    case 2:
                        fprintf(fo, "%c%c", result[0], result[1]);
                        break;
                    case 3:
                        fprintf(fo, "%c%c%c", result[0], result[1], result[2]);
                        break;
                    case 4:
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                        break;
                }
            } else {
                code_point_to_UTF8(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        }
    }
}

void UTF16BE_convert_to_UTF16LE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fscanf(f, "%c%c", &ch[0], &ch[1]); // читаем ненужный бом
    fprintf(fo, "%c%c", 0xFF, 0xFE);
    while (1) {
        fscanf(f, "%c%c", &ch[0], &ch[1]);
        if (feof(f)) {
            break;
        }
        if (UTF16BE_to_code_point(ch, 2) <= 0xD7FF) {
            NoB = code_point_to_UTF16LE(result, UTF16BE_to_code_point(ch, 2));
            switch (NoB) {
                case 2:
                    fprintf(fo, "%c%c", result[0], result[1]);
                    break;
                case 4:
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    break;
            }
        } else {
            fscanf(f, "%c%c", &ch[2], &ch[3]);
            if (UTF16BE_to_code_point(ch, 4) <= 0x10FFFF) {
                NoB = code_point_to_UTF16LE(result, UTF16BE_to_code_point(ch, 4));
                switch (NoB) {
                    case 2:
                        fprintf(fo, "%c%c", result[0], result[1]);
                        break;
                    case 4:
                        fprintf(fo, "%c%c%c%c", ch[1], ch[0], ch[3], ch[2]);
                        break;
                }
            } else {
                code_point_to_UTF16LE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        }
    }
}

void UTF16BE_convert_to_UTF32BE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fscanf(f, "%c%c", &ch[0], &ch[1]); // читаем ненужный бом
    fprintf(fo, "%c%c%c%c", 0x00, 0x00, 0xFE, 0xFF);
    while (1) {
        fscanf(f, "%c%c", &ch[0], &ch[1]);
        if (feof(f)) {
            break;
        }
        if (UTF16BE_to_code_point(ch, 2) <= 0xD7FF) {
            NoB = code_point_to_UTF32BE(result, UTF16BE_to_code_point(ch, 2));
            switch (NoB) {
                case 2:
                    fprintf(fo, "%c%c", result[0], result[1]);
                    break;
                case 4:
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    break;
            }
        } else {
            fscanf(f, "%c%c", &ch[2], &ch[3]);
            if (UTF16BE_to_code_point(ch, 4) <= 0x10FFFF) {
                NoB = code_point_to_UTF32BE(result, UTF16BE_to_code_point(ch, 4));
                switch (NoB) {
                    case 2:
                        fprintf(fo, "%c%c", result[0], result[1]);
                        break;
                    case 4:
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                        break;
                }
            } else {
                code_point_to_UTF32BE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        }
    }
}

void UTF16BE_convert_to_UTF32LE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fscanf(f, "%c%c", &ch[0], &ch[1]); // читаем ненужный бом
    fprintf(fo, "%c%c%c%c", 0xFF, 0xFE, 0x00, 0x00);
    while (1) {
        fscanf(f, "%c%c", &ch[0], &ch[1]);
        if (feof(f)) {
            break;
        }
        if (UTF16BE_to_code_point(ch, 2) <= 0xD7FF) {
            NoB = code_point_to_UTF32LE(result, UTF16BE_to_code_point(ch, 2));
            switch (NoB) {
                case 2:
                    fprintf(fo, "%c%c", result[0], result[1]);
                    break;
                case 4:
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    break;
            }
        } else {
            fscanf(f, "%c%c", &ch[2], &ch[3]);
            if (UTF16BE_to_code_point(ch, 4) <= 0x10FFFF) {
                NoB = code_point_to_UTF32LE(result, UTF16BE_to_code_point(ch, 4));
                switch (NoB) {
                    case 2:
                        fprintf(fo, "%c%c", result[0], result[1]);
                        break;
                    case 4:
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                        break;
                }
            } else {
                code_point_to_UTF32LE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        }
    }
}

void UTF16LE_convert_to_UTF16BE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fscanf(f, "%c%c", &ch[0], &ch[1]); // читаем ненужный бом
    fprintf(fo, "%c%c", 0xFE, 0xFF);
    while (1) {
        fscanf(f, "%c%c", &ch[0], &ch[1]);
        if (feof(f)) {
            break;
        }
        if (UTF16LE_to_code_point(ch, 2) <= 0xD7FF) {
            NoB = code_point_to_UTF16BE(result, UTF16LE_to_code_point(ch, 2));
            switch (NoB) {
                case 2:
                    fprintf(fo, "%c%c", result[0], result[1]);
                    break;
                case 4:
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    break;
            }
        } else {
            fscanf(f, "%c%c", &ch[2], &ch[3]);
            if (UTF16LE_to_code_point(ch, 4) <= 0x10FFFF) {
                NoB = code_point_to_UTF16BE(result, UTF16LE_to_code_point(ch, 4));
                switch (NoB) {
                    case 2:
                        fprintf(fo, "%c%c", result[0], result[1]);
                        break;
                    case 4:
                        fprintf(fo, "%c%c%c%c", ch[1], ch[0], ch[3], ch[2]);
                        break;
                }
            } else {
                code_point_to_UTF16BE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        }
    }
}

void UTF16LE_convert_to_UTF32BE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fscanf(f, "%c%c", &ch[0], &ch[1]); // читаем ненужный бом
    fprintf(fo, "%c%c%c%c", 0x00, 0x00, 0xFE, 0xFF);
    while (1) {
        fscanf(f, "%c%c", &ch[0], &ch[1]);
        if (feof(f)) {
            break;
        }
        if (UTF16LE_to_code_point(ch, 2) <= 0xD7FF) {
            NoB = code_point_to_UTF32BE(result, UTF16LE_to_code_point(ch, 2));
            switch (NoB) {
                case 2:
                    fprintf(fo, "%c%c", result[0], result[1]);
                    break;
                case 4:
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    break;
            }
        } else {
            fscanf(f, "%c%c", &ch[2], &ch[3]);
            if (UTF16LE_to_code_point(ch, 4) <= 0x10FFFF) {
                NoB = code_point_to_UTF32BE(result, UTF16LE_to_code_point(ch, 4));
                switch (NoB) {
                    case 2:
                        fprintf(fo, "%c%c", result[0], result[1]);
                        break;
                    case 4:
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                        break;
                }
            } else {
                code_point_to_UTF32BE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        }
    }
}

void UTF16LE_convert_to_UTF32LE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fscanf(f, "%c%c", &ch[0], &ch[1]); // читаем ненужный бом
    fprintf(fo, "%c%c%c%c", 0xFF, 0xFE, 0x00, 0x00);
    while (1) {
        fscanf(f, "%c%c", &ch[0], &ch[1]);
        if (feof(f)) {
            break;
        }
        if (UTF16LE_to_code_point(ch, 2) <= 0xD7FF) {
            NoB = code_point_to_UTF32LE(result, UTF16LE_to_code_point(ch, 2));
            switch (NoB) {
                case 2:
                    fprintf(fo, "%c%c", result[0], result[1]);
                    break;
                case 4:
                    fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                    break;
            }
        } else {
            fscanf(f, "%c%c", &ch[2], &ch[3]);
            if (UTF16LE_to_code_point(ch, 4) <= 0x10FFFF) {
                NoB = code_point_to_UTF32LE(result, UTF16LE_to_code_point(ch, 4));
                switch (NoB) {
                    case 2:
                        fprintf(fo, "%c%c", result[0], result[1]);
                        break;
                    case 4:
                        fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                        break;
                }
            } else {
                code_point_to_UTF32LE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
            }
        }
    }
}

void UTF32BE_convert_to_UTF16BE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fprintf(fo, "%c%c", 0xFE, 0xFF);
    fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]);
        if (feof(f)) {
            break;
        }

        NoB = code_point_to_UTF16BE(result, UTF32BE_to_code_point(ch, 4));
        switch(NoB) {
            case 2:
                fprintf(fo, "%c%c", result[0], result[1]);
                break;
            case 4:
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
            default:
                code_point_to_UTF16BE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
        }
    }
}

void UTF32BE_convert_to_UTF16LE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fprintf(fo, "%c%c", 0xFF, 0xFE);
    fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]);
        if (feof(f)) {
            break;
        }

        NoB = code_point_to_UTF16LE(result, UTF32BE_to_code_point(ch, 4));
        switch(NoB) {
            case 2:
                fprintf(fo, "%c%c", result[0], result[1]);
                break;
            case 4:
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
            default:
                code_point_to_UTF16LE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
        }
    }
}

void UTF32BE_convert_to_UTF32LE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fprintf(fo, "%c%c%c%c", 0xFF, 0xFE, 0x00, 0x00);
    fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]);
        if (feof(f)) {
            break;
        }

        NoB = code_point_to_UTF32LE(result, UTF32BE_to_code_point(ch, 4));
        switch(NoB) {
            case 4:
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
            default:
                code_point_to_UTF32LE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
        }
    }
}

void UTF32LE_convert_to_UTF16BE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fprintf(fo, "%c%c", 0xFE, 0xFF);
    fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]);
        if (feof(f)) {
            break;
        }
        NoB = code_point_to_UTF16BE(result, UTF32LE_to_code_point(ch, 4));
        switch(NoB) {
            case 2:
                fprintf(fo, "%c%c", result[0], result[1]);
                break;
            case 4:
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
            default:
                code_point_to_UTF16BE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
        }
    }
}

void UTF32LE_convert_to_UTF16LE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fprintf(fo, "%c%c",  0xFF, 0xFE);
    fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]);
        if (feof(f)) {
            break;
        }
        NoB = code_point_to_UTF16LE(result, UTF32LE_to_code_point(ch, 4));
        switch(NoB) {
            case 2:
                fprintf(fo, "%c%c", result[0], result[1]);
                break;
            case 4:
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
            default:
                code_point_to_UTF16LE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
        }
    }
}

void UTF32LE_convert_to_UTF32BE(FILE *f, FILE *fo) {
    unsigned char ch[4];
    unsigned char result[4];
    int NoB;
    fprintf(fo, "%c%c%c%c", 0x00, 0x00, 0xFE, 0xFF);
    fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]); // читаем ненужный бом
    while (1) {
        fscanf(f, "%c%c%c%c", &ch[0], &ch[1], &ch[2], &ch[3]);
        if (feof(f)) {
            break;
        }
        NoB = code_point_to_UTF32BE(result, UTF32LE_to_code_point(ch, 4));
        switch(NoB) {
            case 4:
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
            default:
                code_point_to_UTF32BE(result, 0xFFFD);
                fprintf(fo, "%c%c%c%c", result[0], result[1], result[2], result[3]);
                break;
        }
    }
}

void UTF8_convert_to_UTF8_without_BOM(FILE *f, FILE *fo) {
    fseek(f, 3, SEEK_SET);
    char ch;
    fscanf(f, "%c", &ch);
    while(!feof(f)) {
        fprintf(fo, "%c", ch);
        fscanf(f, "%c", &ch);
    }
}

void UTF8_without_BOM_convert_to_UTF8(FILE *f, FILE *fo) {
    char ch;
    fprintf(fo, "%c%c%c", 0xEF, 0xBB, 0xBF);
    fscanf(f, "%c", &ch);
    while(!feof(f)) {
        fprintf(fo, "%c", ch);
        fscanf(f, "%c", &ch);
    }
}
/**------------------------------------- Converters -------------------------------------*/

unsigned int check_BOM(const char *name) {
    FILE *f = fopen(name, "r");
    if (f == NULL) {
        printf ("ERROR: *f = NULL -> ERROR_OPENING_FILE");
        fclose(f);
        _Exit(13);
    }
    unsigned char BOM[4];
    fread(BOM, sizeof(char), 4, f);
    if (memcmp(BOM, "\x00\x00\xFE\xFF", 4) == 0) { // UTF-32 big-endian
        fclose(f);
        return 5;
    } else if (memcmp(BOM, "\xFF\xFE\x00\x00", 4) == 0) { // UTF-32 little-endian
        fclose(f);
        return  4;
    } else if (memcmp(BOM, "\xFE\xFF", 2) == 0) { // UTF-16 big-endian
        fclose(f);
        return  3;
    } else if (memcmp(BOM, "\xFF\xFE", 2) == 0) { // UTF-16 little-endian
        fclose(f);
        return  2;
    } else if (memcmp(BOM, "\xEF\xBB\xBF", 3) == 0) { // UTF-8 with BOM
        fclose(f);
        return 1;
    } else { // UTF-8 without BOM
        fclose(f);
        return  0;
    }
}

/**
* 0 – UTF-8 без BOM;
* 1 – UTF-8 с BOM;
* 2 – UTF-16 Little Endian;
* 3 – UTF-16 Big Endian;
* 4 – UTF-32 Little Endian;
* 5 – UTF-32 Big Endian.
 */

int main(int argc, char **argv) {
    if (argc != 4) {
        perror("ERROR: No args");
        return 11;
    }

    int outUTF;
    sscanf(argv[3], "%u", &outUTF);
    unsigned int inUTF = check_BOM(argv[1]);

    FILE *fi = fopen(argv[1], "rb");
    if (fi == NULL) {
        printf("Error: the input file cannot be opened.");
        return 13;
    }

    FILE *fo;
    fo = fopen(argv[2], "wb");
    if (fo == NULL) {
        printf("Error: the output file cannot be opened.");
        fclose(fi);
        return 13;
    }

    switch(inUTF) {
        case 0:
            switch (outUTF) {
                case 1:
                    UTF8_without_BOM_convert_to_UTF8(fi, fo);
                    break;
                case 2:
                    UTF8_convert_to_UTF16LE(fi, fo);
                    break;
                case 3:
                    UTF8_convert_to_UTF16BE(fi, fo);
                    break;
                case 4:
                    UTF8_convert_to_UTF32LE(fi, fo);
                    break;
                case 5:
                    UTF8_convert_to_UTF32BE(fi, fo);
                    break;
            }
            break;
        case 1:
            switch (outUTF) {
                case 0:
                    UTF8_convert_to_UTF8_without_BOM(fi, fo);
                    break;
                case 2:
                    fseek(fi, 3, SEEK_SET);
                    UTF8_convert_to_UTF16LE(fi, fo);
                    break;
                case 3:
                    fseek(fi, 3, SEEK_SET);
                    UTF8_convert_to_UTF16BE(fi, fo);
                    break;
                case 4:
                    fseek(fi, 3, SEEK_SET);
                    UTF8_convert_to_UTF32LE(fi, fo);
                    break;
                case 5:
                    fseek(fi, 3, SEEK_SET);
                    UTF8_convert_to_UTF32BE(fi, fo);
                    break;
            }
            break;
        case 2:
            switch (outUTF) {
                case 0:
                    UTF16LE_convert_to_UTF8(fi, fo, false);
                    break;
                case 1:
                    UTF16LE_convert_to_UTF8(fi, fo, true);
                    break;
                case 3:
                    UTF16LE_convert_to_UTF16BE(fi, fo);
                    break;
                case 4:
                    UTF16LE_convert_to_UTF32LE(fi, fo);
                    break;
                case 5:
                    UTF16LE_convert_to_UTF32BE(fi, fo);
                    break;
            }
            break;
        case 3:
            switch (outUTF) {
                case 0:
                    UTF16BE_convert_to_UTF8(fi, fo, false);
                    break;
                case 1:
                    UTF16BE_convert_to_UTF8(fi, fo, true);
                    break;
                case 2:
                    UTF16BE_convert_to_UTF16LE(fi, fo);
                    break;
                case 4:
                    UTF16BE_convert_to_UTF32LE(fi, fo);
                    break;
                case 5:
                    UTF16BE_convert_to_UTF32BE(fi, fo);
                    break;
            }
            break;
        case 4:
            switch (outUTF) {
                case 0:
                    UTF32LE_convert_to_UTF8(fi, fo, false);
                    break;
                case 1:
                    UTF32LE_convert_to_UTF8(fi, fo, true);
                    break;
                case 2:
                    UTF32LE_convert_to_UTF16LE(fi, fo);
                    break;
                case 3:
                    UTF32LE_convert_to_UTF16BE(fi, fo);
                    break;
                case 5:
                    UTF32LE_convert_to_UTF32BE(fi, fo);
                    break;
            }
            break;
        case 5:
            switch (outUTF) {
                case 0:
                    UTF32BE_convert_to_UTF8(fi, fo, false);
                    break;
                case 1:
                    UTF32BE_convert_to_UTF8(fi, fo, true);
                    break;
                case 2:
                    UTF32BE_convert_to_UTF16LE(fi, fo);
                    break;
                case 3:
                    UTF32BE_convert_to_UTF16BE(fi, fo);
                    break;
                case 4:
                    UTF32BE_convert_to_UTF32LE(fi, fo);
                    break;
            }
            break;
    }

    fclose(fi);
    fclose(fo);
    return 0;
}