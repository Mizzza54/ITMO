#include <stdio.h>
#include <string.h>
void lection() {
    // CP 1251 - Кодировка Windows(ANSI)
    // CP 866 - Альтернати́вная кодиро́вка» — основанная на CP437 кодовая страница, где все специфические европейские
    // символы во второй половине заменены на кириллицу, а псевдографические символы оставлены нетронутыми.
    // КОИ - 8 - кодировка для электронной почты
    // DBCS (Double Byte Character Set) — набор двухбайтовых символов. CP 932
    // Unicode - стандарт кодирования символов, включающий в себя знаки почти всех письменных языков мира.
    // Создавалась как универсальная кодировка, но первая версия не поддержавала все кроме европейских символов.
    // Далее Unicode делится на несколько видов: 1)UCS-2  2)UCS-4  3)UTF-16  4)UTF-8
    // UTF-16 -> кодировка с переменной длиной, либо 2 либо 4 байта.
    // UTF-8 -> один символ занимает от 1 до 4 байт
    // BOM(Byte Order Mark) — специальный символ Юникод, вставляемый в начало файла для обозначения того, что в файле
    // используется Юникод, а также для косвенного указания кодировки и порядка байтов, с помощью которых символы Юникода были закодированы.
    // BOM символы не нужны в кодировке Unicode UTF-8
    // Плюсы UTF-8: 1)не нужны кодовые таблицы, трансляций символов и всех прочего, что были ранее с однобайтовыми кодировками;
    // 2)Не нужны BOMы -> легче декодировать, вроде как меньше ошибок;
    // В языке C/С++ строка - массива символов, которая заканчивается нулевым символом
}

size_t len1(const char *p) {
    size_t len = 0;
    while (p[len] != '\0') {
        len++;
    }
    return len;
}

size_t len2(const char *p) {
    size_t len = 0;
    for (len = 0; p[len]; len++);
    return len;
}

size_t len3(const char *p) {
    const char *p2;
    for (p2 = p; *p2; p2++);
    return p2 - p;
}

int main() {
    char ch[20];
    scanf("%10s", ch); // Ограничение ввода до 10 символов.
    printf("%d\n", len1(ch));
    printf("%d\n", len2(ch));
    printf("%d\n", len3(ch));
    // Итог: С/С++ могут сломать память. Выделять память через команды. Если этого не длеать можно стереть нужную нам память,
    // а там могут находится важные файлы, например системные.
    int a[10];
    int b[10];
    // b = a -> так делать нельзя. Массивы это специализированные указатели которые похожи на int *const p.
    // Мы не можем менять константный указатель. Массивы намертво привязаны к памяти.
    for (size_t i = 0; i < 10; i++) {
        b[i] = a[i];
    }
    memcpy(b, a, 10 * sizeof(*a)); // Скопировать в массив b массив a
    return 0;
}
