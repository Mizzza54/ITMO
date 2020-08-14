#include <vector>
#include <string>
#include <ostream>
#include <iomanip>
#include <iostream>
#include <fstream>

#define DataTypeForIndex long long
#define DataTypeForDigits int
#define NODPAE 9 /// Number of digits per array element

class BigInteger {
    static const int Base = 1000000000;  /// Основание системы счисления (1 000 000 000)
    bool isNegative{};                   /// Знак числа: true - отрицательное, false - положительное
    void RemoveLeadingZeros();           /// Убирает незначимые нули
    void RightShift();                   /// Сдвиг вправо - вспомогательная функция для деления

public:
    std::vector<DataTypeForDigits> Digits;   ///  Вектор для хранения числа

    BigInteger();
    explicit BigInteger(std::string);        ///  Каст string к BigInteger

    template <typename T,
            typename GENERIC =
            typename std::enable_if<std::is_same<T, unsigned int>::value ||
                                    std::is_same<T, signed int>::value ||
                                    std::is_same<T, unsigned long >::value ||
                                    std::is_same<T, signed long>::value ||
                                    std::is_same<T, unsigned long long>::value ||
                                    std::is_same<T, signed long long>::value>::type>

    BigInteger(T value) {
        if (value < 0) {
            this->isNegative = true;
            value = -value;
        }
        else {
            this->isNegative = false;
        }
        do {
            this->Digits.push_back(value % BigInteger::Base);
            value /= BigInteger::Base;
        } while (value != 0);
    }

    BigInteger operator +() const;                                       /// Унарный плюс
    BigInteger operator -() const;                                       /// Унарный минус
    friend bool operator ==(const BigInteger&, const BigInteger&);       /// Равно
    friend bool operator !=(const BigInteger&, const BigInteger&);       /// Не равно
    friend bool operator <(const BigInteger&, const BigInteger&);        /// Строго меньше
    friend bool operator <=(const BigInteger&, const BigInteger&);       /// Меньше или равно
    friend bool operator >(const BigInteger&, const BigInteger&);        /// Строго больше
    friend bool operator >=(const BigInteger&, const BigInteger&);       /// Больше или равно
    friend BigInteger operator +(BigInteger, const BigInteger&);         /// Сложение
    friend BigInteger operator -(BigInteger, const BigInteger&);         /// Вычитание
    friend BigInteger operator *(const BigInteger&, const BigInteger&);  /// Умножение
    friend BigInteger operator /(const BigInteger&, const BigInteger&);  /// Целочисленное деление
    friend BigInteger operator %(const BigInteger&, const BigInteger&);  /// Остаток от деления
    friend BigInteger sqrt(const BigInteger&);                           /// Квадратный корень
    friend std::ostream& operator <<(std::ostream&, const BigInteger&);
};

BigInteger::BigInteger() {
    this->isNegative = false;
}

BigInteger BigInteger::operator +() const {
    return BigInteger(*this);
}

BigInteger BigInteger::operator -() const {
    BigInteger copy(*this);
    copy.isNegative = !copy.isNegative;
    return copy;
}

BigInteger::BigInteger(std::string str) {
    if (str[0] == 'N' && str[1] == 'a' && str[2] == 'N' && str[3] == '\0') {
        this->isNegative = true;
        this->Digits.push_back(-1);
    } else if (str.length() == 0) {
        this->isNegative = false;
    } else {
        if (str[0] == '-') {
            str = str.substr(1);
            this->isNegative = true;
        } else {
            this->isNegative = false;
        }

        for (DataTypeForIndex i = str.length(); i > 0; i -= NODPAE) {
            if (i < NODPAE) {
                this->Digits.push_back(atoi(str.substr(0, i).c_str()));
                break;
            } else {
                this->Digits.push_back(atoi(str.substr(i - NODPAE, NODPAE).c_str()));
            }
        }

        this->RemoveLeadingZeros();
    }
}

void BigInteger::RemoveLeadingZeros() {
    while (this->Digits.size() > 1 && this->Digits.back() == 0) {
        this->Digits.pop_back();
    }

    if (this->Digits.size() == 1 && this->Digits[0] == 0) {
        this->isNegative = false;
    }
}

bool operator ==(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    if (LeftNumber.isNegative != RightNumber.isNegative) {
        return false;
    }

    if (LeftNumber.Digits.size() != RightNumber.Digits.size()) {
        return false;
    }

    if (LeftNumber.Digits.empty()) {
        return RightNumber.Digits.empty() || (RightNumber.Digits.size() == 1 && RightNumber.Digits[0] == 0);
    }

    if (RightNumber.Digits.empty()) {
        return LeftNumber.Digits.size() == 1 && LeftNumber.Digits[0] == 0;
    }

    for (DataTypeForIndex i = 0; i < LeftNumber.Digits.size(); i++) {
        if (LeftNumber.Digits[i] != RightNumber.Digits[i]) {
            return false;
        }
    }

    return true;
}

bool operator <(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    if (LeftNumber == RightNumber) {
        return false;
    }

    if (LeftNumber.isNegative) {
        if (RightNumber.isNegative) {
            return ((-RightNumber) < (-LeftNumber));
        } else {
            return true;
        }
    } else if (RightNumber.isNegative) {
        return false;
    } else {
        if (LeftNumber.Digits.size() != RightNumber.Digits.size()) {
            return LeftNumber.Digits.size() < RightNumber.Digits.size();
        } else {
            for (DataTypeForIndex i = LeftNumber.Digits.size() - 1; i >= 0; i--) {
                if (LeftNumber.Digits[i] != RightNumber.Digits[i]) {
                    return LeftNumber.Digits[i] < RightNumber.Digits[i];
                }
            }

            return false;
        }
    }
}

bool operator !=(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    return !(LeftNumber == RightNumber);
}

bool operator <=(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    return (LeftNumber < RightNumber || LeftNumber == RightNumber);
}

bool operator >(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    return !(LeftNumber <= RightNumber);
}

bool operator >=(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    return !(LeftNumber < RightNumber);
}

BigInteger operator +(BigInteger LeftNumber, const BigInteger& RightNumber) {
    if (LeftNumber.isNegative) {
        if (RightNumber.isNegative) {
            return -(-LeftNumber + (-RightNumber));
        } else {
            return RightNumber - (-LeftNumber);
        }
    } else if (RightNumber.isNegative) {
        return LeftNumber - (-RightNumber);
    }

    DataTypeForDigits Carry = 0;
    for (DataTypeForIndex i = 0; i < std::max(LeftNumber.Digits.size(), RightNumber.Digits.size()) || Carry != 0; i++) {
        if (i == LeftNumber.Digits.size()) {
            LeftNumber.Digits.push_back(0);
        }
        LeftNumber.Digits[i] += Carry + (i < RightNumber.Digits.size() ? RightNumber.Digits[i] : 0);
        Carry = LeftNumber.Digits[i] >= BigInteger::Base ? 1 : 0;
        if (Carry != 0) {
            LeftNumber.Digits[i] -= BigInteger::Base;
        }
    }

    return LeftNumber;
}


BigInteger operator -(BigInteger LeftNumber, const BigInteger& RightNumber) {
    if (RightNumber.isNegative) {
        return LeftNumber + (-RightNumber);
    } else if (LeftNumber.isNegative) {
        return -(-LeftNumber + RightNumber);
    } else if (LeftNumber < RightNumber) {
        return -(RightNumber - LeftNumber);
    }

    DataTypeForDigits Carry = 0;
    for (DataTypeForIndex i = 0; i < RightNumber.Digits.size() || Carry != 0; i++) {
        LeftNumber.Digits[i] -= Carry + (i < RightNumber.Digits.size() ? RightNumber.Digits[i] : 0);
        Carry = LeftNumber.Digits[i] < 0 ? 1 : 0;
        if (Carry != 0) {
            LeftNumber.Digits[i] += BigInteger::Base;
        }
    }

    LeftNumber.RemoveLeadingZeros();
    return LeftNumber;
}

BigInteger operator *(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    BigInteger Result;
    Result.Digits.resize(LeftNumber.Digits.size() + RightNumber.Digits.size());
    for (DataTypeForIndex i = 0; i < LeftNumber.Digits.size(); i++) {
        DataTypeForDigits Carry = 0;
        for (DataTypeForIndex j = 0; j < RightNumber.Digits.size() || Carry != 0; j++) {
            long long Current = Result.Digits[i + j] + LeftNumber.Digits[i] * 1LL * (j < RightNumber.Digits.size() ? RightNumber.Digits[j] : 0) + Carry;
            Result.Digits[i + j] = static_cast<DataTypeForDigits> (Current % BigInteger::Base);
            Carry = static_cast<DataTypeForDigits> (Current / BigInteger::Base);
        }
    }

    Result.isNegative = LeftNumber.isNegative != RightNumber.isNegative;
    Result.RemoveLeadingZeros();
    return Result;
}

void BigInteger::RightShift() {
    if (this->Digits.empty()) {
        this->Digits.push_back(0);
        return;
    }
    this->Digits.push_back(this->Digits.back());
    for (DataTypeForIndex i = this->Digits.size() - 2; i > 0; i--) {
        this->Digits[i] = this->Digits[i - 1];
    }
    this->Digits[0] = 0;
}

BigInteger operator /(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    if (RightNumber == 0) {
        BigInteger Result("NaN");
        return Result;
    }

    BigInteger CopyOfRightNumber = RightNumber;
    CopyOfRightNumber.isNegative = false;
    BigInteger Result, Current;
    Result.Digits.resize(LeftNumber.Digits.size());
    for (DataTypeForIndex i = static_cast<DataTypeForIndex> (LeftNumber.Digits.size()) - 1; i >= 0; i--) {
        Current.RightShift();
        Current.Digits[0] = LeftNumber.Digits[i];
        Current.RemoveLeadingZeros();
        DataTypeForIndex x = 0, l = 0, r = BigInteger::Base;
        while (l <= r) {
            DataTypeForIndex m = (l + r) / 2;
            BigInteger t = CopyOfRightNumber * m;
            if (t <= Current) {
                x = m;
                l = m + 1;
            } else {
                r = m - 1;
            }
        }

        Result.Digits[i] = x;
        Current = Current - CopyOfRightNumber * x;
    }

    Result.isNegative = LeftNumber.isNegative != RightNumber.isNegative;
    Result.RemoveLeadingZeros();
    return Result;
}

BigInteger operator %(const BigInteger& LeftNumber, const BigInteger& RightNumber) {
    BigInteger Result = LeftNumber - (LeftNumber / RightNumber) * RightNumber;
    if (Result.isNegative) {
        Result = Result + RightNumber;
    }
    return Result;
}

BigInteger sqrt(const BigInteger& Number) {
    if (Number.isNegative) {
        BigInteger Result("NaN");
        return Result;
    }
    BigInteger Result = Number, Partition = (Number + 1) / 2;
    while (Partition < Result) {
        Result = Partition;
        Partition = (Partition + Number / Partition) / 2;
    }
    return Result;
}

std::ostream& operator <<(std::ostream& stream, const BigInteger& Number) {
    if (Number.isNegative && !Number.Digits.empty() && Number.Digits[0] == -1) {
        stream << "NaN";
    } else if (Number.Digits.empty()) {
        stream << 0;
    } else {
        if (Number.isNegative) {
            stream << '-';
        }
        stream << Number.Digits.back();
        char old_fill = stream.fill('0');
        for (long long i = static_cast<long long> (Number.Digits.size()) - 2; i >= 0; i--) {
            stream << std::setw(NODPAE) << Number.Digits[i];
        }
        stream.fill(old_fill);
    }

    return stream;
}


int main(int argc, char **argv) {
    if (argc != 3) {
        std::cout << "Error: wrong number of arguments." << std::endl;
        return 11;
    }

    std::ifstream in(argv[1]);
    if (!in.is_open()) {
        std::cout << "Error: the input file cannot be opened." << std::endl;
        in.close();
        return 13;
    }

    std::ofstream out(argv[2]);
    if (!out.is_open()) {
        std::cout << "Error: the output file cannot be opened." << std::endl;
        in.close();
        out.close();
        return 13;
    }

    std::string str1, str2, operation;
    in >> str1 >> operation;
    BigInteger LeftNumber(str1);
    if (operation != "#") {  /// ‘#’ – корень квадратный
        in >> str2;
        BigInteger RightNumber(str2);
        switch(operation[0]) {
            case '+':  /// ‘+’ - сложение
                out << LeftNumber + RightNumber;
                break;
            case '-':  /// ‘-’ - вычитание
                out << LeftNumber - RightNumber;
                break;
            case '*':  /// ‘*’ - умножение
                out << LeftNumber * RightNumber;
                break;
            case '/':  /// ‘/’ - деление
                out << LeftNumber / RightNumber;
                break;
            case '%':  /// ‘%’ - остаток от деления
                out << LeftNumber % RightNumber;
                break;
            case '<':  /// ‘<’, ‘<=’ - меньше или строго меньше
                if (operation.length() == 1) {
                    if (LeftNumber < RightNumber) {
                        out << 1;
                    } else {
                        out << 0;
                    }
                } else {
                    if (LeftNumber <= RightNumber) {
                        out << 1;
                    } else {
                        out << 0;
                    }
                }
                break;
            case '>':  /// ‘>’, ‘>=’ - больше или строго больше
                if (operation.length() == 1) {
                    if (LeftNumber > RightNumber) {
                        out << 1;
                    } else {
                        out << 0;
                    }
                } else {
                    if (LeftNumber >= RightNumber) {
                        out << 1;
                    } else {
                        out << 0;
                    }
                }
                break;
            case '=':  /// ‘==’ - равно
                if (LeftNumber == RightNumber) {
                    out << 1;
                } else {
                    out << 0;
                }
                break;
            case '!':  /// ‘!=’ - не равно
                if (LeftNumber != RightNumber) {
                    out << 1;
                } else {
                    out << 0;
                }
                break;
            default:
                std::cout << "Error: Wrong operation.";
                in.close();
                out.close();
                return 15;
        }
    } else {
        out << sqrt(LeftNumber);
    }

    in.close();
    out.close();
    return 0;
}