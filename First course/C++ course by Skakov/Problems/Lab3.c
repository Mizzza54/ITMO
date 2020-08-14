#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ZERO 0.0
#define ONE 1.0
#define MAXVALUE 1e+32
#define MINVALUE -1e+32
#define FLT_EPSILON 1.192092896e-06

/**
 * Простите за глобальные переменные 😢
 */
double MaxValue = MINVALUE;
double MinValue = MAXVALUE;

/**
 * float - single precision (32 бита) IEEE-754
 * 1 бит - под знак, 8 бит - под экспоненту, 23 бита — под мантиссу
 * Абсолютная точность у типа данных float равна 7 или более точно FLT_EPSILON = 1.192092896e-07
 * Данная функция проверяет ранвы ли два числа, у которых тип данных double
 * @param double LeftNumber - первое число, double RightNumber - второе число, double Epsilon - точность
 * @return 1 - числа равны 0 - числа неравны
 */
int EqualsForFloatNumbers (double LeftNumber, double RightNumber, double Epsilon) {
    double diff = fabs(LeftNumber - RightNumber);
    //LeftNumber = fabs(LeftNumber);
    //RightNumber = fabs(RightNumber);

    //double Largest = (RightNumber > LeftNumber) ? RightNumber : LeftNumber;

    if (diff <= Epsilon * 16 * (MaxValue - MinValue)) {
        return 1; // true
    } else {
        return 0; // false
    }

}

/**
 * Данная функция решает СЛАУ методом Гаусса
 * Рассматривается следующее выражение:
 * A * X = Y, где A - матрица коэффициентов, X - матрица решений, Y - матрица свободных членов
 * @param int n - размер СЛАУ, double A[n][n] - двумерный массив коэффициентов размера n на n,
 * double Y[n] - массив свободных членов размера n, double *X - указатель на массив решений.
 * @return int Code - 0 (Решения есть и оно единственно), 1 (“many solutions” решение не единственно), 2 (“no solution” решения отсутствуют)
 */
int SolvingLinearSystemByTheGaussMethod(int n, double **A, double *Y, double *X) {
    for (int j = 0; j < n; j++) {
        /**
         * Поиск максимально по модулю элемента
         */
        double CurrentMaxElement = fabs(A[j][j]);
        int CurrentRowOfMaxElement = j;
        for (int i = j; i < n; i++) {
            if (fabs(A[i][j]) > CurrentMaxElement) {
                CurrentMaxElement = fabs(A[i][j]);
                CurrentRowOfMaxElement = i;
            }
        }

        /**
         * Меняем j-ую строку и строку с максимальным элементом местами
         */
        if (j != CurrentRowOfMaxElement) {
            double tmp;
            for (int i = 0; i < n; i++) {
                tmp = A[j][i];
                A[j][i] = A[CurrentRowOfMaxElement][i];
                A[CurrentRowOfMaxElement][i] = tmp;
            }
            tmp = Y[j];
            Y[j] = Y[CurrentRowOfMaxElement];
            Y[CurrentRowOfMaxElement] = tmp;
        }

        /**
         * Нормурием строки и вычитаем
         */
        for (int i = j; i < n; i++) {
            if (EqualsForFloatNumbers(A[i][j], ONE, FLT_EPSILON) == 0) {  // Если A[i][j] сравнима с 1.0 по epsilon то нормировать не будем
                if (EqualsForFloatNumbers(A[i][j], ZERO, FLT_EPSILON) == 1) {  // Если A[i][j] сравнима с 0.0 то пропускаем, так как на ноль делить нельзя
                    A[i][j] = ZERO;
                    continue;
                }

                double DivideByElement = A[i][j];

                for (int col = 0; col < n; col++) {
                    if (EqualsForFloatNumbers(A[i][col], ZERO, FLT_EPSILON) == 1) {  // Если A[i][col] сравнима с 0.0 по epsilon то присваиваем A[i][col] значение 0.0
                        A[i][col] = ZERO;                                            // Делаем мы это для того чтобы: 1) маленькое число, сравнимое по epsilon с 0.0
                    }                                                                // не превратилось в большее число которое не будет сравнимо с нулем
                                                                                     // 2) ноль делить на что-то должен получится ноль
                    A[i][col] = A[i][col] / DivideByElement;

                    if (EqualsForFloatNumbers(A[i][col], ZERO, FLT_EPSILON) == 1) {
                        A[i][col] = ZERO;
                    }
                    if (EqualsForFloatNumbers(A[i][col], ONE, FLT_EPSILON) == 1) {
                        A[i][col] = ONE;
                    }
                }

                if (EqualsForFloatNumbers(Y[i], ZERO, FLT_EPSILON) == 1) {
                    Y[i] = ZERO;
                }

                Y[i] = Y[i] / DivideByElement;

                if (EqualsForFloatNumbers(Y[i], ZERO, FLT_EPSILON) == 1) {
                    Y[i] = ZERO;
                }
                if (EqualsForFloatNumbers(Y[i], ONE, FLT_EPSILON) == 1) {
                    Y[i] = ONE;
                }
            }

            if (i == j) {
                continue;
            }

            for (int col = 0; col < n; col++) {
                if (EqualsForFloatNumbers(A[i][col], A[j][col], FLT_EPSILON) == 1) {
                    A[i][col] = ZERO;
                } else {
                    A[i][col] = A[i][col] - A[j][col];
                    if (EqualsForFloatNumbers(A[i][col], ZERO, FLT_EPSILON) == 1) {
                        A[i][col] = ZERO;
                    }
                }
            }
            if (EqualsForFloatNumbers(Y[i], Y[j], FLT_EPSILON) == 1) {
                Y[i] = ZERO;
            } else {
                Y[i] = Y[i] - Y[j];
                if (EqualsForFloatNumbers(Y[i], Y[j], FLT_EPSILON) == 1) {
                    Y[i] = ZERO;
                }
            }

        }
    }

    /**
     * Записывем ответ в матрицу X
     */
    for (int j = n - 1; j >= 0; j--) {
        X[j] = Y[j];
        for (int i = 0; i < j; i++) {
            Y[i] = Y[i] - A[i][j] * X[j];
        }
    }

    /**
     * Сравниваем ранг матрицы и ранг расширенной матрицы
     */
    int RangOfMatrix = 0;
    int RangOfAdvancedMatrix = 0;
    int CountForMatrix;
    int CountForAdvancedMatrix;
    for (int i = n - 1; i >= 0; i--) {
        CountForMatrix = 0;
        CountForAdvancedMatrix = 0;
        for (int j = 0; j < n + 1; j++) {
            if (j == n && EqualsForFloatNumbers(Y[i], ZERO, FLT_EPSILON) == 0) {
                CountForAdvancedMatrix++;
            } else if (EqualsForFloatNumbers(A[i][j], ZERO, FLT_EPSILON) == 0 && j != n) {
                CountForMatrix++;
                CountForAdvancedMatrix++;
            }
        }

        if (CountForMatrix != 0) {
            RangOfMatrix++;
        }
        if (CountForAdvancedMatrix != 0) {
            RangOfAdvancedMatrix++;
        }
    }

    if (RangOfMatrix != RangOfAdvancedMatrix) {
        return 2;
    } else  if (RangOfMatrix == RangOfAdvancedMatrix && RangOfMatrix < n) {
        return 1;
    } else {
        return 0;
    }
}

int main(int argc, char **argv) {
    //  😷🍒🤕
    if (argc != 3) {
        printf("Error: wrong number of arguments.");
        return 11;
    }

    FILE *in = fopen(argv[1], "r");
    if (in == NULL) {
        printf("Error: the input file cannot be opened.");
        fclose(in);
        return 13;
    }

    FILE *out = fopen(argv[2], "w");
    if (out == NULL) {
        printf("Error: the output file cannot be opened.");
        fclose(in);
        fclose(out);
        return 13;
    }

    double val1 = 0.2; // 2/10 = 1/5
    double val2 = 1 / sqrt(5) / sqrt(5); // 1/5
    printf("val1 = %.32lf\nval2 = %.32lf", val1, val2);

    int n;
    fscanf(in, "%d", &n);
    if (n <= 0) {
        printf("Error: the size of the linear system can not be negative");
        fclose(in);
        fclose(out);
        return 15;
    }

    double **A = malloc(sizeof(double*) * n);
    if (A == NULL) {
        printf("Error: can't reallocate memory");
        fclose(in);
        fclose(out);
        return 17;
    }
    for (int i = 0; i < n; i++) {
        A[i] = malloc(sizeof(double) * n);
        if (A[i] == NULL) {
            printf("Error: can't reallocate memory");
            for (int ii = 0; ii < i; ii++) {
                free(A[i]);
            }
            free(A);
            fclose(in);
            fclose(out);
            return 17;
        }
    }

    double *Y = malloc(n * sizeof(double));
    double *X = malloc(n * sizeof(double));
    if (Y == NULL || X == NULL) {
        printf("Error: can't reallocate memory");
        for (int i = 0; i < n; i++) {
            free(A[i]);
        }
        free(A);
        free(Y);
        free(X);
        fclose(in);
        fclose(out);
        return 17;
    }

    for (int i = 0; i < n; i++ ) {
        for (int j = 0; j < n; j++) {
            fscanf(in, "%lf", &A[i][j]);
            if (A[i][j] > MaxValue) {
                MaxValue = A[i][j];
            }
            if (A[i][j] < MinValue) {
                MinValue = A[i][j];
            }
        }
        fscanf(in, "%lf", &Y[i]);
        if (Y[i] > MaxValue) {
            MaxValue = Y[i];
        }
        if (Y[i] < MinValue) {
            MinValue = Y[i];
        }
    }

    int Code = SolvingLinearSystemByTheGaussMethod(n, A, Y, X);

    switch (Code) {
        case 0:
            for (int i = 0; i < n; i++) {
                fprintf(out, "%lf \n", X[i]);
            }
            break;
        case 1:
            fprintf(out, "many solutions \n");
            break;
        case 2:
            fprintf(out, "no solution \n");
            break;
        default:
            printf("Error: unknown error.");
            for (int i = 0; i < n; i++) {
                free(A[i]);
            }
            free(A);
            free(Y);
            free(X);
            fclose(in);
            fclose(out);
            return 15;
    }

    for (int i = 0; i < n; i++) {
        free(A[i]);
    }
    free(A);
    free(Y);
    free(X);
    fclose(in);
    fclose(out);
    return 0;
}
