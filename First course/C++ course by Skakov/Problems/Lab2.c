/**
 * @author Michale Gerasimov
 * start: 03.03.2020
 * @version 05.04.2020
 */

#include <stdio.h>
#include <string.h>
#include <stdatomic.h>
#include <stdlib.h>
#define MAX 2048

struct Human {
    char name[21], surname[21], middle_name[21];
    unsigned long long phone_number;
};

int cmp(const struct Human *a, const struct Human *b) {
    int temp = strcmp(a -> surname, b -> surname);
    if (temp == 0) {
        temp = strcmp(a -> name, b -> name);
        if (temp == 0) {
            temp = strcmp(a -> middle_name, b -> middle_name);
            if (temp == 0) {
                if (a ->phone_number > b -> phone_number) {
                    temp = 100;
                } else if (a ->phone_number < b -> phone_number) {
                    temp = -100;
                } else {
                    temp = 0;
                }
            }
        }
    }
    return temp;
}

void swap(struct Human *a, struct Human *b){
    struct Human save = *a;
    *a = *b;
    *b = save;
}

int Partition (struct Human *a, int left, int right, int *new_left, int *new_right) {
    int partition_point = (right + left) / 2;
    struct Human mid = a[partition_point];
    while (left <= right) {
        while (cmp(&mid, &a[left]) > 0) {
            left++;
        }
        while (cmp(&a[right], &mid) > 0) {
            right--;
        }
        if (left <= right) {
            swap(&a[right], &a[left]);
            left++;
            right--;
        }
    }
    *new_left = left;
    *new_right = right;
    return partition_point;
}

void Sort(struct Human *a, int size) {
    int stack_left[MAX], stack_right[MAX];
    int current_position = 0;
    stack_left[0] = 0;
    stack_right[0] = size - 1;
    while (current_position >= 0) {
        int left = stack_left[current_position];
        int right = stack_right[current_position];
        current_position--;
        while (left < right) {
            int new_left, new_right, *pi = &new_left, *pj = &new_right;
            int partition_point = Partition(a, left, right, pi, pj);
            if (new_left < partition_point) {
                if (new_left < right) {
                    current_position++;
                    stack_left[current_position] = new_left;
                    stack_right[current_position] = right;
                }
                right = new_right;
            } else {
                if (new_right > left) {
                    current_position++;
                    stack_left[current_position] = left;
                    stack_right[current_position] = new_right;
                }
                left = new_left;
            }
        }
    }
}


int main(int argc, char **argv) {
    if (argc != 3) {
        perror("ERROR: No args");
        return 11;
    }

    int capacity = 100;
    struct Human *Humans = (struct Human*) malloc(capacity * sizeof(struct Human));
    if (Humans == NULL) {
        printf("ERROR: *Humans = NULL -> MEM_OUT");
        return 6;
    }

    FILE *fi = fopen(argv[1], "r");
    if (fi == NULL) {
        printf ("ERROR: *fi = NULL -> ERROR_OPENING_FILE");
        return 13;
    }
    int len = 0;
    while(!feof(fi)) {
        fscanf(fi, "%s %s %s %lld\n", Humans[len].surname, Humans[len].name, Humans[len].middle_name, &Humans[len].phone_number);
        len++;
        if (len + 1 == capacity) {
            capacity *= 2;
            Humans = (struct Human*) realloc(Humans, capacity * sizeof(struct Human));
            if (Humans == NULL) {
                printf("ERROR: *Humans = NULL -> MEM_OUT");
                return 6;
            }
        }
    }
    fclose(fi);

    Sort(Humans, len);

    FILE *fo = fopen(argv[2], "w");
    if (fo == NULL) {
        printf ("ERROR: *fo = NULL -> ERROR_OPENING_FILE");
        return 13;
    }
    for (int i = 0; i < len; i++) {
        fprintf(fo, "%s %s %s %lld\n", Humans[i].surname, Humans[i].name, Humans[i].middle_name, Humans[i].phone_number);
    }
    fclose(fo);
    free(Humans);
    return 0;
}
