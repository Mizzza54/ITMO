#include <cstdio>
#include <cmath>
/**
 * @author Michale Gerasimov
 * start: 11.05.2020
 * @version -
 */

int main() {
    int n;
    scanf("%d", &n);
    n++;
    int m = (int) (log(n)/log(2)) + 1;
    int dp[n][m];

    dp[0][0] = 0;
    for (int i = 1; i < n; i++) {
        scanf("%d", &dp[i][0]);
    }

    for (int j = 1; j < m; j++) {
        for (int i = 0; i < n; i++){
            dp[i][j] = dp[dp[i][j - 1]][j - 1];
        }
    }

    for (int i = 1; i < n; i++) {
        printf("%d: ", i);
        for (int j = 0; j < m; j++) {
            if (dp[i][j] == 0) {
                break;
            }
            printf("%d ", dp[i][j]);
        }
        printf("\n");
    }

    return 0;
}