from pprint import pprint


def dp(max_weight, items):
    dp = [[0]*(max_weight+1) for _ in range(len(items)+1)]
    for i in xrange(0, len(items)):
        for j in xrange(1, max_weight+1):
            cnt = 0
            while cnt*items[i][0] <= j:
                w = cnt*items[i][0]
                price = cnt*items[i][1]
                dp[i+1][j] = max(dp[i+1][j], price + dp[i][j-w])
                cnt += 1
    return dp

items = [
    (1, 7),
    (2, 10),
    (3, 15),
    (5, 18)
]
max_weight = 8
res = dp(max_weight, items)
pprint(res)
print res[len(items)][max_weight]