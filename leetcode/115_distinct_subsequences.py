class Solution:
    def numDistinct(self, s: str, t: str) -> int:
        dp = [[1] * (1+len(s))]
        for c in t:
            dp.append([0]*(1+len(s)))
        
        for i in range(0, len(t)):
            for j in range(0, len(s)):
                if t[i] == s[j]:
                    dp[i+1][j+1] = dp[i][j] + dp[i+1][j]
                else:
                    dp[i+1][j+1] = dp[i+1][j]
        return dp[len(t)][len(s)]