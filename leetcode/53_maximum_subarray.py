class Solution:
    def maxSubArray(self, nums: List[int]) -> int:
        maxi = nums[0]
        local_max = [maxi]
        for i, n in enumerate(nums[1:]):
            tmp = max(n, n+local_max[i])
            maxi = max(tmp, maxi)
            local_max.append(tmp)
        return maxi