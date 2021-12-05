class Solution:
    def twoSum(self, nums, target):
        for i, a in enumerate(nums):
            for j, b in enumerate(nums[i+1:]):
                if a+b == target:
                    return [i, i+j+1]