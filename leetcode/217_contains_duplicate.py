class Solution:
    def containsDuplicate(self, nums: List[int]) -> bool:
        if len(nums) < 2:
            return False
        seen = set()
        for n in nums:
            if n in seen:
                return True
            seen.add(n)
        return False
        