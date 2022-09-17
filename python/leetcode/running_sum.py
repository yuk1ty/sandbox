from typing import List

class Solution:
    def runningSum(self, nums: List[int]) -> List[int]:
        sum = [0] * len(nums)
        sum[0] = nums[0] 
        for i in range(len(nums)):
            sum[i] = sum[i - 1] + nums[i]
        return sum
