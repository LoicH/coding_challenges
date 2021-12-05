
class Solution:
    def threeSum(self, nums: List[int]) -> List[List[int]]:
        n_vus = set()
        nums.sort()
        solutions = []
        for i, a in enumerate(nums[:-2]):
            # print("{}/{}".format(i, len(nums)))
            # On fixe a, donc on se retrouve avec le 2sum problem
            if a in n_vus:
                # Si on a déjà résolu le 2sum pour ce nombre on avance
                continue
            n_vus.add(a)
            gauche = i+1
            droite = len(nums) - 1
            cible = 0 - a
            while gauche < droite:
                # print(gauche, droite)
                if nums[gauche] + nums[droite] == cible:
                    solutions.append([a, nums[gauche], nums[droite]])
                    gauche += 1
                    while gauche < droite and nums[gauche-1] == nums[gauche]:
                        gauche += 1
                    droite -= 1
                    while gauche < droite and nums[droite+1] == nums[droite]:
                        droite -= 1
                    
                elif nums[gauche] + nums[droite] < cible:
                    gauche += 1
                else: 
                    droite -= 1
        return solutions