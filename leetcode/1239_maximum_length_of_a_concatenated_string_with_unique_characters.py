class Solution:
    def maxLength(self, arr) -> int:
        nb_elts = len(arr)
        mem = [None] * (2**nb_elts)
        maxi = 0
        alphabet = string.ascii_lowercase
        # Création des masques
        for i, s in enumerate(arr):
            masque = [sum(1 for c in s if c==a) for a in alphabet]
            try:
                mem[2**i] = int("".join(str(c) for c in masque), 2)
                l = len([1 for n in masque if n])
                if l > maxi:
                    maxi = l
            except ValueError:
                mem[2**i] = None
        # Programmation dynamique
        for i in range(1, 2**nb_elts):
            if mem[i] is not None:
                continue
            k = i.bit_length()-1
            a = 2**k
            b = i-a
            if mem[b] and mem[a] and mem[a] & mem[b] == 0:
                mem[i] = mem[a] + mem[b]
                nb_chars = sum(1 for c in bin(mem[i]) if c=='1')
                if nb_chars > maxi:
                    maxi = nb_chars
        return maxi

