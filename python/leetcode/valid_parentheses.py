# (, {, [ のどれかだった場合（"開始"とする）、スタックに積んでおく。
# "開始"でなかった場合、スタックに積んだものを順に上からpopしていき、一致していなかったらFalseを返す。

class Solution:
    def isValid(self, s: str) -> bool:
        stack = []
        
        for char in s:
            if char in ["(", "{", "["]:
                stack.append(char)
            else:
                if not stack:
                    return False
                
                curr = stack.pop()
                
                if curr == "(":
                    if char != ")":
                        return False
                if curr == "{":
                    if char != "}":
                        return False
                if curr == "[":
                    if char != "]":
                        return False
                
        if stack:
            return False
        
        return True
