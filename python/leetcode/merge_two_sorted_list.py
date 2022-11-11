from typing import List, Optional

# Definition for singly-linked list.
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

# 最初に思いついた解法。一度リストに直して2つのリストを結合→Python の標準関数でソート。
# その後、LinkedList に組み直して完成。
# LinkedList 内の in-place でやれると速そうだなと思いつつ、実装を思いつかなかった。

class Solution:
    def mergeTwoLists(self, list1: Optional[ListNode], list2: Optional[ListNode]) -> Optional[ListNode]:
        whole_list = []
        self.append_list(list1, whole_list)
        self.append_list(list2, whole_list)
        return self.to_linked_list(sorted(whole_list))
        
    
    def append_list(self, source: Optional[ListNode], list: List[int]):
        if source == None:
            return None
        
        list.append(source.val)
        
        if source.next == None:
            return
        
        self.append_list(source.next, list)
        
    
    def to_linked_list(self, list: List[int]) -> Optional[ListNode]:
        if len(list) == 0:
            return None
        elif len(list) == 1:
            return ListNode(list[0], None)
        else:
            return ListNode(list[0], self.to_linked_list(list[1:]))
