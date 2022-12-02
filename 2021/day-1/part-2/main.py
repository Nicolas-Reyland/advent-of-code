from __future__ import annotations
import sys

def solve(l: list[int]) -> int:
    N = 0
    for index in range(len(l) - 3):
        if l[index] < l[index + 3]:
            N += 1
    return N

if __name__ == "__main__":
    lines = filter(None, sys.stdin.read().split('\n'))
    nums = list(map(int, lines))
    result = solve(nums)
    print(result)
