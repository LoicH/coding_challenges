import itertools
import math
import re
from collections import Counter, defaultdict

import aocd
import numpy as np
from tqdm import tqdm, trange
from dataclasses import dataclass
from typing import Optional, Tuple
import json
import binarytree as btree

magnitude_examples = {
    "[[1,2],[[3,4],5]]": 143,
    "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]": 1384,
    "[[[[1,1],[2,2]],[3,3]],[4,4]]": 445,
    "[[[[3,0],[5,3]],[4,4]],[5,5]]": 791,
    "[[[[5,0],[7,4]],[5,5]],[6,6]]": 1137,
    "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]": 3488,
    "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]": 4140,
}

example_input = """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"""


# @dataclass
# class Node:
#     value: Optional[int] = None
#     left: Optional = None
#     right: Optional = None

#     def __repr__(self):
#         if self.value is not None:
#             return str(self.value)
#         else:
#             l = repr(self.left)
#             r = repr(self.right)
#             return "[" + l + "," + r + "]"

#     @classmethod
#     def from_list(cls, nodes):
#         if isinstance(nodes, list) and len(nodes) == 2:
#             return Node(left=Node.from_list(nodes[0]), right=Node.from_list(nodes[1]))
#         elif isinstance(nodes, int):
#             return Node(value=nodes)

#     @classmethod
#     def from_string(cls, data: str):
#         tree_list = [
#             Node.from_list(nodes=json.loads(line)) for line in data.splitlines()
#         ]

#         tree = tree_list[0]
#         for t in tree_list[1:]:
#             tree = Node(left=tree, right=t)
#         return tree


# assert str(Node(1)) == "1"
# assert str(Node(left=Node(1), right=Node(2))) == "[1,2]"
# assert str(Node.from_list([[1, 2], [[3, 4], 5]])) == "[[1,2],[[3,4],5]]"
# assert str(Node.from_string(example_input)).startswith("[[[[[[[[[[[[0")


def read_data(s):
    return s


def add_right(tree: btree.Node, val: int) -> btree.Node:
    if tree.value != -1:
        return btree.Node(tree.value + val)
    else:
        return btree.Node(value=-1, left=tree.left, right=add_right(tree.right, val))


def tree_from_list(nodes) -> btree.Node:
    if isinstance(nodes, list) and len(nodes) == 2:
        left = tree_from_list(nodes[0])
        right = tree_from_list(nodes[1])
        node = btree.Node(value=-1)
        node.left = left
        node.right = right
    elif isinstance(nodes, int):
        node = btree.Node(value=nodes)
    return node


sample_tree = tree_from_list([[1, 2], [[3, 4], 5]])
assert add_right(sample_tree, 6).values == [
    -1,
    -1,
    -1,
    1,
    2,
    -1,
    11,
    None,
    None,
    None,
    None,
    3,
    4,
]
# "[[1,2],[[3,4],11]]"


def tree_from_string(line: str) -> btree.Node:
    tree = tree_from_list(nodes=json.loads(line))
    return tree


from_l = tree_from_list([[1, 2], [[3, 4], 5]])
from_s = tree_from_string("[[1,2],[[3,4],5]]")
assert from_l.equals(from_s)


def explode(tree):
    def _explode(tree: btree.Node, add_l=0, add_r=0, i=0) -> btree.Node:
        if tree.value != -1:
            print("Bizarre... ", tree)
            return tree
        left_val, right_val = tree.left.value, tree.right.value
        left_left_res, left_right_res = 0, 0
        right_left_res, right_right_res = 0, 0
        if i == 4 and left_val != -1 and right_val != -1:
            return btree.Node(0), left_val + add_r, right_val
        if left_val != -1:
            new_left = btree.Node(left_val + add_r)
            add_r = 0
        else:
            new_left, left_left_res, left_right_res = _explode(
                tree.left, add_r=add_r, i=i + 1
            )
            add_r = 0

        if right_val != -1:
            new_right = btree.Node(right_val + add_r + left_right_res)
            add_r = 0
        else:
            new_right, right_left_res, right_right_res = _explode(
                tree.right, add_r=left_right_res, i=i + 1
            )
            if right_left_res > 0:
                new_left = add_right(new_left, right_left_res)
        return (
            btree.Node(value=-1, left=new_left, right=new_right),
            left_left_res,
            right_right_res,
        )

    t, ll, rr = _explode(tree)
    return t


explode_examples = {
    "[[[[[9,8],1],2],3],4]": "[[[[0,9],2],3],4]",
    "[7,[6,[5,[4,[3,2]]]]]": "[7,[6,[5,[7,0]]]]",
    "[[6,[5,[4,[3,2]]]],1]": "[[6,[5,[7,0]]],3]",
    # "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]": "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
    # "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]": "[[3,[2,[8,0]]],[9,[5,[7,0]]]]",
    "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]": "[[3,[2,[8,0]]],[9,[5,[7,0]]]]",
}

for unexploded, expect_exploded in explode_examples.items():
    tree_expect_exploded = tree_from_string(expect_exploded)
    exploded = explode(tree_from_string(unexploded))
    assert exploded.equals(tree_expect_exploded), "{} != {}".format(
        exploded.values, tree_expect_exploded.values
    )

# def reduce_tree(tree):
#     def aux(tree: Node, add_left=0, add_right=0, i=0) -> Tuple[Node, int, int]:
#         if tree.value is not None:
#             return (tree, add_left, add_right)
#         if i >= 4 and tree.left.value is not None and tree.right.value is not None:
#             return (
#                 Node(value=0),
#                 tree.left.value + add_left,
#                 tree.right.value + add_right,
#             )
#         new_left, new_add_l, new_add_r = aux(tree.left, add_left, add_right, i + 1)

#     tree, _ = aux(tree)
#     return tree


def split(tree: btree.Node) -> btree.Node:
    def _split(tree: btree.Node) -> Tuple[btree.Node, bool]:
        """Return the result of one split, and if there was a split"""
        if tree.value != -1:
            if tree.value >= 10:
                half = tree.value / 2
                return (
                    btree.Node(
                        value=-1,
                        left=btree.Node(math.floor(half)),
                        right=btree.Node(math.ceil(half)),
                    ),
                    False,
                )
            else:
                return tree, True
        new_left, did_not_split = _split(tree.left)
        if not did_not_split:
            return btree.Node(value=-1, left=new_left, right=tree.right), did_not_split
        else:
            new_right, did_not_split = _split(tree.right)
            return btree.Node(value=-1, left=new_left, right=new_right), did_not_split

    tree, did_not_split = _split(tree)
    return tree


split_examples = {
    "[[[[0,7],4],[15,[0,13]]],[1,1]]": "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]",
    "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]": "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]",
}

for before_split, after_split in split_examples.items():
    split_result = split(tree_from_string(before_split))
    expected_tree = tree_from_string(after_split)
    assert split_result.equals(expected_tree), "{} != {}".format(
        split_result.values, expected_tree.values
    )


def step_reduce(tree: btree.Node) -> btree.Node:
    """since "explode" is done before "split",
    we explode the tree as much as we can and then perform one split"""
    new_tree = explode(tree)
    while not new_tree.equals(tree):
        tree = new_tree
        new_tree = explode(tree)
    # Then when the "explosions" are done, we split the tree once
    new_tree = split(tree)
    return new_tree


def reduce_tree(tree: btree.Node) -> btree.Node:
    new_tree = step_reduce(tree)
    while not new_tree.equals(tree):
        tree = new_tree
        new_tree = step_reduce(tree)
    return new_tree


def magnitude(tree):
    if tree.value != -1:
        return tree.value
    else:
        return 3 * magnitude(tree.left) + 2 * magnitude(tree.right)


# Test the magnitude examples
for string, expect_mag in magnitude_examples.items():
    compute_mag = magnitude(tree_from_string(string))
    assert compute_mag == expect_mag, "{} is different from expected {}".format(
        compute_mag, expect_mag
    )


def reduced_tree_from_str(data: str) -> btree.Node:
    tree = None
    for line in tqdm(data.splitlines()):
        new_tree = tree_from_string(line)
        if tree is None:
            tree = new_tree
        else:
            tree = btree.Node(value=-1, left=tree, right=new_tree)
            tree = reduce_tree(tree)
    return tree


reduce_examples = {
    """[1,1]
[2,2]
[3,3]
[4,4]""": "[[[[1,1],[2,2]],[3,3]],[4,4]]",
    """[1,1]
[2,2]
[3,3]
[4,4]
[5,5]""": "[[[[3,0],[5,3]],[4,4]],[5,5]]",
    """[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]""": "[[[[5,0],[7,4]],[5,5]],[6,6]]",
    """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]""": "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]",
}

print("Testing reduce examples...")
for s, expect_reduced in reduce_examples.items():
    reduced = reduced_tree_from_str(s)
    expected_tree = tree_from_string(expect_reduced)
    assert reduced.equals(expected_tree), "{} != {}".format(
        reduced.values, expected_tree.values
    )
print("Tests passed!")

# Part 1
def part1(data: str):
    return magnitude(reduced_tree_from_str(data))


example_data = read_data(example_input)
assert part1(example_data) == 4140

puzzle_input = aocd.get_data(day=18)

puzzle_data = read_data(puzzle_input)
# aocd.submit(part1(puzzle_data))

# Part 2
def part2(data):
    max_magnitude = -1
    combinations = list(itertools.product(data.splitlines(), repeat=2))
    for line1, line2 in tqdm(combinations):
        tree1 = tree_from_string(line1)
        tree2 = tree_from_string(line2)
        reduced = reduce_tree(btree.Node(value=-1, left=tree1, right=tree2))
        mag = magnitude(reduced)
        max_magnitude = max(max_magnitude, mag)
    return max_magnitude


assert part2(example_data) == 3993

print(part2(puzzle_data))
