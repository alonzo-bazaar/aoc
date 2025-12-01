from typing import Tuple
from sys import argv

node_registry = {}

class Node:
    def __init__(self, key:str, left:str, right:str):
        self.key = key
        self.right = right
        self.left = left
        node_registry[key] = self

    def get_left(self):
        return node_registry[self.left]

    def get_right(self):
        return node_registry[self.right]

def parse_row(row:str) -> Node:
    return Node(row[0:3], row[7:10], row[12:15])

def follow_insctructions(n:Node, inst:str) -> Tuple[Node, int]:
    steps = 0
    for c in inst:
        if c == 'L':
            steps += 1
            n = n.get_left()
        elif c == 'R':
            steps += 1
            n = n.get_right()

        if n.key=='ZZZ':
            return n, steps

    return n, steps

def follow_ghost_instructions(nodes:list[Node], inst:str) -> Tuple[list[Node], int]:
    steps = 0
    for c in inst:
        if c == 'L':
            steps += 1
            nodes = [n.get_left() for n in nodes]
        elif c == 'R':
            steps += 1
            nodes = [n.get_right() for n in nodes]

        if all_z(nodes):
            return nodes, steps

    return nodes, steps

def number_of_steps(n:Node, inst:str) -> int:
    total_steps = 0
    while n.key != 'ZZZ':
        new_n, steps = follow_insctructions(n, inst)
        n = new_n
        total_steps += steps
        print("after ", steps, " steps, now totaling ", total_steps, " we are at node ", n.key)

    return total_steps

def number_of_ghost_steps(nodes:list[Node], inst:str) -> int:
    total_steps = 0
    while not all_z(nodes):
        new_nodes, steps = follow_ghost_instructions(nodes, inst)
        nodes = new_nodes
        total_steps += steps
        # print("after ", steps, " ghost steps, now totaling ", total_steps, " we are at nodes ", [n.key for n in nodes[0:3]])

    return total_steps

def all_z(nodes:list[Node]):
    for node in nodes:
        if node.key[2] != 'Z':
            return False
    return True

def parse_file(name:str) -> Tuple[Node, str]:
    """
    returns instructions string, together with the AAA node we're starting from
    """
    with open(name) as fileobj:
        inst = fileobj.readline()
        fileobj.readline()
        for line in fileobj:
            parse_row(line)

    return node_registry['AAA'], inst

def parse_ghost_file(name:str) -> Tuple[list[Node], str]:
    """
    returns instructions string, together with all the nodes that end in A
    """
    with open(name) as fileobj:
        inst = fileobj.readline()
        fileobj.readline()
        for line in fileobj:
            parse_row(line)

    return [n for n in node_registry.values() if n.key[2] == 'A'], inst
            
if __name__=="__main__":
    a_nodes, inst = parse_ghost_file(argv[1])
    res = number_of_ghost_steps(a_nodes, inst)
    print(res)
    exit(0)
